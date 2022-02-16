# Fit XGBoost with DART to a dataset, using cross-validation to
# select hyperparameters.
#
# n.rounds (the number of trees) is set manually rather than by
# cross-validation because there can be a range of its values where
# increasing it seems to slow down XGBoost a lot while only
# improving performance a little bit.

#' @export
#' @import data.table
xgboost.dart.cvtune = function(
        d, dv, ivs,
        n.rounds,
        weight.v = NULL,
        objective = "reg:squarederror",
        eval_metric = "rmse",
        n.param.vectors = 50L,
        n.folds = 2L,  # Ignored if `folds` is set.
        folds = NULL,
        progress = F,
        ...){
    xgboost.extra = list(...)

    assert(objective %in% c("reg:squarederror", "logcosh"))
    assert(eval_metric %in% c("rmse", "mae"))

    if (objective == "logcosh")
       objective = logcosh.objective

    eval_metric_f = function(x, y, weights = NULL) switch(eval_metric,
        rmse = sqrt(
            if (is.null(weights))
                mean((x - y)^2)
            else
                sum((x - y)^2 * weights) / sum(weights)),
        mae =
            if (is.null(weights))
                mean(abs(x - y))
            else
                sum(abs(x - y) * weights) / sum(weights))

    design = hyperparam.set(n.param.vectors)

    fit = function(dslice, params, fast = F)
        xgboost::xgboost(
            verbose = 0,
            params = c(
                list(
                    booster = "dart",
                    objective = objective,
                    eval_metric = eval_metric,
                    one_drop = T),
                xgboost.extra,
                params),
            data = as.matrix(dslice[, mget(ivs)]),
            label = dslice[[dv]],
            weight = (if (is.null(weight.v)) NULL else
                dslice[[weight.v]]),
            nrounds = n.rounds)

    if (is.null(folds))
        folds = sample(rep(1 : n.folds, len = nrow(d)))
    if (progress)
       {bar.steps = nrow(design) * length(unique(folds)) + 1
        bar = txtProgressBar(min = 0, max = bar.steps, style = 3)}

    step = 0
    best.design.i = which.min(lapply(1 : nrow(design), function(design.i)
       {preds = data.table(y = rep(NA_real_, nrow(d)))
        for (fold.i in sort(unique(folds)))
           {m = fit(d[folds != fold.i], design[design.i], fast = T)
            preds[
                folds == fold.i,
                y := predict(m,
                    newdata = as.matrix(d[folds == fold.i, mget(ivs)]))]
            step <<- step + 1
            if (progress)
                setTxtProgressBar(bar, step)}
        eval_metric_f(preds$y, d[[dv]],
            (if (is.null(weight.v)) NULL else d[[weight.v]]))}))
    if (length(best.design.i) == 0)
        stop('No best design, probably NA values in data.')
    m = fit(d, design[best.design.i])
    if (progress)
       {setTxtProgressBar(bar, bar.steps)
        close(bar)}
    list(model = m, pred.fun = function(newdata, ...)
        predict(m,
            newdata = as.matrix(newdata[, mget(ivs)]),
            ...))}

#' @export
#' @import ParamHelpers
hyperparam.set = function(n.param.vectors){
    pow2 = function(x) 2^x
    ps = makeParamSet(
        # Based on autoxgboost's defaults.
        makeNumericParam("eta", lower = .01, upper = .5),
        makeNumericParam("gamma", lower = -7, upper = 6, trafo = pow2),
        makeNumericParam("lambda", lower = -10, upper = 10, trafo = pow2),
        makeNumericParam("alpha", lower = -10, upper = 10, trafo = pow2),
        makeDiscreteParam("max_depth", values = c(3, 6, 9)),
        makeDiscreteParam("rate_drop", values = c(0, .01, .025, .05)))
    design = as.data.table(
        Just.universal::with.temp.seed(as.integer(n.param.vectors), generateDesign(
            n.param.vectors, par.set = ps, trafo = T,
            fun = lhs::maximinLHS)))
    for (dcol in colnames(design))
      {# Reverse the factorization of discrete parameters.
       if (is.factor(design[[dcol]]))
           design[, (dcol) := as.numeric(as.character(get(dcol)))]
       # Round off the selected parameters to a few significant
       # figures.
       if (is.numeric(design[[dcol]]))
           design[, (dcol) := signif(get(dcol), 2)]}
    unique(design)}

logcosh.objective = function(preds, dtrain)
  # log(cosh(residuals)) is an everywhere twice-differentiable
  # loss function that approximates abs(residuals).
   {e = preds - getinfo(dtrain, "label")
    # The derivative of log(cosh(x)) is tanh(x), and the
    # derivative of tanh(x) is sech(x)^2 = 1/cosh(x)^2.
    list(grad = tanh(e), hess = 1/cosh(e)^2)}

#' @import data.table
xgboost.dart.cvtune.example = function(absolute = F, weighted = F)
   {xgb.threads = 10

    set.seed(5)
    N = 2000
    d = transform(data.table(
        x1 = rnorm(N),
        x2 = rnorm(N),
        x3 = rnorm(N)),
        y = 2*x2 + (abs(x3) < 1) + rnorm(N),
        weight = ifelse(x3 < 0, 100, 1))
    train = (1 : N) <= 1000

    fit = xgboost.dart.cvtune(
        d = d[train],
        dv = "y", ivs = c("x1", "x2", "x3"),
        weight.v = (if (weighted) "weight" else NULL),
        n.rounds = 10,
        objective = (if (absolute) "logcosh" else "reg:squarederror"),
        eval_metric = (if (absolute) "mae" else "rmse"),
        progress = T,
        nthread = xgb.threads)

    rmse = function(x, y)
        sqrt(mean((x - y)^2))
    evaluate = function(dslice)
        round(d = 3, rmse(
            fit$pred.fun(dslice),
            dslice$y))

    message("Training RMSE: ", evaluate(d[train]))
    message("Test RMSE: ", evaluate(d[!train]))

    if (weighted)
       {message("Test RMSE on high-weighted: ", evaluate(d[!train & weight == 100]))
        message("Test RMSE on low-weighted: ", evaluate(d[!train & weight == 1]))}

    invisible(fit)}
