suppressPackageStartupMessages(
   {library(data.table)})

repeated.idw.tables = function(
        locations,
          # A matrix or data frame of points.
        is.source = T,
          # A logical vector of locations that can ever have
          # source data.
        maxdist = NULL,
          # Only consider source points closer than this distance to
          # each query point.
        multiplier = 1,
          # Multiply all squared distances by this number to help avoid
          # floating-point problems due to very small or large weights.
        source.subsetter = function(i) T)
          # A function that should take an index of `locations` and
          # return a logical vector indicating which elements of
          # `locations[is.source]` to use for the given query point.
          # Use `source.subsetter` to hold out source points for
          # cross-validation.
   {locations = as.matrix(locations)
    source = t(locations[is.source,])

    # Set `si` to a vector with as many elements as there are locations.
    # Each element is NA when `is.source` is false, or the corresponding
    # index of `source` when `is.source` is true.
    if (identical(is.source, T))
        si = 1 : nrow(locations)
    else
       {si = ifelse(is.source, 1, NA_real_)
        si[!is.na(si)] = 1 : sum(!is.na(si))}

    minweight = if (is.null(maxdist)) 0 else
        1/(maxdist^2 * multiplier)
    tables = lapply(1 : nrow(locations), function(i)
       {weights = 1 / (colSums((source - locations[i,])^2) * multiplier)
        w = which(weights > minweight & source.subsetter(i))
        cbind(w, weights[w], deparse.level = 0)})
    stopifnot(all(sapply(tables, nrow)))
    attr(tables, "si") = si
    attr(tables, "ncol(source)") = ncol(source)
    tables}

repeated.idw = function(tables, li, group, outcome,
        make.prediction = T, fallback = NA_real_, progress = F)
   {d = data.table(li, group, outcome, make.prediction)
    si = attr(tables, "si")
    if (progress)
        bar = txtProgressBar(min = 0, max = length(unique(group)), style = 3)
    d[, by = group, prediction :=
       {if (progress)
            setTxtProgressBar(bar, .GRP)
        s = rep(NA_real_, attr(tables, "ncol(source)"))
        lisi = attr(tables, "si")[li]
        s[lisi[!is.na(lisi)]] = outcome[!is.na(lisi)]
        preds = rep(NA_real_, .N)
        preds[make.prediction] = sapply(li[make.prediction], function(i)
           {tab = tables[[i]]
            v = s[tab[, 1]]
            weights = tab[!is.na(v), 2]
            if (length(weights))
                sum(v[!is.na(v)] * weights) / sum(weights)
            else
                fallback})
        preds}]
    if (progress)
        close(bar)
    d$prediction}
