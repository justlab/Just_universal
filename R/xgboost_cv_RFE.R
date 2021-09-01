#
# Selected functions from CWV paper to run cv with RFE. Some functions have been
# further improved compared to the CWV paper
#
# Auther info. into the Description: person(given = "Yang", family = "Liu", role = c("aut"), email = "lyhello@gmail.com", comment = c(ORCID = "0000-0001-6557-6439"))
#
# 



# Version 3 CV with RFE -------------------------------------------------------------
# 19.4, a better version that seperates the rfe process
# 19.7, use random search by Kodi::xgboost.dart.cvtune

#' run.k.fold.cv.rfe.wrap: wrap cv (the `run.k.fold.cv` function) with rfe.
#'
#' Version 3 CV with RFE (recursive feature selection using SHAP),
#' use random search by `xgboost.dart.cvtune`.
#'
#'
#' @importFrom stats as.formula cor lm na.omit predict var
#' @import xgboost
#' @import data.table
#' @import here
#'
#'
#' @param data the dataset
#' @param sat Name of the satellite, just for labelling purpose, for example "terra", or just "".
#' @param y_var the y variables, for example y_var = "AOD_diff"
#' @param features0 the features to use, can incl. y_var or excl. y_var
#' @param k_fold default to 5 k_fold cross-validation
#' @param n_rounds default to 100, could set to lower for fast testing
#' @param stn_var the variable presenting stations if cv by stn
#' @param day_var the variable presenting dayint if cv by day,
#' if provide both, will run cv by station. day_var should be date, or int.
#' @param run_param_cv run parameter search or not, default to TRUE,
#' but set to FALSE during the RFE process, could be changed to TRUE in the
#' code to allow parameter tuning during RFE process. May not be necessary.
#' @param run_rfe default to TRUE, run RFE
#' @param predict_whole_model default to TRUE, fit the whole model in the end
#' using selected features from RFE
#'
#' @export run.k.fold.cv.rfe.wrap
#' @return a list of object:
#' * sat and by: record of satellite and cv by what
#' * time_spent: time spent to run the cv
#' * bin_list: how was the bins divided
#' * modeldt1_wPred: dt of the data with predicted values from cv
#' * rmse_all_folds: overall rmse from cv
#' * features_rank_rfe: the features ranked by RFE
#' * features_rank_rfe_record: what were selected during each round of RFE
#' * shap_score: the shap values dataset from cv
#' * xgb_param_list1: each folder's hyperparameters in list
#' * xgb_param_list2 = unlist(xgb_param_list1)
#' * rmse_rfe: rmse from each round of RFE, used to select features
#' * var_selected_rfe: features selected using RFE, used to fit the whole model
#' * xgb_param_dart_whole_model
#' * features_rank_full_model: SHAP rank of features if just run the whole model
#' not based on RFE
#'
run.k.fold.cv.rfe.wrap <- function(
  data,
  sat,
  y_var,
  features0,
  k_fold = 5,
  n_rounds = 100,
  stn_var = NULL,
  day_var = NULL,
  run_param_cv = T,
  progress = T,
  run_rfe = T, # optional to run rfe
  predict_whole_model = T # optional to predict using whole dataset
)
{
  xgb_threads <- get.threads()
  
  modeldt1 <- data.table::copy(setDT(data))
  if(!is.null(day_var)) {
    modeldt1[,dayint := as.integer(get(day_var))]
    by = "day"
  }
  if(!is.null(stn_var)) {
    modeldt1[,stn := get(stn_var)]
    by = "stn"
  }
  if (!is.null(day_var) & !is.null(stn_var))
    stop("Please provide either day_var or stn_var.")
  # some internal variables names
  y_formula <- as.formula(paste(y_var, "~."))
  y_var_pred <- paste0(y_var, "_pred") # name of the predicted y
  y_var_pred_whole <- paste0(y_var, "_pred_whole") # name of the predicted y
  
  # divide data by folders and get list of index
  time0 <- Sys.time()
  temp <- prepare.bin(modeldt1, by = by, k_fold = k_fold)
  modeldt1 <- temp$data
  bin_list <- temp$bin # k_fold of stations
  index.fs.list <-  list(
    # return the observations in each fold
    stn = function(x, bin_list)modeldt1[stn%in%bin_list[[x]], which = TRUE],
    day = function(x, bin_list)modeldt1[dayint%in%bin_list[[x]], which = TRUE]
  )
  
  index_train <- index_test <- list()
  for (k in 1:k_fold){
    index_test[[k]] <- index.fs.list[[by]](k, bin_list)
    index_train[[k]] <- -index_test[[k]]
  }
  
  # run k-fold cv and record SHAP matrix, predicted value, overall rmse...
  if (!y_var%in%features0) features0 <- c(features0, y_var) 
  dataXY0 <- modeldt1[,..features0]
  message("Run k-fold cv \n")
  cv_results <- run.k.fold.cv(sat = sat, k_fold=k_fold, run_param_cv = run_param_cv,
                              dataXY_df = dataXY0, by = by, n_rounds = n_rounds, 
                              y_var = y_var, progress = progress,
                              index_train = index_train, index_test = index_test,
                              xgb_threads = xgb_threads)
  # write param list to global environment
  # so in rfe process there is no need to run param search
  xgb_param_list_full <- cv_results$xgb_param_list2
  # save to drive instead of using <<- to send to global. Not recommended in funcs.
  if (!dir.exists(here("Intermediate"))) dir.create(here("Intermediate"))
  saveRDS(xgb_param_list_full, here("Intermediate", "xgb_param_list_full.rds"))
  cv_results_full_model <- cv_results
  modeldt1 <-  cbind(modeldt1, cv_results$y_pred_dt)
  
  ## RFE part
  if (run_rfe){
    message("Run RFE \n")
    n_features <- length(features0) - 1
    features_rank_rfe <- rep(NA_character_, n_features)
    features_rank_rfe_record <- list()
    rmse_rfe <- rep(NA_real_, n_features)
    
    for (nf in (n_features):1){
      features_rank_rfe_record[[nf]] <- cv_results$features_rank_full_model # save all the features
      cat("nf = ",nf, 'features ranked for',sat, 'are\n', features_rank_rfe_record[[nf]], '\n')
      features_rank_rfe[nf] <- cv_results$features_rank_full_model[nf] # save the last feature
      features_updated <- cv_results$features_rank_full_model[-nf] # remove the last and refit to model
      rmse_rfe[nf] <- cv_results$rmse_all_folds
      features_updated_Y <- c(features_updated, y_var) # ...just to incl. Y
      dataXY_updated <- dataXY0[,..features_updated_Y] # dataset with updated X and Y
      if (nf > 1){
        cv_results <- run.k.fold.cv(sat = sat, k_fold=k_fold,
                                    run_param_cv = F,  # ! #
                                    dataXY_df = dataXY_updated, #!#
                                    by = by, y_var = y_var,
                                    n_rounds = n_rounds,
                                    index_train = index_train,
                                    index_test = index_test,
                                    progress = progress,
                                    xgb_threads = xgb_threads)
      }
    }
    rfe_output <- list(features_rank_rfe = features_rank_rfe,
                       features_rank_rfe_record = features_rank_rfe_record,
                       rmse_rfe = rmse_rfe)
    
    ## predict whole data
    if (predict_whole_model){
      # select by number of features from rfe
      var_selected <- features_rank_rfe[1: which.min(rmse_rfe)]
      rsxgb_whole <- xgboost.dart.cvtune(
        n.rounds = n_rounds,
        d = modeldt1, dv = y_var, ivs = var_selected,
        progress = progress, nthread = xgb_threads)
      xgb_param_dart <- rsxgb_whole$model$params
      xgbmod <- rsxgb_whole$model
      modeldt1[[y_var_pred_whole]] <-  rsxgb_whole$pred.fun(modeldt1)
      rfe_output <- list(features_rank_rfe = features_rank_rfe,
                         features_rank_rfe_record = features_rank_rfe_record,
                         rmse_rfe = rmse_rfe,
                         var_selected_rfe = var_selected,
                         xgb_param_dart_whole_model = xgb_param_dart)
    }
  }
  time_spent <- Sys.time() - time0
  
  # output
  general_output <- list(sat = sat, by=by, time_spent = time_spent,
                         bin_list = bin_list, modeldt1_wPred = modeldt1)
  
  if (run_rfe) {
    return(c(general_output, cv_results_full_model, rfe_output))
  } else {
    return(c(general_output, cv_results_full_model))}
}


#' core cv function what uses \code{xgboost.dart.cvtune} to run cv for one fold.
#'
#' internal function.
#' @inheritParams run.k.fold.cv.rfe.wrap
#' @param dataXY_df dataset with only X and Y
#' @param index_train index for training
#' @param index_test index for testing
#' @param xgb_threads xgb_threads passed from parent function
#' @param by sting of "stn" or "day" passed from parent function 
#' @param progress whether to show progress bars, default TRUE
#' @param ... other arguments
#'
run.k.fold.cv <- function(sat, k_fold, run_param_cv, dataXY_df, y_var,
                          index_train, index_test, xgb_threads, by, n_rounds, 
                          progress = TRUE, ...){
  y_var_pred <- paste0(y_var, "_pred") # name of the predicted y
  Y <-  dataXY_df[,..y_var]
  data_X <- dataXY_df[, -..y_var]
  # prepare dataset
  n_row <- nrow(data_X)
  n_col <- ncol(data_X)
  
  
  # output dataset
  y_pred_dt <- data.table(y_pred = rep(NA_real_, n_row))
  shap_score <- as.data.table(matrix(rep(NA_real_,n_row*n_col),ncol = n_col))
  names(shap_score) <- names(data_X)
  xgb_param_list1 <- xgb_param_list2 <- list()
  BIAS0 <- rep(NA_real_, k_fold)
  # loop for each fold
  
  set.seed(1234)
  for (i in 1:k_fold){
    cat('number of obs in testing fold', i, 'is:', nrow(data_X[index_test[[i]],]), '\n')
    # change to use random search
    if (run_param_cv){
      rsxgb0 <- xgboost.dart.cvtune(
        # by default, gives 100 rounds, and it is enough by experience
        n.rounds = n_rounds,
        d = dataXY_df[index_train[[i]],], dv = y_var, ivs = colnames(data_X),
        progress = progress, nthread = xgb_threads)
      # manually select and store some params
      xgb_param_dart <- c(rsxgb0$model$params[c(1,2,4, 6:11)], nrounds = rsxgb0$model$niter)
      xgbmod <- rsxgb0$model
      #
      # autoincv1 <- run.autoxgboost.dart(traindt = dataXY_df[index_train[[i]],], target = y_var, simple = T)
      # xgb_param_dart <- mlr::getHyperPars(autoincv1$final.learner)
      
      # fit model
      y_pred_dt[index_test[[i]], y_pred:= rsxgb0$pred.fun(dataXY_df[index_test[[i]],])] # record the predicted cwv_diff
      # predicted SHAP
      shap_pred <- as.data.table(rsxgb0$pred.fun(dataXY_df[index_test[[i]],],
                                                 predcontrib = TRUE, approxcontrib = FALSE))
    } else {
      # use already obtained param to fit the model (old way)
      xgb_param_list_full <- readRDS(here("Intermediate", "xgb_param_list_full.rds"))
      xgb_param_dart <- xgb_param_list_full[[paste0(by, i)]]
      train_mm <- as.matrix(data_X[index_train[[i]],])
      test_mm <- as.matrix(data_X[index_test[[i]],]) # features do not contain Y variable
      xgbmod <- rfe.fit(X = train_mm, Y = as.matrix(Y[index_train[[i]],..y_var]), 
                        xgb_param = xgb_param_dart)
      # predicted y
      y_pred_dt[index_test[[i]], y_pred:= predict(xgbmod, test_mm)]
      # predicted SHAP
      shap_pred <- as.data.table(predict(xgbmod, test_mm ,predcontrib = TRUE, approxcontrib = FALSE))
    }
    
    BIAS0[i] <- first(shap_pred$BIAS)
    shap_pred[, BIAS := NULL]
    shap_score[index_test[[i]],] <- shap_pred # record the SHAP values (for whole model)
    
    xgb_param_list1[[paste0(by, i)]] <- unlist(xgb_param_dart)
    xgb_param_list2[[paste0(by, i)]] <- xgb_param_dart
  }
  cat("loop finished\n")
  rmse_all_folds <- sqrt(mean((y_pred_dt$y_pred - dataXY_df[[y_var]])^2))
  # features ranked by SHAP
  features_rank_full_model <- names(colMeans(abs(shap_score))[order(colMeans(abs(shap_score)),
                                                                    decreasing = T)])
  setnames(y_pred_dt, "y_pred", y_var_pred)
  return(list(rmse_all_folds = rmse_all_folds,
              features_rank_full_model = features_rank_full_model,
              shap_score = shap_score,
              y_pred_dt = y_pred_dt,
              BIAS = BIAS0,
              xgb_param_list1 = xgb_param_list1,
              xgb_param_list2 = xgb_param_list2))
}


#' internal function to get threads to use
#'
get.threads <- function(){
  future::availableCores()/2
}

#' A wrapped function to run xgboost model.
#'
#' @param X predictors, Notice that **should NOT contains Y**.
#' @param Y dependent variable Y.
#' @param xgb_param a list of hyperparameters selected, contains nrounds
#' @importFrom  xgboost xgboost
#' @return xgboost model object
#'
rfe.fit <- function(X, Y, xgb_param){
  if (!is.null(xgb_param$seed)) set.seed(xgb_param$seed) else set.seed(1234)
  xgb_threads <- get.threads()
  # message(paste("xgb_param is", unlist(xgb_param)))
  xgbmod <- xgboost::xgboost(data = as.matrix(X),
                    label = as.matrix(Y),
                    params = xgb_param,
                    nrounds = xgb_param$nrounds,
                    verbose = FALSE,
                    nthread = xgb_threads,
                    early_stopping_rounds = 8)
  return(xgbmod)
}

#' prepare the folder by day or by station.
#'
#' Internal function for cv
#'
#' @param modeldt1 dataset
#' @param by must be either stn or day
#' @param k_fold k fold
#' @return list of a dataset with group assignment and a `bin_list` showing what are in each bin
#'
prepare.bin <- function(modeldt1, by, k_fold){
  # used for 5 fold
  if (by == 'stn') by_var = 'stn'
  else if (by == 'day')  by_var = 'dayint'
  else stop('choose between by = stn or day')
  ss <- unique(modeldt1[, .(group_count = .N), by = by_var])
  ss <- ss[order(-group_count),]
  ##
  ## use helper.pack.bins is better
  ss$bin_stn <- helper.pack.bins(ss[, group_count], k_fold)
  bin_list <- split(ss[[by_var]], ss$bin_stn)
  
  setkeyv(modeldt1, by_var)
  setkeyv(ss, by_var)
  modeldt1 <- modeldt1[ss, on = by_var]
  modeldt1$bin_stn <- as.factor(modeldt1$bin_stn)
  
  message('Obs in each stn_bin and total obs:')
  print(ss[, sum(group_count), by = bin_stn])
  
  return(list(data = modeldt1, bin = bin_list))
}

# Help func. --------------------------------------------------------------
#' assign groups numbers to each station by their number of observations.
#'
#' Internal function for cv
#'
#' @param object.sizes a vector of object size
#' @param n.bins number of bins (e.g. 5 folders )
#' @import data.table
#' @export helper.pack.bins
#' @return a vector of group assignment
#' @examples helper.pack.bins(seq(1:10), n.bins = 4)
helper.pack.bins = function(object.sizes, n.bins){
  # by Kodi
  # Greedily pack objects with sizes given by `object.sizes`
  # into `n.bins` bins. Return a vector of the bin indices.
  d = data.table::data.table(size = as.integer(object.sizes),
                             bin = NA_integer_)
  d[, oi := .I]
  # Shuffle the input so ties on object size are broken randomly.
  d = d[sample.int(nrow(d))]
  bin.sizes = rep(0L, n.bins)
  while (anyNA(d$bin))
  {the.oi = d[is.na(bin)][which.max(size), oi]
  bsi = which.min(bin.sizes)
  d[oi == the.oi, bin := bsi]
  bin.sizes[bsi] = bin.sizes[bsi] + d[oi == the.oi, size]}
  d[order(oi), bin]
}


# Some corresponding functions to generate output -------------------------

#' report R squared
#' @param dataXY the dataset, modeldt1_wPred from the model above
#' @param nround round the results
#'
#' @export report.r.squared
#'
report.r.squared <- function(dataXY, nround = 2){
  SSE0 <- sum((dataXY[[y_var]] - dataXY[[y_var_pred]])^2)
  R2 <- 1- SSE0/(var(dataXY[[y_var]])*(dataXY[,.N]-1))
  rmse <-  sqrt(mean((dataXY[[y_var_pred]] - dataXY[[y_var]])^2))
  rmse_pcnt <- rmse / sqrt(mean((dataXY[[y_var]])^2))
  s <-  paste('N =', dataXY[,.N],', RMSE reduced from',
            round(sqrt(mean((dataXY[[y_var]])^2)),4),
            'to', round(rmse,4), "(", round(rmse_pcnt*100, nround), "%),", "\n",
            
            'Traditional R2: 1-SSE/(SST(n-1)) =', round(R2*100,nround),'%;', "\n",
            
            'Pearson R2 between y_var_pred and y_var is',
            round(cor(dataXY[[y_var]], dataXY[[y_var_pred]])^2*100, nround),
            '%;', "\n",
            
            "Pearson r = ",round(cor(dataXY[[y_var]], dataXY[[y_var_pred]]),
                                 nround+2), '.')
  print(s)
  return(s)
}


#' Show RFE results: make plots of rmse for new RFE.
#' updated on (19.04.06). only one line is plotted.
#' @param rmse_rfe use rferesults$rmse_rfe
#' @import ggplot2
#' 
#' @return ggplot2 object
#' @export rfe.rmse.plot
rfe.rmse.plot <- function(rmse_rfe = rferesults$rmse_rfe){
  rmse_rfe_dt <- data.table(x = 1:length(rmse_rfe),
                            y = rmse_rfe)
  n_min <- which.min(rmse_rfe)
  ggplot2::ggplot(data = rmse_rfe_dt, aes(x=x, y=y)) +
    geom_point() +
    geom_line(size = 0.5) +
    labs(x = "Number of Features",
         y = "Testing rmse from all folds of CV", color = "", group = "") +
    geom_vline(xintercept = n_min, size = 0.3, linetype = 2) +
    theme_bw()
}



if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c(".","oi","bin","size","aeronetdt",
                           "sat","long","group","group_count",
                           "bin_stn","y_var","y_var_pred",
                           "xgb_threads", "rferesults", "x", "y","..yvar",
                            "y_pred",
                           "xgb_param_list_full", "BIAS", "dayint", "stn",
                           "..features0", "..y_var",
                           "xgboost.dart.cvtune",
                           "..features_updated_Y", "xgboost.dart.cvtune"
  ))
}
