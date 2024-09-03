predict.XGB <- function (dat_train, new_dat, X, grd, nb = 200, alpha = 0.20, seed = 137985){
  # This function computes point predictions for individual patients based on an
  # ensemble of bootstrap models. It accepts a dataframe with multiple 
  # measurements, but only the latest measurement for each model will be used 
  # for the prediction. Additionally, prediction intervals and visualizations 
  # can be computed.
  
  # Arguments:
  # <dat_train>     a data frame with training data, containing both predictors and dependent var
  # <new_dat>       a data frame with new data to provide predictions for
  # <X>             a character containing the names of the independent variables
  # <grd>           tuning grd for xgb model
  # <nb>            an optional integer specifying number of bootstrap models to create
  # <alpha>         an optional significance level [0,1]
  # <seed>          an optional integer specifying the seed
  
  # # Debug
  # dat_train <- train_xgb
  # new_dat <- eval_xgb
  # X <- colnames(select(train_xgb,-c(Outcome,Number)))
  # nb <- 300
  # grd <- xgb_grd
  # alpha <- 0.2

  # training method and config
  train_ctrl <- trainControl(
    method = "none", # fit model on full bootstrap train set
    verbose = F, # TRUE for training log
    allowParallel = F, # FALSE for reproducible results
    savePredictions = T # save predictions on hold-out folds
  )

  # # create repeated cross-validation folds
  # myRepFolds <- my.rep.folds(dat_train, IDvarn = "Number", nreps = 5, nfolds = 5, seed = seed)
  # 
  # # training method and config
  # train_ctrl <- trainControl(
  #   method = "cv", # cross-validation
  #   number = length(myRepFolds), # amount of folds*repeats
  #   index = myRepFolds, # fixed folds for repeated CV
  #   verbose = F, # TRUE for training log
  #   allowParallel = F, # FALSE for reproducible results
  #   savePredictions = T # save predictions on hold-out folds
  # )

  # create modeling formula
  form_str <- paste("Outcome ~", paste(X, collapse = " + "))
  formula <- as.formula(form_str)
  
  # init
  n <- nrow(dat_train) # nr of training samples
  nd <- nrow(new_dat) # nr of new data points to provide predictions for 
  pb <- progress_bar$new(total = nb) 
  
  # pre-allocate
  ystar <- matrix(nrow = nb, ncol = nd)
  yhat <- ystar
  
  # make predictions and calculate residuals for each bootstrapped model
  set.seed(seed)
  for (b in 1:nb) {
    
    # randomly sample part train_dat to be the bootstrapped training data
    train_idxs <- sample(1:n, size = n, replace = TRUE)
    btstrp_train <- dat_train[train_idxs,]
    btstrp_val <- anti_join(dat_train, btstrp_train, by = "Number") # remaining data is "validation" set
    
    ## Bias / Parameter Uncertainty ##
    # train bootstrap model
    M_b <- train(
      form = formula, # model formula
      data = btstrp_train, # bootstrap train data
      trControl = train_ctrl, # train method
      tuneGrid = grd, # hyperparameters
      method = "xgbTree", # modelling method
      metric = "MAE", # performance metric to optimize
      na.action = na.pass
    )
    
    # get bootstrapped model predictions for train, val and test sets
    preds_train <- predict(M_b, btstrp_train)
    preds_val <- predict(M_b, btstrp_val)
    yhat_b <- predict(M_b, new_dat)
    
    ## Variance / Residual Uncertainty ## 
    # get bootstrapped training & validation residuals
    train_res <- resid(M_b, type="p") # training residuals (from model object)
    val_res <- btstrp_val$Outcome - preds_val # bootstrapped validation residuals
    #test_res <- yhat_b - new_dat$Outcome # test residuals
    
    # take percentiles of train & val residuals and bootstrapped deviation so that they can be compared
    val_res <- quantile(val_res, probs = seq(0, 1, by = 0.005))
    train_res <- quantile(train_res, probs = seq(0, 1, by = 0.005))
    
    # Weight training and val residuals to get better performance estimate (from Elements of Statistical Learning)
      # first create matrices for y_train and preds
      y_mat <- matrix(rep(btstrp_train$Outcome, each = n), nrow = n, byrow = T)
      preds_mat <- matrix(rep(preds_train, each = n), nrow = n, byrow = F)
      
      # calculate the "no information error"
      gamma <- mean(abs(y_mat - preds_mat))
      # alt: less comp. expensive, but more sensitive to outliers
      # gamma <- mean(abs(sample(btstrp_train$Outcome) - sample(preds_train))) 
      
      # calculate relative overfitting rate & residual weight factor
      ROR <- abs(mean(val_res) - mean(train_res)) / abs(gamma - mean(train_res)) # "relative overfitting rate"
      wf <- .632 / (1 - .368 * ROR) # weight factor
    
    # calculate weighted residuals
    res <- (1 - wf) * train_res + wf * val_res
    
    # sample future residuals from the weighted residuals 
    eps <- sample(res, size = nd, replace = T)
    
    # # sample future residuals from the test residuals
    # eps <- sample(test_res, size = nd, replace = T)
    
    # simulate future data point: future Y = predicted Y + future residual
    yhat[b,] <- yhat_b
    ystar[b,] <- yhat_b + eps
    
    # add tick to progress bar
    pb$tick()
  }
  
  # point prediction = median of bootstrap predictions (median less sensitive to outliers than mean)
  yhat <- apply(yhat, 2, median)  
  
  # apply ARAT ceil and floor effect to simulated targets
  ystar <- apply(ystar, 2, function(x) pmax(pmin(x, 57), 0))
  colnames(ystar) <- new_dat$Number
  
  # get upper and lower bounds of (1-alpha) PI for each patient in new dat
  qs <- c(alpha / 2, (1 - alpha / 2))
  percentiles <- apply(ystar, 2, function(x) quantile(x, probs = qs))
  percentiles <- t(percentiles) 
  colnames(percentiles) <- c('lb', 'ub')
  
  # store in df
  pred_xgb <- data.frame(new_dat$Number,yhat,percentiles[,'lb'],percentiles[,'ub'],new_dat$Outcome)
  colnames(pred_xgb) <- (c("Number","Predicted","lb","ub","Actual"))
  
  # store in list
  pred_list <- list(pred_xgb, ystar)
  
  return(pred_list)
  
  # ggplot() +
  #   geom_point(aes(x = preds_train, y = train_res)) +
  #   labs(y = "Training residuals",
  #        x = "Fitted values")
  # 
  # ggplot(cvResults_b) +
  #   geom_point(aes(x = MedPred, y = obs-MedPred)) +
  #   labs(y = "CV residuals",
  #        x= "Fitted values")
}


