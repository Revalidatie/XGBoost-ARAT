xgb.learn.curve <- function (dat_train, dat_test, X, grid, IDvarn = "Number", seed = 49789){
  # this function creates a learning curve for a given model
  # Arguments:
    # <dat_train>     a data frame containing the train data, including patient ID and outcome
    # <dat_test>      a data frame containing the test data, including patient ID and outcome
    # <X>             a character containing the names of the independent variables
    # <IDvarn>        an optional character vector containing the name of patient ID variable
    # <grid>          a data frame containing the tuning gird
    # <seed>          an optional seed
  
  # debug
  dat_train <-  lctrain_xgb
  dat_test <- lctest_xgb
  X <- colnames(select(train_xgb,-c(Outcome,Number)))
  grid <- xgb_grd
  IDvarn <- "Number"
  seed <- seed

  source('Script/Functions/my.rep.folds.R') # create patient-wise repeated folds for CV
  
  # set seed
  set.seed(seed)
  
  ## split the train data into small subsets of approximately equal size ##
    # ~percent increment
    per_inc <- 0.10
    
    # get unique patients
    uniq_pat <- unique(dat_train[[IDvarn]])
  
    # calculate subset size (in # of patients)
    nPatients <- length(uniq_pat)
    subsetsize <- floor(nPatients * per_inc)
    nSubsets <- ceiling(nPatients / subsetsize)
    
    # shuffle to ensure randomness
    shuffled <- sample(uniq_pat)
    
    # create list with ~equally sized subsets of patients
    subsets <- split(shuffled,
                     rep(1:nSubsets, 
                         each = subsetsize, 
                         length.out = nPatients
                         )
                     )
      
  ## setup before loop ##
    # preallocate memory
    MedAE_test <- numeric(length(nSubsets))
    MAE_test <- MedAE_test
    MedAE_train <- MedAE_test
    MAE_train <- MedAE_test
    MAE_val <- MedAE_test
    trainsize <- integer(length(nSubsets))
    
    # number of folds and repeats for cross validation
    nrepeats <- 5 # [5-10]
    nfolds <- 5 # [5-10]
    
    # define initial train set
    cur_patients <- subsets[[1]]
    
    # create modeling formula
    form_str <- paste("Outcome ~", paste(X, collapse = " + "))
    formula <- as.formula(form_str)
    
    if (subsetsize < nfolds){
      stop("Size of training sample increment should be bigger than number of CV folds.")
    }
  
  # iterate over subsets:
  # train model on increasing train size and validate performance
  for (i in 1:nSubsets) {
    
    # extract the current training data
    cur_train <- dat_train %>% 
      filter(.data[[IDvarn]] %in% cur_patients)
    
    # set seed
    set.seed(seed)
    
    # create repeated cross-validation folds 
    myRepFolds <- my.rep.folds(cur_train, IDvarn = IDvarn, nreps = 5, nfolds = 5, seed = seed)
  
    # training method and config
    train_ctrl <- trainControl(
      method = "cv", # cross-validation
      number = length(myRepFolds), # amount of folds*repeats
      index = myRepFolds, # fixed folds for repeated CV
      verbose = F, # TRUE for training log
      allowParallel = F # FALSE for reproducible results
    )
    
    # train model on current train set
    M <- train(
      form = formula, # model
      data = cur_train, # train data set
      trControl = train_ctrl, # train method
      tuneGrid = grid, # hyperparameters
      method = "xgbTree", # modelling method
      metric = "MAE", # performance metric to optimize
      na.action = na.pass
    )
    
    # perform predictions and retrieve absolute error
    pred_test <- predict(M, dat_test)
    pred_train <- predict(M, dat_train)
    
    # drop rows with NA (function 'predict' will do the same)
    dat_test_nona <- na.omit(dat_test)
    dat_train_nona <- na.omit(dat_train)
    
    # calculate absolute errors
    AE_test <- abs(dat_test_nona$Outcome - pred_test)
    AE_train <- abs(dat_train_nona$Outcome - pred_train)
    
    # store mean abs. error
    MedAE_test[i] <- median(AE_test)
    MAE_test[i] <- mean(AE_test)
    MedAE_train[i] <- median(AE_train)
    MAE_train[i] <- mean(AE_train)
    MAE_val[i] <- M$results$MAE
    
    # store size of training set
    trainsize[i] <- nrow(cur_train)
    
    if (i < nSubsets){
      # increase train set size: add next subset to current train data 
      cur_patients <- c(cur_patients,subsets[[i+1]])
    }
  }
  
  lcresults <- data.frame(Train_size = trainsize, 
                          MedAE_test = MedAE_test, 
                          MedAE_train = MedAE_train, 
                          MAE_test = MAE_test,
                          MAE_train = MAE_train,
                          MAE_val = MAE_val
                          )  
  
  # plot MAE of train validation and test
  vars_to_plot <- select(lcresults,-c(MedAE_test,MedAE_train))
  long <- gather(vars_to_plot, key = "Set", value = "val",-Train_size)
  maxMAE <- max(long$val)
  label_mapping <- c("MAE_test" = "Test", 
                     "MAE_val" = "Validation",
                     "MAE_train" = "Train", 
                     "MedAE_test" = "Test", 
                     "MedAE_train" = "Train")
  
  lcplot1 <- ggplot(long, aes(x = Train_size, y = val, colour = Set, shape = Set)) + 
    geom_line(linewidth = 1) +
    geom_point(size = 3) + 
    scale_y_continuous(limits = c(0,maxMAE)) +
    labs(x = 'Training sample size', y= 'Mean Absolute Error') +
    scale_color_viridis(discrete=T, labels = label_mapping) +
    scale_shape_manual(values = c(15, 16, 17), labels = label_mapping)
  
  # plot MedAE of train and test (MedAE not available as performance metric in caret, so no val perf.)
  vars_to_plot <- select(lcresults,-c(MAE_test,MAE_train,MAE_val))
  long <- gather(vars_to_plot, key = "Set", value = "val",-Train_size)
  maxMedAE <- max(long$val)
  
  lcplot2 <- ggplot(long, aes(x = Train_size, y = val, colour = Set, shape = Set)) + 
    geom_line(linewidth = 1) +
    geom_point(size = 3) + 
    scale_y_continuous(limits = c(0,maxMedAE)) +
    labs(x = 'Training sample size', y= 'Median Absolute Error') +
    scale_color_viridis(discrete=T, labels =  label_mapping) +
    scale_shape_manual(values = c(15, 16), labels = label_mapping)
  
  lc_dat <- list(lcresults,lcplot1, lcplot2)
  
  return(lc_dat)
}