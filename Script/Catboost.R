# ------------------------------------------------------------------------------------------------------------------------ #
#                         Prediction of patient-specific recovery based on early clinical data                             #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Pre-processing and exploration of patient data and a comparison of different prediction modelling methods  #
# Authors:      Govert van der Gun                                                                                         #
# Created:      15-09-2023                                                                                                 #
# Version:      0.2 (11-2023)                                                                                              #
# R.version:    4.3.1 (2023-06)                                                                                            #
# Rstudio:      2023.09.1-494                                                                                              #
#                                                                                                                          #
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #
 

# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  # working directory
  setwd("V:\\Prive\\Gun, Govert van der\\BoostMe")
  # setwd("C:\\Users\\289035\\R")

  source("setup.R") # general setup
  source('box.plot.jitter.R') # custom function for jittered box plots
  source('Selles_MM.R') # custom function(s) for running Selles mixed models
  source('xgb.learn.curve.R') # custom function for creating learning curve
  
  # seed for reproducibility
  seed <- 497892
  set.seed(seed)
  
  # select run
  run <- 1
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                      Data acquisition                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  # load raw data
  load("Data_raw.RData")
  data_raw <- data
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                      Data preprocessing                                                  #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  # remove outliers and/or erroneous data
  summary(data_raw)
  
  # count & drop patients with 1 or less observations
  patient_counts <- data_raw %>%
    group_by(Number) %>%
    summarise(Count = n())
  
  # count
  patient_counts %>% 
    summarise(across(Count, list(mean = mean, sd = sd, min = min, max = max), .names = "nObs-{fn}")) 
  nPatients_drop <- sum(patient_counts$Count <= 1) 
  
  cat("# Patients with less than 2 obs.:", nPatients_drop, "\n")
  
  # drop
  data <- data_raw %>%
    filter(Number %in% patient_counts$Number[patient_counts$Count > 1])
  
  # replace erroneous data
  data <- data %>% 
    mutate(RTPA = replace(RTPA, RTPA > 1, NA)) %>%  # RTPA is binary?
    mutate(Sens = replace(Sens, Sens > 2, NA)) # NIHS scale 8 is max [2]
  
  # drop patients who's last obs. < 152d or first obs. >90d
  patients_to_drop <- data %>%
          group_by(Number) %>%
          summarise(first_obs = first(Days), last_obs = last(Days)) %>%
          filter(first_obs > 90 | last_obs < 152) %>%
          pull(Number)
  
  data <- data %>% filter(!(Number %in% patients_to_drop))
  
  # convert categorical vars to factor
  fdata <- data
  fdata$Number <- factor(data$Number)
  
  labels <- list(
    GENDER = c("male", "female"),
    BAMFORD = c("LACI", "PACI", "TACI"),
    RTPA = c("no", "yes"),
    AFFECTED_BODYSIDE = c("right", "left"),
    PREFERRED_HAND = c("right", "left", "no pref."),
    SA = c("0","9","14","19","25","33"),
    FE = c("none", "partial", "full"),
    Sens = c("0","1","2"),
    Neglect = c("0","1","2")
  )
  
  for (var_name in names(labels)) {
      # convert to factor and assign correct labels
      fdata[[var_name]] <- factor(fdata[[var_name]], labels = labels[[var_name]])
      # keep useful metadata
      attr(fdata[[var_name]], "label") <- attr(data_raw[[var_name]],"label") 
  }
  
  # ------------------------------------------------------
  # Problem 1
  # ------------------------------------------------------
  # predict last_ARAT, based on first assessment all covariates (1 observation per patient)
  
    # select last_ARAT and last_Days of each patient
    last_ARAT <- fdata %>%
      group_by(Number) %>%
      slice_max(Days,n=1) %>% # filter last obs.
      ungroup() %>% 
      select('Number','ARAT', 'Days') %>%  # select only ARAT, Days and patient ID
      rename(last_ARAT = ARAT, last_Days = Days) %>% 
      filter(last_Days >= 152) # only obs. of 5 months or older
    
    # select first observation of all variables of each patient
    first_assess <- fdata %>%
      group_by(Number) %>%
      slice_min(Days,n=1) %>% # select first col
      ungroup() %>%  
      filter(Days <= 90) # drop obs. of 3 months or older
    
    # merge and sort
    data1 <- merge(first_assess, last_ARAT, by = "Number") %>% arrange(Number)
    
    # get variable names
    vars = colnames(data1)
    allvars = vars[2:length(vars)] # all variables except patient number
    catvars = names(labels) # only categorical variables
    numvars = setdiff(allvars, catvars) # only numerical variables
    
    # store in df for modelling
    dat_mod <- data1
      
    if (run == 2){
      # ------------------------------------------------------
      # Problem 2
      # ------------------------------------------------------
      
      # predict last_ARAT, based on first assessment + most important covariates (1 observation per patient)
  
      keepcols <- c("Number","MIARM","FMARM","MILEG","NIHSS","Days","AGE","ARAT","SA","BAMFORD","Neglect","RTPA",
                    "last_ARAT","last_Days")
  
      # drop non-important predictors, convert SA to numeric
      data2 <- data1 %>%
        select(all_of(keepcols))
      
      # get variable names
      vars = colnames(data2)
      allvars = vars[2:length(vars)] # all variables except patient number
      catvars = c("BAMFORD","Neglect","RTPA","SA") # only categorical variables
      numvars = setdiff(allvars, catvars) # only numerical variables
      
      # store in df for modelling
      dat_mod <- data2
    }
      
    if (run == 3){
      # ------------------------------------------------------
      # Problem 3
      # ------------------------------------------------------
      
      # predict last_ARAT, based on first assessment + all covariates except FMARM
      
      # drop non-important predictors, convert SA to numeric
      data3 <- data1 %>%
        select(-FMARM)
      
      # get variable names
      vars = colnames(data3)
      allvars = vars[2:length(vars)] # all variables except patient number
      catvars = names(labels) # only categorical variables
      numvars = setdiff(allvars, catvars) # only numerical variables
      
      # store in df for modelling
      dat_mod <- data3
    }
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                       Data exploration                                                   #
# ------------------------------------------------------------------------------------------------------------------------ #
 
  # ------------------------------------------------------
  # Analyse missing data
  # ------------------------------------------------------   
    
    # check for NA's
    colSums(is.na(dat_mod))
    
  # ------------------------------------------------------
  # Normality check
  # ------------------------------------------------------ 
    
    # numerical variables only
    dat_norm <- dat_mod %>%
      select(all_of(numvars)) %>%
      na.omit()
    
    # apply shapiro test to columns and immediately extract p value
    shap_p <- apply(dat_norm, 2, function(x) shapiro.test(x)$p.value) 
    
    # create data frame for plotting with p value as character
    shap_lab <- data.frame(
      label = paste0("p = ",as.character(shap_p)),
      Variable  = numvars
    )
    
    dat_norm %>%   
      gather(key = Variable, value = Value) %>% 
      ggplot(aes(x=Value)) + geom_density(fill="lightgrey") + fancy +
      facet_wrap(~Variable,ncol=3,scales="free") +
      geom_text(
        data    = shap_lab,
        mapping = aes(x = Inf, y = -Inf, label = label),
        hjust = 1.2, 
        vjust = -3, 
      ) +
      labs(x = "",
           y = "")
    
  # ------------------------------------------------------
  # Exploratory plots
  # ------------------------------------------------------ 
    
    # Convert the data from wide to long format
    dat_long_cat <- melt(dat_mod, id.vars = "Number", measure.vars = catvars)
    
    # Create separate bar plots for each categorical variable
    ggplot(dat_long_cat, aes(x = value)) +
      geom_bar() +
      labs(
        x = "Category",
        y = "Count"
      ) +
      fancy_multi +
      facet_wrap(~variable, scales = "free")
    
    # create jittered box plots for all numvars
    plots <- box.plot.jitter(dat_mod, ID = "Number", vars = numvars)
      
    # plot boxplots in grid
    grid.arrange(grobs = plots, ncol = 3)
      
    # get mean and sd for numvars
    dat_mod %>% 
      summarise(across(all_of(numvars), list(mean = mean, sd = sd, min = min, max = max), .names = "{col}-{fn}")) %>% 
      pivot_longer(cols = everything(), # pivot to long format and rename cols/rows
                  names_to = c(".value", "stat"),
                  names_sep = "-")
    
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                     Feature engineering                                                  #
# ------------------------------------------------------------------------------------------------------------------------ #
    
    # Create dummy vars model
    formula <- as.formula(paste("~ ", paste(catvars, collapse = " + "))) # formula for the vars to OHE
    dummies_model <- dummyVars(formula, data=dat_mod) # exclude patient ID

    # Apply dummy vars model to data
    dat_mod_ohe <- data.frame(predict(dummies_model, newdata = dat_mod))
    
    # get colnames of dummyvars
    dummynames <- colnames(dat_mod_ohe)
    
    dat_mod <- cbind(dat_mod,dat_mod_ohe) # bind encoded vars back to main modelling data 
    # NOTE: factor variables are still included in the df, make appropriate selection before training models

# ------------------------------------------------------------------------------------------------------------------------ #
#                                                       Data modelling                                                     #
# ------------------------------------------------------------------------------------------------------------------------ #
  
    # ------------------------------------------------------
    # Data Partitioning
    # ------------------------------------------------------
    
    # grouped based split (80-20)
    parts <- partition(dat_mod, p = 0.20, id_col = "Number")
    test_split <- parts[[1]] %>% 
      mutate(Set = "Test")
    train_split <- parts[[2]] %>% 
      mutate(Set = "Train")
    
    # evaluate balance train-test split
    combined_dat_mod <- rbind(train_split, test_split)
    
    # number of observations
    ggplot(combined_dat_mod,aes(x = Set)) +
      geom_bar() +
      labs(x = "Set",
           y = "Count") +
      fancy
    
    # ARAT scores
    ggplot(combined_dat_mod,aes(x = last_ARAT, fill = factor(Set))) +
      geom_density(alpha = 0.5) +
      labs(x = "last_ARAT",
           y = "Density") +
      scale_fill_viridis(discrete=T) +
      fancy
    
    # stratified repeated cross-validation (use this when multiple obs. per patient)
    nrepeats <- 5 # [5-10]
    nfolds <- 5 # [5-10]
    
    # create new folds for each repetitions and convert to a caret-friendly format
    myRepFolds <- 1:nrepeats %>%
      map(
        function(x) {
          # create folds
          folds <- 1:nfolds
          train_folds <- fold(train_split, k = length(folds), id_col = 'Number')
          
          # Convert our folds to a caret-friendly format
          myFolds <- map(folds, ~ which(train_folds$.folds %in% .))
          
          foldNames <- folds %>%
            map(
              ~ case_when(
                TRUE ~ paste0('Fold', ., '.Rep', x)
              )
            )
          unlist
          
          names(myFolds) <- foldNames
          
          myFolds
        }
      )
    
    myRepFolds <- list_flatten(myRepFolds) # caret requires flat list
    
    # ------------------------------------------------------
    # Data Preparation
    # ------------------------------------------------------
    
    # select data for XGBoost
    # xgb requires model formula and OHE encoded categorical variables
    
    train_xgb <- select(train_split,-c(Number,Set,last_Days,all_of(catvars))) # drop non-predictors and catvars
    test_xgb <- select(test_split,-c(Number,Set,last_Days,all_of(catvars)))
    
    # # # create pre-processing model. On train predictors only to avoid leakage
    # ppmodel <- preProcess(select(train_xgb,-last_ARAT), method = c("center", "scale"))
    # 
    # # apply model to train and test set and reattach last_ARAT (not scaled)
    # ttrain_xgb <- predict(ppmodel, train_xgb) %>%
    #   mutate(last_ARAT = train_xgb$last_ARAT)
    # ttest_xgb <- predict(ppmodel, test_xgb) %>%
    #   mutate(last_ARAT = test_xgb$last_ARAT) 
    
    # select data for Catboost
    # cb requires XY variable structure and handles categorical variables itself
    
    train_cb <- select(train_split,-c(Number,Set,last_Days,all_of(dummynames))) # drop non-predictors and dummyvars
    test_cb <- select(test_split,-c(Number,Set,last_Days,all_of(dummynames)))
    
    X <- select(train_cb,-c(last_ARAT)) # predictors
    Y <- train_cb$last_ARAT # target variable
    
    # select data for Mixed Effects Model
    # one ore multiple repeated measurements of ARAT, SA, FE, Days & Number
    
    mm_measures <- c('Number','Days','SA','FE','ARAT')
    
    # train data frame
    train_mm <- data %>% # data = df before factor conversion and before repeated measurements were dropped
      select(all_of(mm_measures)) %>% # select relevant predictors
      filter(Number %in% train_split$Number) # filter only patients in train split
    
   # idem for test data frame
    test_mm <- data %>%
      select(all_of(mm_measures)) %>% 
      filter(Number %in% test_split$Number)
      
  # ------------------------------------------------------
  # Training
  # ------------------------------------------------------
    
    # training method and config
    train_ctrl <- trainControl(
      method = "cv", # cross-validation
      number = length(myRepFolds), # amount of folds*repeats
      index = myRepFolds, # fixed folds for repeated CV
      verbose = F, # TRUE for training log
      allowParallel = F, # FALSE for reproducible results
    )
      
     # ------------------------------------------------------
     # XGBoost
     # ------------------------------------------------------
    
      # XGB default hyperparameters
      xgb_grd <- expand.grid(
        nrounds = 100, # number of boosting rounds
        max_depth = 6, # max tree depth
        eta = 0.3, # learning rate
        gamma = 0, # regularization param. (pruning)
        colsample_bytree = 1, # fraction of features used for each tree
        min_child_weight = 1, # minimum sum of instance weight (prevent single obs. childs)
        subsample = 1 # fraction of training data used in each round
      )
    
      # train xgb base model
      M0_xgb <- train(
        last_ARAT ~ ., # model
        data = train_xgb, # train data set, drop cols that should not be used as predictor
        trControl = train_ctrl, # train method
        tuneGrid = xgb_grd, # hyperparameters
        method = "xgbTree", # modelling method
        metric = "MAE", # performance metric to optimize
        na.action = na.pass
      )
      
    # ------------------------------------------------------
    # CatBoost
    # ------------------------------------------------------
      
      # CatBoost default hyperparameters
      cb_grd <- expand.grid(
        iterations = 100, # number of boosting rounds
        depth = 6, # max tree depth
        learning_rate = 0.3, # learning rate
        l2_leaf_reg = 0, # regularization param. (pruning)
        border_count = 64, # number of splits for nums
        rsm = 1 # fraction of features used for each tree
        )

      # train catboost base model
      M0_cb <- train(X, # predictors
                     Y, # target variable
                     trControl = train_ctrl, # train method
                     tuneGrid = cb_grd, # hyperparameters
                     method = catboost.caret, # modelling method
                     metric = "MAE", # performance metric to optimize
                     logging_level = 'Silent'
                     )
      
    # ------------------------------------------------------
    # Selles Mixed Model
    # ------------------------------------------------------
      
      # built mixed model  
      M0_mm <- lme(ARAT ~ SA * ns(Days, knots = c(6, 11, 19, 34, 50, 91)) + FE * ns(Days, knots = c(6, 11, 19, 34, 50, 91)),
                   data = train_mm,
                   random = list(Number = pdDiag(form = ~ ns(Days, knots = c(6, 11, 19, 34, 50, 91)))),
                   na.action = na.exclude,
                   control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
                   )

  # ------------------------------------------------------
  # Tuning
  # ------------------------------------------------------
  
  # ------------------------------------------------------
  # Model evaluation & comparison
  # ------------------------------------------------------

    # predict last ARAT using boosting machines
    pred_last_ARAT_xgb <- predict(M0_xgb, test_xgb)
    pred_last_ARAT_cb <- predict(M0_cb, test_cb)
    
    # predict last ARAT for MM using IndvPred_lme (custom, Selles et al.)
    # seems to work for individuals only so looping through patients
    iPatients <- unique(test_mm$Number) # unique patients in test set
    
    # extract outcome measure
    mm_outcome <- test_mm %>% # last obs. of each patient
      group_by(Number) %>% 
      slice_max(Days, n=1) %>% 
      ungroup()
    
    # extract predictors
    nm <- 1 # number of serial measurements to include
    mm_predictors <- test_mm %>% # first nm obs. of each patient
      group_by(Number) %>% 
      slice_min(Days, n=nm) %>% 
      ungroup() 
    
    # preallocate
    pred_last_ARAT_mm <- matrix(0, nrow = length(iPatients), ncol = 2)
    
    for (i in 1:length(iPatients)){
      cur_patient <- iPatients[i] # ID of current patient
      
      cur_pat_dat <- mm_predictors[mm_predictors$Number == cur_patient,] # prediction data of current patient
      
      cur_pat_outcome <- mm_outcome[mm_outcome$Number == cur_patient,] # outcome of current patient
   
      # custom function by Selles et al.
      cur_pat_pred <- IndvPred_lme(lmeObject = M0_mm, # model object
                                   newdata = cur_pat_dat, # data to provide predictions for. Can be 1 or multiple patients with repeated measurements
                                   timeVar = "Days", # name of time variable in observational data (newdata)
                                   times = c(cur_pat_outcome$Days-1,cur_pat_outcome$Days), # custom time variable for the predictions
                                   all_times = FALSE, # logical to set if predictions should be made for observed timepoints as well (T=yes)
                                   level = 0.95, # confidence interval level
                                   M = 500, # ?
                                   interval = "prediction", # ?
                                   return_data = TRUE) # ?
      
      # select only last_ARAT and patient ID
      cur_pat_pred <- cur_pat_pred %>% 
        filter(Days == cur_pat_outcome$Days) %>% 
        select(c(Number,pred))
      
      # store in matrix
      pred_last_ARAT_mm[i, ] <- as.numeric(cur_pat_pred)
    }
    
    # merge outcome and predictions
    pred_last_ARAT_mm <- as_tibble(pred_last_ARAT_mm)
    pred_mm <- cbind(pred_last_ARAT_mm,mm_outcome$ARAT)
    colnames(pred_mm) <- (c("Number", "pred_ARAT","last_ARAT"))
    
    # calculate and store performance metrics
    maxARAT <- 57
    results <- data.frame(Number =  test_split$Number, # patient number
                          AE_M0_xgb = abs(test_xgb$last_ARAT - pred_last_ARAT_xgb), # absolute error
                          sAE_M0_xgb = abs(test_xgb$last_ARAT - pred_last_ARAT_xgb) / maxARAT, # min-max scaled
                          AE_M0_cb = abs(test_cb$last_ARAT - pred_last_ARAT_cb),
                          sAE_M0_cb = abs(test_cb$last_ARAT - pred_last_ARAT_cb) / maxARAT,
                          AE_M0_mm = abs(pred_mm$last_ARAT - pred_mm$pred_ARAT),
                          sAE_M0_mm = abs(pred_mm$last_ARAT - pred_mm$pred_ARAT) / maxARAT
                          )
    
    # print results to console
    median(results$AE_M0_xgb)
    median(results$AE_M0_cb)
    median(results$AE_M0_mm)

# ------------------------------------------------------------------------------------------------------------------------ #
#                                                    Data visualization                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
 
  # ------------------------------------------------------
  # Figures
  # ------------------------------------------------------
      
    # create jittered box plots for performance metrics
    plots <- box.plot.jitter(results, ID = 'Number')
    grid.arrange(grobs = plots, ncol = 2)
    
    # plot recovery profile of outliers
    p1 <- fdata %>%
      filter(Number %in% plots$AE_M0_xgb$outliers$Number) %>% 
      ggplot(aes(x = Days, y = ARAT, colour=Number)) +
        geom_point() +
        geom_line() +
        labs(
          x = "Days",
          y = "ARAT",
          title = "XGBoost"
        ) +
        scale_color_viridis(discrete=T) +
        fancy
    
    p2 <- fdata %>%
      filter(Number %in% plots$AE_M0_cb$outliers$Number) %>% 
      ggplot(aes(x = Days, y = ARAT, colour=Number)) +
      geom_point() +
      geom_line() +
      labs(
        x = "Days",
        y = "ARAT",
        title = "CatBoost"
      ) +
      scale_color_viridis(discrete=T) +
      fancy
    
    grid.arrange(grobs = list(p1,p2), ncol = 2)
    
    # MAE vs Days
    p1 <- merge(results, dat_mod, by = "Number") %>% # merge results and modelling data
      ggplot(aes(x = Days, y = AE_M0_xgb, colour = last_ARAT)) +
      geom_point() +
      labs(
        x = "Days first measurement",
        y = "Absolute Error",
        title = "XGBoost"
      ) +
      scale_color_viridis(discrete = F) +
      fancy
    
    p2 <- merge(results, dat_mod, by = "Number") %>% # merge results and modelling data
      ggplot(aes(x = Days, y = AE_M0_cb, colour = last_ARAT)) +
      geom_point() +
      labs(
        x = "Days first measurement",
        y = "Absolute Error",
        title = "CatBoost"
      ) +
      scale_color_viridis(discrete = F) +
      fancy
    
    grid.arrange(grobs = list(p1,p2), ncol = 2)
    
    # SHAP
    # NOTE: SHAP is calculated for all observations (not only train or test)

      # shap for CatBoost
      dat_shap_cb <- select(dat_mod,-c(Number,last_Days,last_ARAT,all_of(dummynames))) 
      colnames <- colnames(dat_shap_cb)
      cbpool <- catboost.load_pool(dat_shap_cb) # catboost pool for predictors
      # get shap values from catboost model:
      shapvalscb <- catboost.get_feature_importance(M0_cb$finalModel, pool = cbpool, type = "ShapValues") 
      shapvalscb <- data.frame(shapvalscb) %>% # convert to df and drop BIAS col (last one)
        select(-ncol(.))
      colnames(shapvalscb) <- colnames # reassign colnames
      
      # for XGboost
      dat_shap_xgb <- select(dat_mod,-c(Number,last_Days,last_ARAT,all_of(catvars)))
      colnames <- colnames(dat_shap_xgb)
      shapvalsxgb <- shap.values(M0_xgb$finalModel, X_train = as.matrix(dat_shap_xgb)) # get shap values from xgb model
      
      if (run == 1 | run == 3){
        shapvalsxgb <- data.frame(shapvalsxgb$shap_score) %>% # convert to dataframe and aggregate dummyvars shap values
          mutate(BAMFORD = `BAMFORD.LACI` + `BAMFORD.TACI` + `BAMFORD.PACI`) %>% 
          mutate(RTPA = `RTPA.no` + `RTPA.yes`) %>% 
          mutate(Neglect = `Neglect.0` + `Neglect.1` + `Neglect.2`) %>% 
          mutate(GENDER = `GENDER.male` + `GENDER.female`) %>% 
          mutate(AFFECTED_BODYSIDE = `AFFECTED_BODYSIDE.right` + `AFFECTED_BODYSIDE.left`) %>% 
          mutate(PREFERRED_HAND = `PREFERRED_HAND.right` + `PREFERRED_HAND.left` + `PREFERRED_HAND.no.pref.`) %>% 
          mutate(SA = `SA.0` + `SA.9` + `SA.14` + `SA.19` + `SA.25` + `SA.33`) %>% 
          mutate(FE = `FE.none` + `FE.partial` + `FE.full`) %>%
          mutate(Sens = `Sens.0` + `Sens.1` + `Sens.2`) %>% 
          select(-all_of(dummynames)) # drop non-aggregated shap vals
      } else if (run == 2){
        shapvalsxgb <- data.frame(shapvalsxgb$shap_score) %>% # convert to dataframe and aggregate dummyvars shap values
          mutate(BAMFORD = `BAMFORD.LACI` + `BAMFORD.TACI` + `BAMFORD.PACI`) %>% 
          mutate(RTPA = `RTPA.no` + `RTPA.yes`) %>% 
          mutate(Neglect = `Neglect.0` + `Neglect.1` + `Neglect.2`) %>% 
          select(-all_of(dummynames)) # drop non-aggregated shap vals
      }
      
      # prepare shap values for plotting
      svobjcb <- shapviz(data.matrix(shapvalscb), X = dat_shap_cb)
      # use dat_shap_Cb instead of dat_shap_xgb b/c dummy shap-vals are already aggregated
      svobjxgb <- shapviz(data.matrix(shapvalsxgb), X = dat_shap_cb)
      
      # plot beeswarms
      p1 <- sv_importance(svobjxgb, kind = "beeswarm") +
        labs(
          title = "XGBoost"
        )
      
      p2 <- sv_importance(svobjcb, kind = "beeswarm") +
        labs(
          title = "CatBoost"
        )
      
      grid.arrange(grobs = list(p1,p2), ncol = 2)

    # learning curve (custom function)
    # tic()
    # lc_dat <- xgb.learn.curve(train_split, # train data
    #                           test_split, # test data
    #                           colnames(select(train_xgb,-last_ARAT)), # names of predictors
    #                           "last_ARAT", # name of target
    #                           xgb_grd, # tuning grid
    #                           iID = 1, # patient ID col index
    #                           seed = seed
    #                           )
    # toc()
    # save(lc_dat, file = "lc_dat.RData")
    
    # sensitivity to trainig sample size
    load('lc_dat.RData')
    lc_dat[[2]] + fancy  
      
      
############################################################################################################################
#                                                   End of syntax                                                          #
############################################################################################################################            