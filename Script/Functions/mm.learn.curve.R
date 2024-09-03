mm.learn.curve <- function (dat_train, dat_test, Tvarn, IDvarn, seed = 49789)
  # this function creates a learning curve for a given model
  # Arguments:
  # <dat_train>     a data frame containing the train data
  # <dat_test>      a data frame containing the test data
  # <Tvarn>         an character vector containing the name of time variable
  # <IDvarn>        an character vector containing the name of patient ID variable
  # <seed>          an optional seed
  
  # dat_train <-  train_mm
  # dat_test <- test_mm
  # IDvarn <- "Number"
  # Tvarn <- "Days"
  # seed <- seed

{
  source('Script/Functions/my.rep.folds.R') # create patient-wise repeated folds for CV
  
  # make sure that IDvarn and Tvarn are called correctly so that LME knows what to do
  iID <- grep(IDvarn,names(dat_train)) # find col index of IDvar
  iT <- grep(Tvarn,names(dat_train)) # find col index of Tvar
  
  # rename IDvar and Tvar in dat_train and dat_test to match LME formula's
  colnames(dat_train)[iID] <- "Number"
  colnames(dat_test)[iID] <- "Number"
  colnames(dat_train)[iT] <- "Time"
  colnames(dat_test)[iT] <- "Time"
  
  # set seed
  set.seed(seed)
  
  ## split the train data into small subsets of approximately equal size ##
  # ~percent increment
  per_inc <- 0.10
  
  # get unique patients
  unique <- unique(dat_train$Number)
  
  # calculate subset size (in # of patients)
  nPatients <- length(unique)
  subsetsize <- floor(nPatients * per_inc)
  nSubsets <- ceiling(nPatients / subsetsize)
  
  # shuffle to ensure randomness
  shuffled <- sample(unique)
  
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
  trainsize <- integer(length(nSubsets))
  
  # define initial train set
  cur_patients <- subsets[[1]]
  
  # iterate over subsets:
  # train model on increasing train size and validate performance
  for (i in 1:nSubsets) {
    
    # extract the current training data
    cur_train <- dat_train %>% 
      filter(Number %in% cur_patients)
    
    # built mixed model  
    M <- lme(ARAT ~ SA * ns(Time, knots = c(6, 11, 19, 34, 50, 91)) + FE * ns(Time, knots = c(6, 11, 19, 34, 50, 91)),
                 data = cur_train,
                 random = list(Number = pdDiag(form = ~ ns(Time, knots = c(6, 11, 19, 34, 50, 91)))),
                 na.action = na.exclude,
                 control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
            )
    
    # perform predictions and retrieve absolute error
    pred_train <- predict.multi.MM(M, 
                                  dat_train, 
                                  nm = 1, # serial measurements to include (not functional)
                                  Tvarn = "Time", # time var
                                  IDvarn = "Number" # ID var
                                  )
    
    pred_test <- predict.multi.MM(M, 
                                  dat_test, 
                                  nm = 1, # serial measurements to include (not functional)
                                  Tvarn = "Time", # time var
                                  IDvarn = "Number" # ID var
                                  )
    
    # calculate absolute errors
    AE_test <- abs(pred_test$pred_ARAT - pred_test$last_ARAT)
    AE_train <- abs(pred_train$pred_ARAT - pred_train$last_ARAT)
    
    # store mean abs. error
    MedAE_test[i] <- median(AE_test)
    MAE_test[i] <- mean(AE_test)
    MedAE_train[i] <- median(AE_train)
    MAE_train[i] <- mean(AE_train)
    
    # store size of training set
    trainsize[i] <- length(unique(cur_train$Number))
    ## NOTE: debateable if this is fair. This model trains on multiple obs. per patient, so the number of obs. it 'sees'
    ## is actually much higher than XGB. 
    
    if (i < nSubsets){
      # increase train set size: add next subset to current train data 
      cur_patients <- c(cur_patients,subsets[[i+1]])
    }
  }
  
  # store eval metrics
  lcresults <- data.frame(Train_size = trainsize, 
                          MedAE_test = MedAE_test, 
                          MedAE_train = MedAE_train, 
                          MAE_test = MAE_test, 
                          MAE_train = MAE_train)  
  
  # prepare for plotting
  vars_to_plot <- select(lcresults,-c(MedAE_test,MedAE_train))
  long <- gather(vars_to_plot, key = "Set", value = "val",-Train_size)
  
  # plot MAE of train and test
  lcplot1 <- ggplot(long, aes(x = Train_size, y = val)) + 
    geom_line(aes(colour = Set), linewidth = 1) + 
    labs(x = 'Training Sample Size', y= 'MAE') +
    scale_color_viridis(discrete=T)
  
  vars_to_plot <- select(lcresults,-c(MAE_test, MAE_train))
  long <- gather(vars_to_plot, key = "Set", value = "val",-Train_size)
  
  # plot MedAE of train and test
  lcplot2 <- ggplot(long, aes(x = Train_size, y = val)) + 
    geom_line(aes(colour = Set), linewidth = 1) + 
    labs(x = 'Training Sample Size', y= 'MedAE') +
    scale_color_viridis(discrete=T)
  
  lc_dat <- list(lcresults,lcplot1, lcplot2)
  
  return(lc_dat)
}