predict.multi.MM <- function (M, new_dat, pred_time = 180, alpha = 0.20){
  # this function retrieves predictions of Selles MM for multiple patients. Since prediction function for Selles MM
  # (IndvPred_lme) only works for individual patients this function loops through patients and stores results.
  #
  # Arguments:
  # <M>             lme model object
  # <new_dat>       a data frame with new data to provide predictions for, should contain predictors and outcome
  # <pred_time>     an optional integer with time to provide point predictions for
  # <alpha>         an optional significance level [0,1]
  
  # debug
  # M <- M_mm
  # new_dat <- eval_mm
  # pred_time <- 180
  # alpha <- 0.2

  source('Script/Functions/Selles.MM.R') # custom function(s) for running Selles mixed models
  
  # store patient IDs in test data
  iPatients <- unique(new_dat$Number) # unique patients in test set
  
  # preallocate
  pred_last_ARAT_mm <- matrix(0, nrow = length(iPatients), ncol = 4)
  
  for (i in 1:nrow(pred_last_ARAT_mm)){
    
    cur_patient <- iPatients[i] # ID of current patient
    
    cur_pat_dat <- subset(new_dat, Number == cur_patient) # prediction data of current patient
    
    # custom function by Selles et al.
    cur_pat_pred <- IndvPred_lme(lmeObject = M, # model object
                                 newdata = cur_pat_dat, # data to provide predictions for
                                 timeVar = "Days", # name of time variable in observational data (newdata)
                                 times = c(pred_time-1,pred_time), # custom time variable for the predictions
                                 all_times = FALSE, # logical to set if predictions should be made for observed timepoints as well (T=yes)
                                 level = 1-alpha, # confidence interval level
                                 M = 500, # ?
                                 interval = "prediction", # ?
                                 return_data = TRUE) # ?
    
    # select patient ID, predicted last_ARAT and lower and upper PI boundaries
    cur_pat_pred <- cur_pat_pred %>% 
      filter(Days == pred_time) %>% 
      select(Days,pred,low,upp)
    
    # store in matrix
    pred_last_ARAT_mm[i,1] <- as.numeric(levels(cur_patient))[cur_patient]
    pred_last_ARAT_mm[i,2:4] <- as.numeric(cur_pat_pred[2:4])                                   
  }
  
  # merge outcome and predictions
  Actual <- new_dat %>% distinct(Number, .keep_all = TRUE)
  pred_mm <- data.frame(pred_last_ARAT_mm) %>% 
    mutate(Actual = Actual$Outcome)
  colnames(pred_mm) <- (c("Number", "Predicted","lb","ub","Actual"))
  
  return(pred_mm)
}