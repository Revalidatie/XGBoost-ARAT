# First run main.R until definition of Selles et al. Mixed Model (M_mm)
# this scripts store prediction performance of the XGB ensemble applied at 
# different times post-stroke
weeks <- c(1, 6, 13)
names <- c("Week 1", "Week 6", "Week 13")

perf_wks_xgb <- data.frame()
baseline_ARAT <- test_xgb %>% 
  group_by(Number) %>% 
  slice_min(Days, n=1) %>% # first obs
  ungroup() %>% 
  select(Number, ARAT) %>% 
  rename(bARAT = ARAT)

xgb_grd <- expand.grid(
  nrounds = 1000, # number of boosting rounds
  max_depth = 4, # max tree depth
  eta = 0.015, # learning rate
  gamma = 0, # regularization param. (pruning)
  colsample_bytree = 1, # fraction of features used for each tree
  min_child_weight = 1, # minimum sum of instance weight (prevent single obs. childs)
  subsample = 0.75 # fraction of training data used in each round
)

for(i in 1:length(weeks)){
  # define moment post-stroke [Days] when predictions are made (day at which model is applied)
  wk <- weeks[i]
  applied_at <- wk*7
  
  # extract measurement(s) from test set to evaluate XGB on
  eval_xgb2 <- test_xgb %>% 
    group_by(Number) %>% 
    filter(Days <= applied_at) %>% 
    slice_max(Days, n=1) %>% # most recent measurement available
    ungroup()
  
  # get bagged prediction of XGB ensemble
  pred_list2 <- predict.XGB(dat_train = train_xgb, # train data to bootstrap on
                           new_dat = eval_xgb2, # new data to provide predictions + intervals for
                           X = colnames(select(train_xgb,-c(Outcome,Number))), # names of predictors
                           grd = xgb_grd, # tune grid
                           nb = 300, # number of bootstraps
                           alpha = 0.2, # significance level for PIs
                           seed = seed
  )
  
  # calculate performance metrics
  pred_xgb2 <- pred_list2[[1]] %>%
    mutate(AE_xgb = abs(Actual-Predicted)) %>% 
    mutate(PIw = abs(lb-ub))
  
  # create groups and select vars of interest
  pred_grps_xgb <- merge(baseline_ARAT, pred_xgb2, by="Number")
  AE <- pred_grps_xgb %>% 
    mutate(ARAT_base = factor(case_when(bARAT <= 22 ~ "L",
                                        bARAT > 22 & bARAT <= 47 ~ "M",
                                        bARAT > 47 ~ "H"),levels = c("L","M","H"))) %>%
    mutate(ARAT_out = factor(case_when(Actual < 20 ~ "L",
                                       Actual >= 20 & Actual < 40 ~ "M",
                                       Actual >= 40 ~ "H"), levels = c("L","M","H"))) %>% 
    select(AE_xgb, PIw, ARAT_base, ARAT_out) %>% 
    rename(AE = AE_xgb)
  
  # calculate performance metric on unseen data
  AE_labeled <- cbind(AE, time = names[i])
  perf_wks_xgb <- rbind(perf_wks_xgb, AE_labeled)
}

perf_wks_xgb$time <- factor(perf_wks_xgb$time, levels = names)

#save(perf_wks_xgb,file = "Data/perf_over_wks_xgb.Rdata")

# note: AE for week 13 and week 6 are similar or even the same in the medium and high baseline groups. This is because the patients in those groups didn't have any more recent measurements than 6 weeks. 
