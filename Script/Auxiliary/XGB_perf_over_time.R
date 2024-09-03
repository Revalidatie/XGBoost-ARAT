# First run XGB.R until definition of M0 (Selles Mixed Model)

weeks <- c(1, 6, 13)
names <- c("Week 1", "Week 6", "Week 13")

perf_wks <- data.frame()
baseline_ARAT <- test_xgb %>% 
  group_by(Number) %>% 
  slice_min(Days, n=1) %>% # first obs
  ungroup() %>% 
  select(Number, ARAT) %>% 
  rename(bARAT = ARAT)

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
                           grd = xgb_grd0, # tune grid
                           nb = 100, # number of bootstraps
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
    mutate(ARAT_base = factor(case_when(bARAT < 10 ~ "L",
                                        bARAT >= 10 & bARAT < 30 ~ "M",
                                        bARAT >= 30 ~ "H"),levels = c("L","M","H"))) %>%
    mutate(ARAT_out = factor(case_when(Actual < 20 ~ "L",
                                       Actual >= 20 & Actual < 40 ~ "M",
                                       Actual >= 40 ~ "H"), levels = c("L","M","H"))) %>% 
    select(AE_xgb, PIw, ARAT_base, ARAT_out)
  
  # calculate performance metric on unseen data
  AE_labeled <- cbind(AE, time = names[i])
  perf_wks <- rbind(perf_wks, AE_labeled)
}

perf_wks$time <- factor(perf_wks$time, levels = names)

#save(perf_wks,file = "Data/perf_over_wks.Rdata")
