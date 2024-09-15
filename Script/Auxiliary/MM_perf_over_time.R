# First run XGB.R until definition of M0 (Selles Mixed Model)

weeks <- c(1, 6, 13)
names <- c("Week 1", "Week 6", "Week 13")

perf_wks_mm <- data.frame()
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
  
  # extract measurement(s) from test set to evaluate MM
  eval_mm2 <- test_mm %>% 
    group_by(Number) %>% 
    filter(Days <= applied_at) %>% 
    ungroup()
 
  # get predictions of MM
  pred_mm2 <- predict.multi.MM(M = M_mm, # lme model object
                               new_dat = eval_mm2, # new data to provide predictions for
                               alpha = alpha # significance level for CIs 
  )
  
  # calculate performance metric on unseen data
  pred_mm2 <- pred_mm2 %>%
    mutate(AE_mm = abs(Actual-Predicted)) %>% 
    mutate(lb = pmax(0, pmin(lb, 57))) %>% 
    mutate(ub = pmax(0, pmin(ub, 57))) %>% 
    mutate(PIw = abs(lb-ub))
  
  # create groups and select vars of interest
  pred_grps_mm <- merge(baseline_ARAT, pred_mm2, by="Number")
  AE <- pred_grps_mm %>% 
    mutate(ARAT_base = factor(case_when(bARAT <= 22 ~ "L",
                                        bARAT > 22 & bARAT <= 47 ~ "M",
                                        bARAT > 47 ~ "H"),levels = c("L","M","H"))) %>%
    mutate(ARAT_out = factor(case_when(Actual < 20 ~ "L",
                                       Actual >= 20 & Actual < 40 ~ "M",
                                       Actual >= 40 ~ "H"), levels = c("L","M","H"))) %>% 
    select(AE_mm, PIw, ARAT_base, ARAT_out) %>% 
    rename(AE = AE_mm)
  
  # calculate performance metric on unseen data
  AE_labeled <- cbind(AE, time = names[i])
  perf_wks_mm <- rbind(perf_wks_mm, AE_labeled)
}

perf_wks_mm$time <- factor(perf_wks_mm$time, levels = names)

save(perf_wks_mm,file = "Data/perf_over_wks_mm.Rdata")

## note: AE for week 13 and week 6 are similar or even the same in the medium and high baseline groups. This is because the patients in those groups didn't have any more recent measurements than 6 weeks. 