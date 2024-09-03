mod.dat.XGB <- function (dat,features,outcome,nm=1,mmn=1,multipred=F){
  # This function slices a data fame with raw data into a data frame that is
  # ready to be used as modelling data for XGB.
  
  # Arguments:
  # <dat>           a data frame with raw or pre-processed data
  # <features>      a character vector with the colnames of the features to include as predictors
  # <outcome>       a character vector with the colname of the outcome variable
  # <nm>            an optional integer with the amount of serial measurements to included as predictors
  # <mmn>           an optional integer with the maximum measurement number: the number of the most recent measurement to include
  # <multipred>     an optional flag for outputting two data frames for multi pont prediction
  
  # Debug
  dat <- dat_mod
  features <- xgb_ftrs
  outcome <- "ARAT"
  nm <- nm
  mmn <- mmn
  multipred <- F

  # initialize
  dat_mod <- list()

  # count & drop patients with less than mmn+1 observations
  patient_counts <- dat %>%
    group_by(Number) %>%
    summarise(Count = n())
  nPatients_drop <- sum(patient_counts$Count < mmn+1) 
  print(cat("Patients with less than", mmn+1,"observations:", nPatients_drop, "dropped. \n"))
  dat_filt <- dat %>%
    filter(Number %in% patient_counts$Number[patient_counts$Count >= mmn+1]) # patients should minimally have mmn+1 measurements
  
  # select the outcome var
  Y <- dat_filt %>%
    group_by(Number) %>%
    slice_max(Days,n=1) %>% # filter last obs.
    ungroup() %>% 
    filter(Days >= 152, Days <= 213) %>%  # outcome should be measured between 5 and 7 months
    select(Number,{{outcome}}) %>%  # select outcome var
    rename(Outcome = {{outcome}})
  
  # select nm obs. of the predictor features and pivot to wide format
  X <- dat_filt %>%
    group_by(Number) %>%
    slice_min(Measurement_nr,n=mmn) %>% # select first nm obs until mmn of each patient
    slice_max(Measurement_nr, n=nm) %>%
    arrange(Days, .by_group = T) %>% # re-arrange b/c slice somehow changes order...
    ungroup() %>%
    filter(Days <= 90) %>%  # drop obs. of 3 months or older
    select(all_of(xgb_ftrs),Measurement_nr) %>%  # select feature set
    pivot_wider( # pivot to wide and name according to measurement nr
      names_from = Measurement_nr,
      values_from = xgb_ftrs[xgb_ftrs != "Number"], 
      names_glue = "{.value}_{Measurement_nr}"
    )
  
  # merge repeated measurements with outcome
  dat_mod_6m <- merge(X, Y, by = "Number") %>% 
    arrange(Number)
  
  dat_mod[[1]] <- dat_mod_6m
  
  # create second data frame for prediction at 6 weeks
  if(multipred==T){
    #TODO built in check that excludes
    
    # select the outcome var
    Y <- dat_filt %>%
      group_by(Number) %>%
      slice_max(Days,n=1) %>% # filter last obs.
      ungroup() %>% 
      filter(Days >= 152, Days <= 213) %>%  # outcome should be measured between 5 and 7 months
      select(Number,ARAT) %>%  # select outcome var
      rename(last_ARAT = ARAT)
    
    # select nm obs. of the predictor features and pivot to wide format
    X <- dat_filt %>%
      group_by(Number) %>%
      slice_min(Measurement_nr,n=mmn) %>% # select first nm obs until mmn of each patient
      slice_max(Measurement_nr, n=nm) %>%
      arrange(Days, .by_group = T) %>% # re-arrange b/c slice somehow changes order...
      ungroup() %>%
      filter(Days <= 90) %>%  # drop obs. of 3 months or older
      select(all_of(xgb_ftrs),Measurement_nr) %>%  # select feature set
      pivot_wider( # pivot to wide and name according to measurement nr
        names_from = Measurement_nr,
        values_from = xgb_ftrs[xgb_ftrs != "Number"], 
        names_glue = "{.value}_{Measurement_nr}"
      )
    
    # merge repeated measurements with outcome
    dat_mod_6w <- merge(X, Y, by = "Number") %>% 
      arrange(Number)
    
    dat_mod[[2]] <- dat_mod_6w
  }
  
  return(dat_mod)
}