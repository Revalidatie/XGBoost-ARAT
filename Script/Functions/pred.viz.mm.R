pred.viz.mm <- function (cur_pat){
  # Plots mm recovery pathway for grid plotting
  
  # Arguments:
  # <cur_pat>   an integer specifying the patient number
  
  # Requires:
  # <M_mm>        a model object containing the trained mixed effects model
  # <eval_mm>     a dataframe with the available measurement at the moment of model application (should contain cur_pat)
  # <applied_at>  an integer specifying the time in days when the model was applied
  
  # filter data frames for current patient
  cur_pat_dat <- eval_mm %>%
    filter(Number == cur_pat) 
  
  # extract/init data
  Tapp <- applied_at
  Tout <- 180 # day number for which the predictions are made
  predtimes <- seq(Tapp, Tout, length.out = 100) # time vector to provide predictions for
  alpha <- 0.2 # significance level of PIs
  
  # get predicted recovery profile with all measurements included
  Indv_pred_mm <- IndvPred_lme(lmeObject = M_mm, # model object
                               newdata = cur_pat_dat, # data to provide predictions for. Can be 1 or multiple patients with repeated measurements
                               timeVar = "Days", # name of time variable in observational data (newdata)
                               times = predtimes, # custom time variable for the predictions
                               all_times = FALSE, # logical to set if predictions should be made for observed timepoints as well (T=yes)
                               level = 0.80, # confidence interval level
                               M = 500, # ?
                               interval = "prediction", # ?
                               return_data = TRUE) # ?
  
  # create col 'type' to indicate which rows are observations and which are predictions
  Indv_pred_mm <- Indv_pred_mm %>%
    mutate(type = ifelse(is.na(low), 'obs', 'pred')) %>%  # obs. if conf. int. is missing
    mutate(pred = as.numeric(pred)) %>% 
    mutate(pred = if_else(pred < 0, 0, pred), # clip prediction and CI
           pred = if_else(pred > 57, 57, pred),
           upp = if_else(upp < 0, 0, upp),
           upp = if_else(upp > 57, 57, upp),
           low = if_else(low < 0, 0, low),
           low = if_else(low > 57, 57, low))
  ub <- Indv_pred_mm$upp[Indv_pred_mm$Days == Tout]
  lb <- Indv_pred_mm$low[Indv_pred_mm$Days == Tout]
  Predicted <- Indv_pred_mm$pred[Indv_pred_mm$Days == Tout]
  Actual <- Indv_pred_mm$Outcome[Indv_pred_mm$Days == Tout]
  ublab <- round(ub,0)
  lblab <- round(lb,0)
  predlab <- round(Predicted,0)
  
  # plot recovery profile
  p <- ggplot() +
        geom_line(data = filter(Indv_pred_mm, type == "pred"), aes(x = Days, y = pred)) +
        geom_point(data = filter(Indv_pred_mm, type == "obs"), aes(x = Days, y = ARAT), , col = "#404688", size = 2) +
        geom_ribbon(data = filter(Indv_pred_mm, type == "pred"), aes(x = Days, y = pred, ymin = low, ymax = upp), fill = "#21918c", alpha = 0.05)
  
  # plot error bar
  adj <- as.numeric(abs(ublab - predlab) < 4 | abs(predlab - lblab) < 4)
  
  p <- p + 
    geom_point(aes(x = Tout, y = Actual), shape=17, size = 2) +
    geom_point(aes(x = Tout, y = Predicted), shape=13, size = 2) +
    geom_errorbar(aes(x = Tout, ymin = lb, ymax = ub), width = 10, lwd = .5)
    # turn-on for error bar labels:
    # geom_text(aes(y=ub, x=Tout+4, label = paste(ublab)), vjust = .5-adj, hjust = -.5+adj, size = 3) +
    # geom_text(aes(y=lb, x=Tout+4, label = paste(lblab)), vjust = .5+adj, hjust = -.5+adj, size = 3) +
    # geom_text(aes(y=Predicted, x=Tout+4, label = paste(predlab)), vjust = .5, hjust = -.5, size = 3, fontface = "bold")
   
  # layout
  xbreaks <- c(0,60,120,180)
  ybreaks <- c(0,20,40,57)
  yexpand <- 6
  
  p <- p + geom_vline(aes(xintercept = Tapp), lty = 'dashed', lwd = 0.5, col = "darkgrey") + # measurement | prediction demarcation
        labs(x = "Days since onset",
             y = "ARAT"
        ) +
        coord_cartesian(ylim = c(0, 57), xlim = c(0,190), clip="off") +
        scale_x_continuous(breaks = xbreaks) +
        scale_y_continuous(breaks = ybreaks, expand = expansion(add = c(yexpand, 0))) +
        fancy_multi
  
  return(p)
}
