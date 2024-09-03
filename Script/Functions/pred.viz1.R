pred.viz1 <- function (cur_pat){
  # Plots scaled ystar density distributions on inside of ARAT recovery plot.
  # For grid plotting
  
  # Arguments:
  # <cur_pat>   an integer specifying the patient number
  
  # Requires:
  # <pred_xgb>  data frame with bootstrap prediction results (output predict.XGB)
  # <dat_plot>  long format data frame with all available measurements
  # <pred_list> a list with outputs of predict.XGB
  
  # extract/init data
  ystar <- pred_list[[2]]
  Tapp <- applied_at
  Tout <- 180 # day number for which the predictions are made
  alpha <- 0.2 # significance level of PIs
  width <- 500 # scales the distribution geom (step 2)
  
  # 0: filter data frames for current patient
  dat_c <- filter(dat_plot, Number == cur_pat)
  most_recent_obs <- dat_c %>%
    slice_max(Days, n=1)
  pred_c <- filter(pred_xgb, Number == cur_pat)
  ystar_c <- ystar[,as.character(cur_pat)]
  ublab <- round(pred_c$ub,0)
  lblab <- round(pred_c$lb,0)
  predlab <- round(pred_c$Predicted,0)
  
  # 1: plot available measurements
  p <- ggplot() + 
    #geom_line(data=dat_c, aes(x = Days, y = ARAT)) + 
    geom_point(data=dat_c, aes(x = Days, y = ARAT), col = "#440154", size = 2)
  
  # 2: plot sideways density distribution of ystar
  dens <- density(ystar_c, from = 0, to = 57)
  
  
  plot_dat <- data.frame( # helper df for plotting density distribution
    y = dens$y*width, 
    x = dens$x
  ) %>% 
    mutate(in_out = case_when(
      x > pred_c$ub ~ "higher",
      x < pred_c$lb ~ "lower",
      x>=pred_c$lb & x<=pred_c$ub ~ "in"
    ))
  
  p <- p + 
    geom_ribbon(data=subset(plot_dat,in_out=="higher"), aes(y=x, xmin=Tout, xmax=y+Tout, fill = "grey"), alpha=.5) +
    geom_ribbon(data=subset(plot_dat,in_out=="lower"), aes(y=x, xmin=Tout, xmax=y+Tout, fill = "grey"), alpha=.5) +
    geom_ribbon(data=subset(plot_dat,in_out=="in"), aes(y=x, xmin=Tout, xmax=y+Tout, fill = "#21918c"), alpha=.5) + 
    scale_fill_identity()
  
  # 3: add line for future recovery pathway
  plot_dat <- data.frame( # helper df for plotting future recovery pathway
    Days = c(Tapp, Tout),
    ypred = c(most_recent_obs$ARAT, pred_c$Predicted),
    yub = c(most_recent_obs$ARAT, pred_c$ub),
    ylb = c(most_recent_obs$ARAT, pred_c$lb)
  )
  
  p <- p +
    #geom_line(data=plot_dat, aes(x = Days, y = ypred), lty = 'dotted', lwd=1) +
    geom_ribbon(data=plot_dat, aes(ymax=yub, ymin=ylb, x=Days, fill = "#21918c"), alpha=.05)
  
  # 4: add point prediction + (custom) error bar
  adj <- as.numeric(abs(ublab - predlab) < 4 | abs(predlab - lblab) < 4)
  
  p <- p + 
    geom_point(data=pred_c, aes(x = Tout, y = Actual), shape=17, size = 2) +
    geom_point(data=pred_c, aes(x = Tout, y = Predicted), shape=13, size = 2) +
    geom_errorbar(data=pred_c, aes(x = Tout, ymin = lb, ymax = ub), width = 10, lwd = .5)
    # turn on for error bar labels
    # geom_text(aes(y=pred_c$ub, x=Tout+4, label = paste(ublab)), vjust = .5-adj, hjust = -.5+adj, size = 3) +
    # geom_text(aes(y=pred_c$lb, x=Tout+4, label = paste(lblab)), vjust = .5+adj, hjust = -.5+adj, size = 3) +
    # geom_text(aes(y=pred_c$Predicted, x=Tout+4, label = paste(predlab)), vjust = .5, hjust = -.5, size = 3, fontface = "bold")
  
  # 5: layout
  xbreaks <- c(0,60,120,180)
  ybreaks <- c(0,20,40,57)
  yexpand <- 6
  
  p <- p + 
    geom_vline(aes(xintercept = Tapp), lty = 'dashed', lwd = 0.5, col = "darkgrey") + # measurement | prediction demarcation
    labs(x = "Days since onset",
         y = "ARAT"
    ) +
    coord_cartesian(ylim = c(0, 57), xlim = c(0,210), clip="off") +
    scale_x_continuous(breaks = xbreaks) +
    scale_y_continuous(breaks = ybreaks, expand = expansion(add = c(yexpand, 0))) +
    fancy_multi
  
  return(p)
}
