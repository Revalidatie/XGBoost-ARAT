pred.viz3 <- function (pred, dat, ystar, Tout = 180, alpha = 0.20, width = 500){
  # Scaled ystar density distributions on inside of ARAT recovery plot.
  
  # Arguments:
  # <pred>      data frame with bootstrap prediction results (output predict.XGB)
  # <dat>       long format data frame with all available measurements
  # <ystar>     matrix with simulated future data points (output predict.XGB)
  # <Tout>      optional integer with day number for which the predictions are made
  # <alpha>     optional float with significance level of PIs
  # <width>     optional integer that scales the distribution plot
  
  # debug
  # ystar <- pred_list[[2]]
  # pred <- pred_xgb
  # dat <- dat_plot
  # alpha <- 0.2
  # width <- 500
  
  # extract data
  patients <- unique(dat$Number)
  
  # initialize
  plotlist <- list()
  
  for (cur_pat in patients) {
    # 0: filter data frames for current patient
    dat_c <- filter(dat, Number == cur_pat)
    pred_c <- filter(pred, Number == cur_pat)
    ystar_c <- ystar[,as.character(cur_pat)]
    ub <- pred_c$ub
    lb <- pred_c$lb
    
    # 1: plot available measurements
    p <- ggplot() + 
      geom_line(data=dat_c, aes(x = Days, y = ARAT)) + 
      geom_point(data=dat_c, aes(x = Days, y = ARAT))
    
    # 2: plot sideways density distribution of ystar
    dens <- density(ystar_c)
    
    plot_dat <- data.frame( # helper df for plotting density distribution
      y = dens$y*width, 
      x = dens$x
    ) %>% 
      mutate(in_out = case_when(
        x > ub ~ "higher",
        x < lb ~ "lower",
        x>=lb & x<=ub ~ "in"
      ))
    
    p <- p + 
      geom_ribbon(data=subset(plot_dat,in_out=="higher"), aes(y=x, xmin=Tout, xmax=Tout+y, fill = "grey")) +
      geom_ribbon(data=subset(plot_dat,in_out=="lower"), aes(y=x, xmin=Tout, xmax=Tout+y, fill = "grey")) +
      geom_ribbon(data=subset(plot_dat,in_out=="in"), aes(y=x, xmin=Tout, xmax=Tout+y, fill = "#21918c")) + 
      scale_fill_identity()
    
    # 3: add point prediction + error bar
    p <- p + 
      geom_point(data=pred_c, aes(x = Tout, y = Predicted),  shape=13, size = 4) +
      geom_errorbar(data=pred_c, aes(x = Tout, ymin = lb, ymax = ub), width = 5, lwd = 1)
    
    # 4: add dashed line for future recovery pathway
    latest_measurement <- dat_c %>%
      slice_max(Days, n=1)
    
    plot_dat <- data.frame( # helper df for plotting future recovery pathway
      Days = c(latest_measurement$Days, Tout),
      y = c(latest_measurement$ARAT, pred_c$Predicted)
    )
    
    p <- p + 
      geom_line(data=plot_dat, aes(x = Days, y = y), lty = 'dashed') +
      labs(title = paste0("Patient ", cur_pat),
           x = "Days since onset",
           y = "ARAT"
      ) +
      coord_cartesian(ylim = c(0, 60)) +
      fancy
    
    plotlist[[cur_pat]] <- p
  }
  
  return(plotlist)
}
