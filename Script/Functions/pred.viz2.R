pred.viz2 <- function (pred, dat, ystar, Tout = 180, alpha = 0.20){
  # Ystar histogram on outside of ARAT recovery plot.
  
  # Arguments:
  # <pred>      data frame with bootstrap prediction results (output predict.XGB)
  # <dat>       long format data frame with all available measurements
  # <ystar>     matrix with simulated future data points (output predict.XGB)
  # <Tout>      optional integer with day number for which the predictions are made
  # <alpha>     optional float with significance level of PIs
  
  # debug
  # ystar <- pred_list[[2]]
  # pred <- pred_xgb
  # dat <- dat_plot
  # alpha <- 0.2

  # extract data
  patients <- unique(dat$Number)
  
  # initialize
  psmooth <- list()
  phist <- list()
  plotlist <- list()
  
  for (cur_pat in patients){
    # 0: filter data frames for current patient
    dat_c <- filter(dat, Number == cur_pat)
    pred_c <- filter(pred,Number == cur_pat)
    ystar_c <- ystar[,as.character(cur_pat)]
    ub <- pred_c$ub
    lb <- pred_c$lb
    
    # 1: create plot with available measurements
    pl <- ggplot() + 
      geom_line(data=dat_c, aes(x = Days, y = ARAT, group=1)) +
      geom_point(data=dat_c, aes(x = Days, y = ARAT))
    
    # 2: add point prediction and error bar
    pl <- pl + 
      geom_point(data=pred_c, aes(x = Tout, y = Predicted),  shape=13, size = 4) +
      geom_errorbar(data=pred_c, aes(x = Tout, ymin = lb, ymax = ub), width = 5, lwd = 1)
    
    # 3: add dashed line for future recovery pathway
    latest_measurement <- dat_c %>%
      slice_max(Days, n=1)
    
    plot_dat <- data.frame( # helper df for plotting future recovery pathway
      Days = c(latest_measurement$Days, Tout),
      y = c(latest_measurement$ARAT, pred_c$Predicted)
    )
    
    pl <- pl + 
      geom_line(data=plot_dat, aes(x = Days, y = y), lty = 'dashed')+
      labs(title = paste0("Patient ", cur_pat),
           x = "Days since onset",
           y = "ARAT"
      ) +
      coord_cartesian(ylim = c(0, 60)) +
      #scale_x_continuous(breaks = seq(0,Tout,30), limits = c(0,Tout)) +
      fancy
    
    # 4: create plot with sideways histogram
    ph <- ggplot() +
      geom_histogram(binwidth = 1, 
                     aes(y=ystar_c,
                         x=after_stat(count)/sum(after_stat(count)),
                         fill = ifelse(ystar_c>=lb & ystar_c<=ub, "Inside", "Outside")
                     )
      ) +
      labs(title = " ",
           x = "Frequency") +
      scale_y_continuous(limits = c(0,60)) +
      scale_x_continuous(labels = function(x) gsub("^0", "", sprintf("%.2f", x))) +
      scale_fill_manual(values = c("Inside" = "#21918c", "Outside" = "grey")) +
      fancy +
      theme(legend.position = "none",          
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "grey")) 
    
    # 5: arrange plots
    suppressMessages( # surpress geom_line warning message when only on obs. is available
      plotlist[[cur_pat]] <- arrangeGrob(pl, ph, ncol=2, widths = c(4,1.5))
    )
  }
  
  return(plotlist)
}