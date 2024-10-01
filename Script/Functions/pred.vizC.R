pred.vizC <- function (cur_pat){
  # Design C: Ystar histogram on outside of ARAT recovery plot.
  
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

  # 0: filter data frames for current patient
  dat_c <- filter(dat_plot, Number == cur_pat)
  most_recent_obs <- dat_c %>%
    slice_max(Days, n=1)
  pred_c <- filter(pred_xgb, Number == cur_pat)
  ystar_c <- ystar[,as.character(cur_pat)]
  ublab <- round(pred_c$ub,0)
  lblab <- round(pred_c$lb,0)
  predlab <- round(pred_c$Predicted,0)
    
  # 1: create plot with available measurements
  p <- ggplot() + 
    #geom_line(data=dat_c, aes(x = Days, y = ARAT)) + 
    geom_point(data=dat_c, aes(x = Days, y = ARAT), col = "#472f7d", size = 2)
  
  # 2: add future recovery pathway
  plot_dat <- data.frame( # helper df for plotting future recovery pathway
    Days = c(Tapp, Tout),
    ypred = c(most_recent_obs$ARAT, pred_c$Predicted),
    yub = c(most_recent_obs$ARAT, pred_c$ub),
    ylb = c(most_recent_obs$ARAT, pred_c$lb)
  )
  
  p <- p +
    #geom_line(data=plot_dat, aes(x = Days, y = ypred), lty = 'dotted', lwd=1) +
    geom_ribbon(data=plot_dat, aes(ymax=yub, ymin=ylb, x=Days), fill = "#21918c", alpha=.05)
  
  # 3: add point prediction and error bar
  adj <- as.numeric(abs(ublab - predlab) < 4 | abs(predlab - lblab) < 4)
  
  p <- p + 
  # turn on for actual prediction 
  # geom_point(data=pred_c, aes(x = Tout, y = Actual), shape=17, size = 2) +
    geom_point(data=pred_c, aes(x = Tout, y = Predicted), shape=13, size = 2) +
    geom_errorbar(data=pred_c, aes(x = Tout, ymin = lb, ymax = ub), width = 5, lwd = .5)
  # turn on for error bar labels
  # geom_text(aes(y=pred_c$ub, x=Tout+4, label = paste(ublab)), vjust = .5-adj, hjust = -.5+adj, size = 3) +
  # geom_text(aes(y=pred_c$lb, x=Tout+4, label = paste(lblab)), vjust = .5+adj, hjust = -.5+adj, size = 3) +
  # geom_text(aes(y=pred_c$Predicted, x=Tout+4, label = paste(predlab)), vjust = .5, hjust = -.5, size = 3, fontface = "bold")
  
  # 5: layout pred plot
  xbreaks <- c(0,60,120,180)
  ybreaks <- c(0,20,40,57)
  yexpand <- 6
  
  p <- p + 
    geom_vline(aes(xintercept = Tapp), lty = 'dashed', lwd = 0.5, col = "darkgrey") + # measurement | prediction demarcation
    labs(x = "Days since onset",
         y = "ARAT"
    ) +
    coord_cartesian(ylim = c(0, 57), xlim = c(0,180), clip="off") +
    scale_x_continuous(breaks = xbreaks) +
    scale_y_continuous(breaks = ybreaks, expand = expansion(add = c(yexpand, 0))) +
    fancy_multi +
    theme(plot.margin = margin(10, 10, 15, 1)) # increase plot margin to make space for annotations
  
  # 4: create plot with sideways histogram
  plot_dat <- data.frame(ystar_c = ystar_c, # helper df
                         bin_colour = if_else(ystar_c>=floor(pred_c$lb) & ystar_c<=ceil(pred_c$ub),"Inside","Outside")
                         )

  p2 <- ggplot(plot_dat, aes(y = ystar_c, fill = bin_colour)) +
    geom_histogram(binwidth = 1,
                   boundary = 0,
                   color = NA,
                   alpha = .5,
                   show.legend = F,
                   aes(x=after_stat(count)/sum(after_stat(count)))
                   ) +
    labs(title = " ",
         x = "Frequency") +
    coord_cartesian(ylim = c(0, 57), clip="off") +
    scale_y_continuous(breaks = ybreaks, expand = expansion(add = c(yexpand, 0))) +
    scale_x_continuous(labels = function(x) gsub("^0", "", sprintf("%.2f", x))) +
    scale_fill_manual(values = c("Inside" = "#21918c", "Outside" = "grey")) +
    fancy_multi +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "grey"),
          plot.margin = margin(4, 10, 15, 1))
  
  # 5: arrange plots
  p_arrange <- arrangeGrob(p, p2, ncol=2, widths = c(4,1.5))
  
  return(p_arrange)
}