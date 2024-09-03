pred.viz <- function (cur_pat){
  # Interpretable visualisations of model output for clinicians and patients.
  
  # Requires:
  # <pred_xgb>  data frame with bootstrap prediction results (output predict.XGB)
  # <dat_plot>  long format data frame with all available measurements
  # <pred_list> a list with outputs of predict.XGB

  # extract/init data
  ystar <- pred_list[[2]]
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
    Days = c(applied_at, Tout),
    ypred = c(most_recent_obs$ARAT, pred_c$Predicted),
    yub = c(most_recent_obs$ARAT, pred_c$ub),
    ylb = c(most_recent_obs$ARAT, pred_c$lb)
  )
  
  p <- p +
    #geom_line(data=plot_dat, aes(x = Days, y = ypred), lty = 'dotted', lwd=1) +
    geom_ribbon(data=plot_dat, aes(ymax=yub, ymin=ylb, x=Days, fill = "#21918c"), alpha=.05)
  
  # 4: add point prediction + (custom) error bar
  adj <- as.numeric(abs(ublab - predlab) < 3 | abs(predlab - lblab) < 3)
  
  p <- p + 
    geom_point(data=pred_c, aes(x = Tout, y = Predicted), shape=13, size = 4) +
    geom_errorbar(data=pred_c, aes(x = Tout, ymin = lb, ymax = ub), width = 5, lwd = 1) +
    geom_text(aes(y=pred_c$ub, x=Tout+4, label = paste(ublab)), vjust = .5-adj, hjust = -.5+adj) +
    geom_text(aes(y=pred_c$lb, x=Tout+4, label = paste(lblab)), vjust = .5+adj, hjust = -.5+adj) +
    geom_text(aes(y=pred_c$Predicted, x=Tout+4, label = paste(predlab)), vjust = .5, hjust = -.5, fontface = "bold")
  
  # 5: layout
  xbreaks <- c(7,14,21,28,42,56,91,122,152,180)
  xlabs <- c("1","2","3","4","6","8","3","4","5","6")
  ybreaks <- c(0,10,20,30,40,50,57)
  
  yexpand <- 6
  
  p <- p + 
    annotate("richtext", x = 28, y = -(yexpand+15), label = "Weeks<br>after stroke", label.color = NA, vjust = 0, hjust = .5) + # x-axis labs
    annotate("richtext", x = 136, y = -(yexpand+15), label = "Months<br>after stroke", label.color = NA, vjust = 0, hjust = .5) +
    annotate("richtext", x = -25, y = 0, label = "**No**<br>movement", label.color = NA, vjust = 0.5, hjust = 1) + # y-axis labs
    annotate("richtext", x = -25, y = 57, label = "**Normal**<br>movement", label.color = NA, vjust = 0.5, hjust = 1) +
    geom_vline(aes(xintercept = applied_at), lty = 'dashed', lwd = 0.5, col = "darkgrey") + # measurement | prediction demarcation
    geom_point(aes(x=applied_at+3, y=-(yexpand-3)), shape="\u25BA", size=2, color="black") +
    annotate("richtext", x=applied_at+3, y=-(yexpand-3), label = "<span style='font-size:10pt'>*Prediction*</span>", label.color = NA, fill = NA, vjust = .5, hjust = 0,) +
    labs(title = "**Your <span style='color:#440154'>measured</span> and <span style='color:#21918c'>predicted</span> arm/hand function**", # titles
         subtitle = paste0("Your current arm/hand function is **", most_recent_obs$ARAT, "**. Based on similar patients, there is<br>
                           an 80% probability that your arm/hand function will fall **between ", lblab," and ", ublab, "**<br>
                           at 6 months."),
         x = "",
         y = ""
    ) +
    coord_cartesian(ylim = c(0, 57), xlim = c(0,210), clip = "off") +
    scale_x_continuous(breaks = xbreaks, labels = xlabs) +
    scale_y_continuous(breaks = ybreaks, expand = expansion(add = c(yexpand, 0))) +
    fancy +
    theme(plot.margin = margin(10, 10, 30, 60)) # increase plot margin to make space for annotations
  
  return(p)
}
