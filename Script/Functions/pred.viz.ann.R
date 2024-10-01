pred.viz.ann <- function (cur_pat){
  # Interpretable, annotated visualisations of model output.
  
  # Arguments:
  # <cur_pat>   an integer specifying the patient number
  
  # Requires:
  # <pred_xgb>  data frame with bootstrap prediction results (output predict.XGB)

  # extract/init data
  Tout <- 180 # day number for which the predictions are made

  # 0: filter data frames for current patient
  dat_c <- filter(dat_plot, Number == cur_pat)
  most_recent_obs <- dat_c %>%
    slice_max(Days, n=1)
  pred_c <- filter(pred_xgb, Number == cur_pat)
  ublab <- round(pred_c$ub,0)
  lblab <- round(pred_c$lb,0)
  predlab <- round(pred_c$Predicted,0)
  
  # 1: call plot
  # currently only works for design A and B
  p <- pred.vizB(cur_pat)
  
  # 2: add in-plot labels
  adj <- as.numeric(abs(ublab - predlab) < 3 | abs(predlab - lblab) < 3)
  
  p <- p + 
    #geom_point(data=pred_c, aes(x = Tout, y = Predicted), shape=13, size = 4) +
    #geom_errorbar(data=pred_c, aes(x = Tout, ymin = lb, ymax = ub), width = 5, lwd = .5) +
    geom_text(aes(y=pred_c$ub, x=Tout+4, label = paste(ublab)), vjust = .5-adj, hjust = -.5+adj) +
    geom_text(aes(y=pred_c$lb, x=Tout+4, label = paste(lblab)), vjust = .5+adj, hjust = -.5+adj) +
    geom_text(aes(y=pred_c$Predicted, x=Tout+4, label = paste(predlab)), vjust = .5, hjust = -.5, fontface = "bold")
  
  # 3: add/replace layout
  xbreaks <- c(0,7,14,21,28,42,56,91,122,152,180)
  xlabs <- c("0","1","2","3","4","6","8","3","4","5","6")
  ybreaks <- c(0,20,40,57)
  
  yexpand <- 6
  
  
    p <-  p + 
    annotate("richtext", x = 28, y = -(yexpand+13), label = "Weeks<br>after stroke", label.color = NA, vjust = 0, hjust = .5) + # x-axis labs
    annotate("richtext", x = 136, y = -(yexpand+13), label = "Months<br>after stroke", label.color = NA, vjust = 0, hjust = .5) +
    annotate("richtext", x = -25, y = 0, label = "**No**<br>movement", label.color = NA, vjust = 0.5, hjust = 1) + # y-axis labs
    annotate("richtext", x = -25, y = 57, label = "**Normal**<br>movement", label.color = NA, vjust = 0.5, hjust = 1) +
    geom_vline(aes(xintercept = applied_at), lty = 'dashed', lwd = 0.5, col = "darkgrey") + # measurement | prediction demarcation
    geom_point(aes(x=applied_at+2, y=-(yexpand-3)), shape="\u25BA", size=2, color="black") +
    annotate("richtext", x=applied_at+3, y=-(yexpand-3), label = "<span style='font-size:10pt'>*Prediction*</span>", label.color = NA, fill = NA, vjust = .5, hjust = 0,) +
    labs(title = "**Your <span style='color:#404688'>measured</span> and <span style='color:#21918c'>predicted</span> arm/hand function**", # titles
         subtitle = paste0("Your current arm/hand function is **", most_recent_obs$ARAT, "**. Based on similar patients, there is<br>
                           an 80% probability that your arm/hand function will fall **between ", lblab," and ", ublab, "**<br>
                           at 6 months."),
         x = "",
         y = ""
    ) +
    coord_cartesian(ylim = c(0, 57), xlim = c(0,180), clip = "off") +
    scale_x_continuous(breaks = xbreaks, labels = xlabs) +
    scale_y_continuous(breaks = ybreaks, expand = expansion(add = c(yexpand, 0))) +
    fancy +
    theme(plot.margin = margin(10, 75, 30, 60)) # increase plot margin to make space for annotations
  
  return(p)
}
