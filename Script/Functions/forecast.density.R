forecast.density <- function (PI_list,alpha){
  # Plot forecast densities for bootstrap prediction intervals. These plots can be 
  # interpreted as probability density functions of possible outcomes. Accepts 
  # list with PIs.
  
  # Arguments:
  # <PI_list>     a list with percentiles and future data points for 1 to n patients
  # <alpha>       the significance level of the PI supplied

  # debug
  # PI_list <- PI80_list
  # p <- 366
  # alpha <- 0.2
  
  # extract variables
  percentiles <- PI_list[[1]]
  patients <- rownames(percentiles)
  ystars <- PI_list[[2]]
  colnames(ystars) <- patients
  PIlevel <- (1-alpha)*100
  
  patients <- as.numeric(patients)
  
  # initialize
  psmooth <- list()
  phist <- list()
  plotlist <- list()
  
  for (p in patients){
    # current patient
    pchar <- as.character(p)
    ystar <- data.frame(last_ARAT = ystars[,pchar])
    ub <- percentiles[pchar,"ub"]
    lb <- percentiles[pchar,"lb"]
    
    # get density subset for credible interval
    density <- data.frame(x = density(ystar$last_ARAT)$x, y = density(ystar$last_ARAT)$y)
    density  <- subset(density, x <= 57 & x >= 0) # cut of tails due to KDE smoothing
    PIdensity <- subset(density, x>=lb & x<=ub)
    aPIdensity <- subset(density, !(x>=lb & x<=ub))
    
    # Density forecast / probability distribution of possible target values
    psmooth[[p]] <- ggplot(ystar, aes(x=last_ARAT)) +
      geom_density(alpha = 0.5) +
      geom_ribbon(data=PIdensity,
                  aes(x=x,ymax=y),ymin=0,fill="#3b528b", alpha=0.7) +
      geom_ribbon(data=aPIdensity,
                  aes(x=x,ymax=y),ymin=0,fill="grey", alpha=0.7) +
      labs(x = "ARAT at 6 months",
           y = "Probability Density") +
      fancy
    
    phist[[p]] <- ggplot(ystar, aes(y=last_ARAT)) +
      geom_histogram(binwidth = 1, 
                     aes(x = after_stat(count)/sum(after_stat(count)), 
                         fill = ifelse(last_ARAT>=lb & last_ARAT<=ub, "Inside", "Outside"))
                     ) +
      geom_hline(yintercept = 46, linetype = "dashed", linewidth = 1) +
      labs(y = "ARAT at 6 months",
           x = "Frequency") +
      scale_y_continuous(limits = c(0,60)) +
      scale_fill_manual(values = c("Inside" = "#5ec962", "Outside" = "grey"), name = paste0(PIlevel, "% Prediction Interval")) +
      fancy
    }
  
  plotlist[["Smooth"]] <- psmooth
  plotlist[["Histo"]] <- phist
  return(plotlist)
}