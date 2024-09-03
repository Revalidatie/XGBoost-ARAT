box.plot.jitter <- function (dataframe, ID = NULL, vars = NULL, y_lim = NULL, return_outlrs = T)
  # this function creates box plots with jittered outliers for columns in <dataframe>
  # Arguments:
  # <dataframe>       a data frame with the variables of interest
  # <ID>              an optional name of the column that contains the ID of the groups. If nothing is specified the function assumes no grouping
  # <y_lim>           an optional vector with fix y limits for the plots
  # <vars>            an optional vector with the column names of the variables to plot
  # <return_outlrs>   an optional logical; 'TRUE' means that the outliers will be returned in the plot list "plots[[var]]$outliers"
  
{
  if (is.null(vars)) {
    vars <- colnames(select(dataframe,-all_of(ID)))
  }
  
  # convert to long format
  datlong <- melt(dataframe, id.vars = ID, measure.vars = vars)
  
  # preallocate list to store plots
  plots <- list()
  
  for (var in vars) { # cycle through variables
    
    # filter only current var & create custom outlier col
    curvar <- datlong[datlong$variable == var, ] 
    
    # outlier thresholds
    Q1 <- quantile(curvar$value,0.25)
    Q3 <- quantile(curvar$value,0.75)
    IQR <- Q3 - Q1
    
    # store outliers separately
    outliers <- curvar %>% 
      mutate(outlier = value < (Q1 - IQR*1.5) | 
               value > (Q3 + IQR*1.5)) %>% 
      filter(outlier == TRUE) %>% 
      select(Number,value)
    
    p <- ggplot(curvar, aes(x = var, y = value)) +
        geom_boxplot(outlier.shape = NA) +  # plot boxplot without outliers
        geom_jitter(data = data.frame(value = outliers$value),width = 0.1) +
        labs(
          x = "",
          y = var
        ) +
        scale_y_continuous(limits = y_lim) +
        fancy
    
    # add boxplot to list
    plots[[var]] <- p
    
    if (isTRUE(return_outlrs)) {
      plots[[var]]$outliers <- outliers
    }
    
  }
  
  return(plots)
}