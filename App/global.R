# libs
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggtext)
library(readxl)

# source functions
source("R/lookup.R")
source("R/genPlot.R")
source("R/check_data.R")

# load lookup table
load("R/XGBlookup.Rdata")
XGBlookup_pat <- XGBlookup[[1]]
XGBlookup_preds <- XGBlookup[[2]]
XGBlookup_ystar <- XGBlookup[[3]]
rm(XGBlookup)

# plotting theme
fancy = theme_light() + theme( 
  axis.title.x = element_text(size = 14, face = "bold"),
  axis.text.x = element_text(size = 11),
  axis.title.y = element_text(size = 14, face = "bold"),
  axis.text.y = element_text(size = 11),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line.x = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  axis.line.y = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  axis.ticks = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  plot.title = element_markdown(),
  plot.subtitle = element_markdown(),
  plot.tag = element_markdown())