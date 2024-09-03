# import misc libraries
library(tidyverse) # standard R data science package set (incl ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr and forcats)
library(pacman) # easy loading and installing of additional packages:
library(usethis) # workflow optimisation

# Data preprocessing
p_load(lubridate, pracma,haven)

# Statistics
p_load(stats, caret, lme4, rmcorr, Metrics, leaps, groupdata2, MLmetrics, xgboost, Hmisc, SHAPforxgboost)

# Data visualization
p_load(corrplot, viridis, naniar, gridExtra, DiagrammeR, reshape2, shapviz, grid, table1, ggtext)

# misc
p_load(tictoc, progress)

# plotting themes
fancy = theme_light() + theme( # standard
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
fancy_multi = theme_light() + theme( # for multi plots (grids)
  axis.title.x = element_text(size = 11),
  axis.text.x = element_text(size = 11),
  axis.title.y = element_text(size = 11),
  axis.text.y = element_text(size = 11),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line.x = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  axis.line.y = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  axis.ticks = element_line(linewidth = 0.3, linetype = "solid", colour = "darkgrey"),
  plot.title = element_markdown(),
  plot.subtitle = element_markdown(),
  plot.tag = element_markdown())