#
# This is a Shiny web application for the XGBoost ARAT prediction model. 
# You can run the application by clicking the 'Run App' button above.
#
# Authors:      Govert van der Gun, adapted from Eleni-Rosalina Andrinopoulou
# Created:      24-07-2024

library(shiny)

source("App/global.R")

shinyApp(ui = ui, server = server) 