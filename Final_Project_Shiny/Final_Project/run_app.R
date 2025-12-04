#Chad Huntebrinker
#Use this app to run the data dashboard

library(shiny)
source("UI.R")
source("SERVER.R")

shinyApp(ui, server)