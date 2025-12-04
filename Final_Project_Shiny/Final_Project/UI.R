#Chad Huntebrinker
#UI file
library(shiny)
library(plotly)
library(DT)

source("Team_Compare_Module.R")

ui <- fluidPage(
  titlePanel("NBA Team Shot Comparison"),
  mod_team_compare_ui("teamcompare")
)