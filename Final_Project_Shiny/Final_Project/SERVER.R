#Chad Huntebrinker
#Server file
library(shiny)
library(plotly)
library(DT)

source("Team_Compare_Module.R")

server <- function(input, output, session) {
  mod_team_compare_server("teamcompare")
  
}