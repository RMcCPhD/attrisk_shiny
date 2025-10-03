
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)

# Source scripts
source("ui_scripts/body.R")

# Define dashboard layout
dashboardPage(
  dashboardHeader(title = "AttRisk"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Trial design", tabName = "predict_design"),
      menuItem("User guide", tabName = "guide")
    )
  ),
  design_body
)


