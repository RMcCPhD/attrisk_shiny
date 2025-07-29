
library(shiny)
library(shinydashboard)
library(tidyverse)

# Define dashboard layout
dashboardPage(
  dashboardHeader(
    title = "AttRisk"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs", selected = "home",
      menuItem("Home", tabName = "home"),
      menuItem("User guide", tabName = "guide"),
      menuItem("Trial design", tabName = "design"),
      menuItem("Trial conduct", tabName = "conduct"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home", 
        h2("AttRisk: A tool for predicting risk of attrition during trial design and conduct"),
        
      ),
      tabItem(
        tabName = "guide",
        h2("User guide")
      ),
      tabItem(
        tabName = "design",
        h2("Trial design")
      ),
      tabItem(
        tabName = "conduct",
        h2("Trial conduct")
      ),
      tabItem(
        tabName = "about",
        h2("About AttRisk"),
        div(
          style = "max-width: 750px; font-size: 15px;",
          uiOutput("about_attrisk")
        ),
        tags$hr(),
        div(
          style = "max-width: 750px; font-weight: bold; font-size: 15px;",
          p("Authors and collaborators:")
        ),
        div(
          style = "max-width: 750px; font-size: 15px;",
          uiOutput("authors_text")
        ),
        tags$hr(),
        div(
          style = "max-width: 750px; font-size: 15px;",
          uiOutput("fback_sugg")
        ),
        tags$hr(),
        div(
          style = "max-width: 750px; font-weight: bold; font-size: 15px;",
          p("Version 1.0.0")
        ),
      )
    )
  )
)
