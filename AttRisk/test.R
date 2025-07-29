
library(shiny)
library(shinydashboard)
library(tidyverse)

# Define dashboard layout
ui <- dashboardPage(
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
        p("Version 1.0.0"),
        p("Authors:"),
        uiOutput("authors_text"),
        p("If you have any questions, feedback or suggestions, please contact: r.mcchrystal.1@research.gla.ac.uk")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Authors text
  output$authors_text <- renderUI({
    authors_path <- "text/authors.txt"
    if (file.exists(authors_path)) {
      imp_authors <- readLines(authors_path)
      tagList(lapply(imp_authors, function(x) tags$p(x)))
    } else {
      p("Authors could not be imported")
    }
  })
}

shinyApp(ui, server)