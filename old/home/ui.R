
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)

# Define dashboard layout
dashboardPage(
  dashboardHeader(title = "AttRisk"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs", selected = "home",
      menuItem("Home", tabName = "home"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    # CSS styling for user prompts
    tags$head(
      tags$style(HTML("
        .prompt-box {
          margin: 150px auto;
          max-width: 600px;git ad
          padding: 40px;
          border-radius: 20px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          text-align: center;
          background-color: #f9f9f9;
        }
        .prompt-box h3 {
          margin-bottom: 30px;
          font-weight: bold;
        }
        .prompt-btn {
          font-size: 18px;
          padding: 15px 30px;
          border-radius: 10px;
          margin: 10px;
        }
      "))
    ),
    
    tabItems(
      # Home page
      tabItem(
        tabName = "home", 
        h2(
          tags$b("AttRisk: An interface for predicting risk of attrition during trial design and conduct")
        ),
        div(
          class = "prompt-box",
          h3("What type of prediction would you like to make?"),
          actionButton(
            "design_pred", "Trial Design",
            class = "prompt-btn btn-primary",
            style = "font-weight: bold; color:white; font-size:150%",
            onclick = "window.location.href='/AttRisk_design/'"
          ),
          actionButton(
            "conduct_pred", "Trial Conduct",
            class = "prompt-btn btn-success",
            style = "font-weight: bold; color:white; font-size:150%",
            onclick = "window.location.href='/AttRisk_conduct/'"
          )
        )
      ),
      
      # About page
      tabItem(
        tabName = "about",
        h2(tags$b("About AttRisk")),
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
        )
      )
    )
  )
)
