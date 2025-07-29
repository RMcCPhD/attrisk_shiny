
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
    # Home page
    tabItems(
      tabItem(
        tabName = "home", 
        h2(tags$b("AttRisk: A tool for predicting risk of attrition during trial design and conduct")),
      ),
      
      # User guide
      tabItem(
        tabName = "guide",
        h2(tags$b("User guide"))
      ),
      
      # Trial design
      tabItem(
        tabName = "design",
        h2(tags$b("Trial design")),
        fluidRow(
          box(
            height = 344,
            width = 3,
            title = tags$b("Numeric design features"),
            column(
              width = 6,
              numericInput(
                "duration",
                "Trial duration (weeks)",
                value = 26,
                min = 2,
                max = 260,
                width = "180px"
              ),
              numericInput(
                "sample",
                "Sample size",
                value = 250,
                min = 30,
                max = 20000,
                width = "180px"
              ),
              numericInput(
                "facilities",
                "Number of sites",
                value = 50,
                min = 1,
                max = 500,
                width = "180px"
              )
            ),
            column(
              width = 6,
              numericInput(
                "arms",
                "Number of arms",
                value = 2,
                min = 2,
                max = 6,
                width = "180px"
              ),
              numericInput(
                "poms",
                "Number of POMs",
                value = 1,
                min = 1,
                max = 4,
                width = "180px"
              )
            )
          ),
          box(
            width = 5,
            title = tags$b("Categorical design features"),
            
            # POMs and WHO regions
            column(
              width = 4,
              checkboxGroupInput(
                "who_reg",
                "WHO regions involved",
                c(
                  "African region", "Regions of the Americas",
                  "Eastern Medi. region", "European region",
                  "SE Asian region", "Western Pacific region"
                ),
                inline = FALSE
              )
            ),
            
            # Eligibility criteria
            column(
              width = 4,
              checkboxGroupInput(
                "age_lower",
                "Lower age limits",
                c("18yr", ">18yr"),
                inline = TRUE
              ),
              checkboxGroupInput(
                "age_upper",
                "Upper age limits",
                c("70yr", ">70yr"),
                inline = TRUE
              ),
              checkboxGroupInput(
                "bmi_lower",
                "Lower bmi limits",
                c("None", ">=20 kg/m2"),
                inline = TRUE
              ),
              checkboxGroupInput(
                "bmi_upper",
                "Upper bmi limits",
                c("None", "<45 kg/m2"),
                inline = TRUE
              )
            ),
            
            # Other design features
            column(
              width = 4,
              checkboxGroupInput(
                "trttype",
                "Comparators",
                c("Placebo", "Active comparator"),
                inline = FALSE
              ),
              checkboxGroupInput(
                "phase",
                "Phase",
                c("3", "4"),
                inline = TRUE
              ),
              checkboxGroupInput(
                "blind",
                "Blinding type",
                c("Open label", "Double", "Triple", "Quadruple"),
                inline = FALSE
              )
            )
          )
        )
      ),
      
      # Trial conduct
      tabItem(
        tabName = "conduct",
        h2(tags$b("Trial conduct"))
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
