
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)

# Define dashboard layout
dashboardPage(
  dashboardHeader(title = tags$b("AttRisk")),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs", selected = "home",
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("User guide", tabName = "guide", icon = icon("book")),
      menuItem("Trial design", tabName = "predict_design", icon = icon("bolt")),
      menuItem("Trial conduct", tabName = "predict_conduct", icon = icon("bolt")),
      menuItem("About", tabName = "about", icon = icon("circle-question"))
    )
  ),
  dashboardBody(
    
    # Use shinyjs
    useShinyjs(),
    
    # Javascript placeholders (greyed-out text in input boxes)
    tags$head(tags$script(src = "style/input_mods.js")),

    tabItems(

      # Home -------------------------------------------------------------------
      
      tabItem(
        tabName = "home", 
        h2(tags$b("AttRisk: A tool for predicting risk of attrition during trial design and conduct"))
      ),
      
      # User guide -------------------------------------------------------------
      
      tabItem(
        tabName = "guide",
        h2(tags$b("User guide"))
      ),
      
      # Trial design -----------------------------------------------------------
      
      tabItem(
        tabName = "predict_design",
        fluidRow(
          box(
            height = 462,
            title = tags$b("Trial design features"),
            div(
              id = "design_box",
              column(
                width = 3,
                numericInput(
                  "duration", "Duration (weeks)*",
                  value = NA, min = 12, max = 52,
                  width = "170px"
                ),
                numericInput(
                  "sample", "Sample size",
                  value = NA, min = 30, max = 1000,
                  width = "170px"
                ),
                numericInput(
                  "facilities", "Number of sites",
                  value = NA, min = 1, max = 500,
                  width = "170px"
                ),
                numericInput(
                  "arms", "Number of arms",
                  value = NA, min = 2, max = 6,
                  width = "170px"
                ),
                actionButton(
                  "reset_design", "Enter duration", 
                  class = "btn-warning",
                  disabled = TRUE,
                  width = "160px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                ),
                div(style = "height: 15px;"),
                actionButton(
                  "predict_design", "Enter duration", 
                  class = "btn-success",
                  disabled = TRUE,
                  width = "160px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                )
              ),
              
              column(
                width = 3,
                radioButtons(
                  "age_lower", "Lower age limits",
                  c("18yr", ">18yr"),
                  inline = TRUE,
                  selected = "18yr"
                ),
                radioButtons(
                  "bmi_lower", "Lower bmi limits",
                  c("None", "<=25 kg/m2"),
                  inline = TRUE,
                  selected = "None"
                ),
                radioButtons(
                  "egfr_lower", "Lower eGFR limits",
                  c("None", "<=30 ml/min/1.73m2", ">30 ml/min/1.73m2"),
                  inline = FALSE,
                  selected = "None"
                ),
                radioButtons(
                  "phase", "Phase",
                  c("3", "4"),
                  inline = TRUE,
                  selected = "3"
                ),
                checkboxGroupInput(
                  "trttype", "Comparators",
                  c("Placebo", "Active comparator"),
                  selected = "Placebo"
                )
              ),
              
              column(
                width = 3,
                radioButtons(
                  "age_upper", "Upper age limits*",
                  c("70yr", ">70yr"),
                  inline = TRUE,
                  selected = "70yr"
                ),
                radioButtons(
                  "bmi_upper", "Upper bmi limits",
                  c("None", "<45 kg/m2"),
                  inline = TRUE,
                  selected = "None"
                ),
                radioButtons(
                  "egfr_upper", "Upper eGFR limits",
                  c("None", "<=60 ml/min/1.73m2", ">60 ml/min/1.73m2"),
                  inline = FALSE,
                  selected = "None"
                ),
                radioButtons(
                  "poms", "Number of POMs",
                  c("1", ">1"),
                  inline = TRUE,
                  selected = "1"
                )
              ),
              
              column(
                width = 3,
                radioButtons(
                  "blind", "Blinding type",
                  c("Open label", "Double", "Triple", "Quadruple"),
                  selected = "Open label"
                ),
                checkboxGroupInput(
                  "who_reg", "WHO regions involved",
                  c(
                    "African region", "Regions of the Americas*",
                    "Eastern Medi. region", "European region*",
                    "SE Asian region", "Western Pacific region"
                  )
                )
              )
            )
          ),
          box(
            title = tags$b("Attrition risk over time"),
            width = 6,
            height = 462,
            plotOutput("plot_cind_design")
          )
        ),
        fluidRow(
          width = 12,
          box(
            title = tags$b("Summary statistics"),
            width = 6,
            height = 462,
            div(style = "height: 380px;", DT::DTOutput("sum_design"))
          ),
          box(
            title = tags$b("Instantaneous attrition hazard (at each timepoint)"),
            width = 6,
            height = 462,
            plotOutput("plot_hazard_design")
          )
        )
      ),
      
      # Trial conduct ----------------------------------------------------------
      
      # About ------------------------------------------------------------------
      
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