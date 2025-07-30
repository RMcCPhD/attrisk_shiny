
library(shiny)
library(shinydashboard)
library(tidyverse)

# Import placeholders, css etc.
# For readability this is kept separate to ui.R
js_placeholders <- source("www/js_placeholders.R")$value

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
      menuItem("Predict", tabName = "predict"),
      menuItem("Convert", tabName = "convert"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    
    js_placeholders,
    
    tabItems(
      tabItem(
        tabName = "home", 
        h2(tags$b("AttRisk: A tool for predicting risk of attrition during trial design and conduct"))
      ),
      
      tabItem(
        tabName = "guide",
        h2(tags$b("User guide"))
      ),
      
      tabItem(
        tabName = "predict",
        h2(tags$b("Predict attrition risk")),
        fluidRow(
          box(
            div(
              style = "max-width: 750px; font-size: 15px;",
              uiOutput("predict_intro")
            )
          )
        ),
        fluidRow(
          box(
            title = tags$b("Trial design features"),
            div(
              id = "design_box",
              column(
                width = 3,
                numericInput(
                  "duration", "Duration (weeks)",
                  value = NA, min = 2, max = 260,
                  width = "170px"
                ),
                numericInput(
                  "sample", "Sample size",
                  value = NA, min = 30, max = 20000,
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
                  "reset_design", "Reset design features", 
                  class = "btn-warning",
                  width = "170px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                )
              ),
              
              column(
                width = 3,
                radioButtons(
                  "age_lower", "Lower age limits",
                  c("18yr", ">18yr"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "age_upper", "Upper age limits",
                  c("70yr", ">70yr"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "bmi_lower", "Lower bmi limits",
                  c("None", ">=20 kg/m2"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "bmi_upper", "Upper bmi limits",
                  c("None", "<45 kg/m2"),
                  inline = TRUE,
                  selected = character(0)
                ),
                div(style = "height: 53px;"),
                actionButton(
                  "predict_design", "Predict (design)", 
                  class = "btn-success",
                  width = "170px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                )
              ),
              
              column(
                width = 3,
                checkboxGroupInput(
                  "who_reg", "WHO regions involved",
                  c(
                    "African region", "Regions of the Americas",
                    "Eastern Medi. region", "European region",
                    "SE Asian region", "Western Pacific region"
                  )
                ),
                radioButtons(
                  "poms", "Number of POMs",
                  c("1", ">1"),
                  inline = TRUE,
                  selected = character(0)
                )
              ),
              
              column(
                width = 3,
                checkboxGroupInput(
                  "trttype", "Comparators",
                  c("Placebo", "Active comparator")
                ),
                radioButtons(
                  "phase", "Phase",
                  c("3", "4"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "blind", "Blinding type",
                  c("Open label", "Double", "Triple", "Quadruple"),
                  selected = character(0)
                )
              )
            )
          ),
          
          box(
            title = tags$b("Participant baseline characteristics"),
            div(
              id = "conduct_box",
              column(
                width = 3,
                numericInput(
                  "mean_age", "Age (years)",
                  value = NA, min = 18, max = 150,
                  width = "170px"
                ),
                numericInput(
                  "mean_t2dur", "T2DM duration (years)",
                  value = NA, min = 0.01, max = 50,
                  width = "170px"
                ),
                numericInput(
                  "mean_hba1c", "HbA1c (%)",
                  value = NA, min = 6, max = 9,
                  width = "170px"
                ),
                numericInput(
                  "mean_hdl", "HDL cholesterol (mmol/L)",
                  value = NA, min = 1, max = 4,
                  width = "170px"
                ),
                actionButton(
                  "reset_conduct", "Reset characteristics", 
                  class = "btn-warning",
                  width = "170px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                )
              ),
              column(
                width = 3,
                numericInput(
                  "mean_bmi", "BMI (kg/m2)",
                  value = NA, min = 19, max = 50,
                  width = "170px"
                ),
                numericInput(
                  "mean_egfr", "eGFR (mmol/min/1.73m2)",
                  value = NA, min = 60, max = 120,
                  width = "170px"
                ),
                numericInput(
                  "mean_dbp", "Diastolic BP (mmHg)",
                  value = NA, min = 60, max = 80,
                  width = "170px"
                ),
                numericInput(
                  "mean_total", "Total cholesterol (mmol/L)",
                  value = NA, min = 1, max = 4,
                  width = "170px"
                ),
                actionButton(
                  "predict_design", "Predict (conduct)", 
                  class = "btn-success",
                  width = "170px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                )
              ),
              column(
                width = 3,
                numericInput(
                  "con_med", "Concomitant medications",
                  value = NA, min = 1, max = 20,
                  width = "170px"
                ),
                numericInput(
                  "comorbs", "Comorbidities",
                  value = NA, min = 1, max = 10,
                  width = "170px"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "sex", "Sex",
                  c("Female", "Male"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "smoke", "Current smoker",
                  c("Yes", "No"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "race", "Race",
                  c("Asian", "Black/African American", "White", "Other"),
                  selected = character(0)
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = tags$b("Attrition over time"),
            width = 6,
            tabBox(
              width = 2,
              tabPanel("Attrition rates"),
              tabPanel("Accumulated risk"),
              tabPanel("Accumulated incidence")
            )
          ),
          box(
            title = tags$b("Summary statistics"),
            width = 6,
            tabBox(
              width = 2,
              tabPanel("Attrition rates"),
              tabPanel("Accumulated risk"),
              tabPanel("Accumulated incidence")
            )
          )
        )
      ),
      
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
