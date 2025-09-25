
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
    
    # Add inner scroll box for summary statistics
    tags$head(tags$style(HTML("
      #design-row .box { display: flex; flex-direction: column; }
      #design-row .box .box-body { flex: 1 1 auto; overflow: auto; }"))),
    
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
                  "duration", "Duration (weeks)",
                  value = NA, min = 12, max = 260,
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
                  "reset_design", "Reset design features", 
                  class = "btn-warning",
                  width = "165px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                ),
                div(style = "height: 15px;"),
                actionButton(
                  "predict_design", "Predict risk", 
                  class = "btn-success",
                  width = "165px",
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
                  "bmi_lower", "Lower bmi limits",
                  c("None", "<=25 kg/m2"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "egfr_lower", "Lower eGFR limits",
                  c("None", "<=30 ml/min/1.73m2", ">30 ml/min/1.73m2"),
                  inline = FALSE,
                  selected = character(0)
                ),
                radioButtons(
                  "phase", "Phase",
                  c("3", "4"),
                  inline = TRUE,
                  selected = character(0)
                ),
                div(style = "height: 5px;"),
                checkboxInput(
                  "uncertainty", "Show uncertainty on plots",
                  NULL
                )
              ),
              
              column(
                width = 3,
                radioButtons(
                  "age_upper", "Upper age limits",
                  c("70yr", ">70yr"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "bmi_upper", "Upper bmi limits",
                  c("None", "<45 kg/m2"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "egfr_upper", "Upper eGFR limits",
                  c("None", "<=60 ml/min/1.73m2", ">60 ml/min/1.73m2"),
                  inline = FALSE,
                  selected = character(0)
                ),
                radioButtons(
                  "poms", "Number of POMs",
                  c("1", ">1"),
                  inline = TRUE,
                  selected = character(0)
                ),
                checkboxGroupInput(
                  "trttype", "Comparators",
                  c("Placebo", "Active comparator")
                )
              ),
              
              column(
                width = 3,
                radioButtons(
                  "blind", "Blinding type",
                  c("Open label", "Double", "Triple", "Quadruple"),
                  selected = character(0)
                ),
                checkboxGroupInput(
                  "who_reg", "WHO regions involved",
                  c(
                    "African region", "Regions of the Americas",
                    "Eastern Medi. region", "European region",
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
            plotOutput("plot_hazard_design")
          )
        ),
        fluidRow(
          # id = "design-row",
          # width = 12,
          # box(
          #   title = tags$b("Attrition risk over time"),
          #   width = 6,
          #   height = 462,
          #   plotOutput("plot_hazard_design", height = "100%")
          # ),
          box(
            title = tags$b("Summary statistics"),
            width = 6,
            height = 462,
            style = "height: 100%; overflow-y: auto",
            tabBox(
              width = 6,
              tabPanel("Risk overview", tableOutput("sum_design_risk")),
              tabPanel("Milestones", tableOutput("sum_design_mstones"))
            )
          )
          # box(
          #   title = tags$b("Summary statistics"),
          #   width = 6,
          #   height = 462,
          #   style = "height:100%; overflow-y: scroll",
          #   tableOutput("sum_design")
          # )
        )
      ),
      
      # Trial conduct ----------------------------------------------------------
      
      tabItem(
        tabName = "predict_conduct",
        h2(tags$b("Trial conduct: Predict attrition risk")),
        fluidRow(
          box(
            title = tags$b("Trial design features"),
            div(
              # id = "design_box",
              column(
                width = 3,
                numericInput(
                  "duration2", "Duration (weeks)",
                  value = NA, min = 12, max = 26,
                  width = "170px"
                ),
                numericInput(
                  "sample2", "Sample size",
                  value = NA, min = 30, max = 500,
                  width = "170px"
                ),
                numericInput(
                  "facilities2", "Number of sites",
                  value = NA, min = 1, max = 500,
                  width = "170px"
                ),
                numericInput(
                  "arms2", "Number of arms",
                  value = NA, min = 2, max = 6,
                  width = "170px"
                ),
                actionButton(
                  "reset_design2", "Reset design features", 
                  class = "btn-warning",
                  width = "170px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                )
              ),
              
              column(
                width = 3,
                radioButtons(
                  "age_lower2", "Lower age limits",
                  c("18yr", ">18yr"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "age_upper2", "Upper age limits",
                  c("70yr", ">70yr"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "bmi_lower2", "Lower bmi limits",
                  c("None", ">=20 kg/m2"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "bmi_upper2", "Upper bmi limits",
                  c("None", "<45 kg/m2"),
                  inline = TRUE,
                  selected = character(0)
                )
              ),
              
              column(
                width = 3,
                checkboxGroupInput(
                  "who_reg2", "WHO regions involved",
                  c(
                    "African region", "Regions of the Americas",
                    "Eastern Medi. region", "European region",
                    "SE Asian region", "Western Pacific region"
                  )
                ),
                radioButtons(
                  "poms2", "Number of POMs",
                  c("1", ">1"),
                  inline = TRUE,
                  selected = character(0)
                )
              ),
              
              column(
                width = 3,
                checkboxGroupInput(
                  "trttype2", "Comparators",
                  c("Placebo", "Active comparator")
                ),
                radioButtons(
                  "phase2", "Phase",
                  c("3", "4"),
                  inline = TRUE,
                  selected = character(0)
                ),
                radioButtons(
                  "blind2", "Blinding type",
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
                  value = NA, min = 18, max = 100,
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
                  "mean_sbp", "Systolic BP (mmHg)",
                  value = NA, min = 60, max = 140,
                  width = "170px"
                ),
                numericInput(
                  "mean_total", "Total cholesterol (mmol/L)",
                  value = NA, min = 1, max = 4,
                  width = "170px"
                ),
                actionButton(
                  "predict_conduct", "Predict (conduct)", 
                  class = "btn-success",
                  width = "170px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                )
              ),
              column(
                width = 3,
                numericInput(
                  "con_med", "Concomitant medications",
                  value = NA, min = 0, max = 20,
                  width = "170px"
                ),
                numericInput(
                  "comorbs", "Comorbidities",
                  value = NA, min = 0, max = 10,
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
            title = tags$b("Attrition rate"),
            width = 6,
            height = 400,
            plotOutput("plot_hazard_conduct", height = 400)
          ),
          box(
            title = tags$b("Summary statistics"),
            width = 6,
            height = 400,
            tabBox(
              width = 6,
              tabPanel("Risk overview", tableOutput("sum_design_risk")),
              tabPanel("Milestones", tableOutput("sum_design_mstones"))
            )
            # div(
            #   style = "height: 100%; overflow-y: auto",
            # )
          )
        )
      ),
      
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