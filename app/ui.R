
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)

# Define dashboard layout
dashboardPage(
  dashboardHeader(title = tags$b("AttRisk")),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs", selected = "guide",
      # User guide menu
      menuItem("User guide", tabName = "guide", icon = icon("book")),
      
      # Design menu
      menuItem(
        "Trial design", tabName = "predict_design", icon = icon("bolt"),
        menuSubItem("Go to page", "predict_design", icon = icon("square-caret-right")),
        checkboxInput("overlay_top5", "Show risk for 5 similar trials?", FALSE),
        conditionalPanel(
          condition = "input.overlay_top5",
          div(
            style = "margin:8px 0 12px 8px; padding:8px; border-left:3px solid #3c8dbc;",
            strong("Similar trials (NCT IDs)"),
            tags$div(style = "font-size: 15px; margin-top:6px;", uiOutput("top5_ids"))
          )
        ),
        checkboxInput("save_outputs", "Save outputs?", FALSE),
        conditionalPanel(
          condition = "input.save_outputs",
          checkboxGroupInput(
            "save_select", "Select outputs to save",
            c("Risk plot", "Instantaneous hazard plot", "Summary statistics")
          ),
          actionButton(
            "savebtn", "Save outputs", class = "btn-success",
            disabled = TRUE
          )
        )
      ),
      
      # Conduct menu
      menuItem(
        "Trial conduct", tabName = "predict_conduct", icon = icon("bolt"),
        menuSubItem("Go to page", "predict_conduct", icon = icon("square-caret-right")),
        checkboxInput("overlay_top5_2", "Show risk for 5 similar trials?", FALSE),
        conditionalPanel(
          condition = "input.overlay_top5_2",
          div(
            style = "margin:8px 0 12px 8px; padding:8px; border-left:3px solid #3c8dbc;",
            strong("Similar trials (NCT IDs)"),
            tags$div(style = "font-size: 12px; margin-top:6px;", uiOutput("conduct-top5_ids"))
          )
        ),
        checkboxInput("save_outputs2", "Save outputs?", FALSE),
        conditionalPanel(
          condition = "input.save_outputs2",
          checkboxGroupInput(
            "save_select2", "Select outputs to save",
            c("Risk plot", "Instantaneous hazard plot", "Summary statistics")
          ),
          actionButton(
            "savebtn2", "Save outputs", class = "btn-success",
            disabled = TRUE
          )
        )
      ),
      
      # About menu
      menuItem("About", tabName = "about", icon = icon("circle-question"))
    )
  ),
  dashboardBody(
    
    # Use shinyjs
    useShinyjs(),
    
    # Javascript placeholders (greyed-out text in input boxes)
    tags$head(
      tags$script(src = "style/input_mods_design.js"),
      tags$script(src = "style/input_mods_conduct.js")
    ),
    
    tabItems(

      # User guide -------------------------------------------------------------
      
      tabItem(
        tabName = "guide", 
        h2(tags$b("AttRisk: A tool for predicting risk of attrition during trial design and conduct"))
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
                  "design-duration", "Duration (weeks)",
                  value = NA, min = 12, max = 52,
                  width = "170px"
                ),
                numericInput(
                  "design-sample", "Sample size",
                  value = NA, min = 30, max = 1000,
                  width = "170px"
                ),
                numericInput(
                  "design-facilities", "Number of sites",
                  value = NA, min = 1, max = 500,
                  width = "170px"
                ),
                numericInput(
                  "design-arms", "Number of arms",
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
                    "African", "The Americas*",
                    "Eastern Mediterranean", "European*",
                    "SE Asian", "Western Pacific"
                  )
                )
              )
            )
          ),
          box(
            title = tags$b("Attrition risk over time"),
            width = 6,
            height = 462,
            plotOutput("plot_risk_design")
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
      
      tabItem(
        tabName = "predict_conduct",
        fluidRow(
          # Trial design features
          box(
            height = 462,
            title = tags$b("Trial design features"),
            div(
              id = "design2_box",
              column(
                width = 3,
                numericInput(
                  "conduct-duration", "Duration (weeks)",
                  value = NA, min = 12, max = 52,
                  width = "170px"
                ),
                numericInput(
                  "conduct-sample", "Sample size",
                  value = NA, min = 30, max = 1000,
                  width = "170px"
                ),
                numericInput(
                  "conduct-facilities", "Number of sites",
                  value = NA, min = 1, max = 500,
                  width = "170px"
                ),
                numericInput(
                  "conduct-arms", "Number of arms",
                  value = NA, min = 2, max = 6,
                  width = "170px"
                ),
                actionButton(
                  "reset_design2", "Enter duration", 
                  class = "btn-warning",
                  disabled = TRUE,
                  width = "160px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "conduct-age_lower", "Lower age limits",
                  c("18yr", ">18yr"),
                  inline = TRUE,
                  selected = "18yr"
                ),
                radioButtons(
                  "conduct-bmi_lower", "Lower bmi limits",
                  c("None", "<=25 kg/m2"),
                  inline = TRUE,
                  selected = "None"
                ),
                radioButtons(
                  "conduct-egfr_lower", "Lower eGFR limits",
                  c("None", "<=30 ml/min/1.73m2", ">30 ml/min/1.73m2"),
                  inline = FALSE,
                  selected = "None"
                ),
                radioButtons(
                  "conduct-phase", "Phase",
                  c("3", "4"),
                  inline = TRUE,
                  selected = "3"
                ),
                checkboxGroupInput(
                  "conduct-trttype", "Comparators",
                  c("Placebo", "Active comparator"),
                  selected = "Placebo"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "conduct-age_upper", "Upper age limits*",
                  c("70yr", ">70yr"),
                  inline = TRUE,
                  selected = "70yr"
                ),
                radioButtons(
                  "conduct-bmi_upper", "Upper bmi limits",
                  c("None", "<45 kg/m2"),
                  inline = TRUE,
                  selected = "None"
                ),
                radioButtons(
                  "conduct-egfr_upper", "Upper eGFR limits",
                  c("None", "<=60 ml/min/1.73m2", ">60 ml/min/1.73m2"),
                  inline = FALSE,
                  selected = "None"
                ),
                radioButtons(
                  "conduct-poms", "Number of POMs",
                  c("1", ">1"),
                  inline = TRUE,
                  selected = "1"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "conduct-blind", "Blinding type",
                  c("Open label", "Double", "Triple", "Quadruple"),
                  selected = "Open label"
                ),
                checkboxGroupInput(
                  "conduct-who_reg", "WHO regions involved",
                  c(
                    "African", "The Americas*",
                    "Eastern Mediterranean", "European*",
                    "SE Asian", "Western Pacific"
                  )
                )
              )
            )
          ),
          # Participant baseline characteristics
          box(
            height = 462,
            title = tags$b("Participant baseline characteristics"),
            div(
              id = "char_box",
              column(
                width = 3,
                numericInput(
                  "conduct-age", "Age (years)*",
                  value = NA, min = 18, max = 100,
                  width = "170px"
                ),
                numericInput(
                  "conduct-t2dur", "T2DM duration (years)*",
                  value = NA, min = 0.01, max = 50,
                  width = "170px"
                ),
                numericInput(
                  "conduct-hba1c", "HbA1c (%)*",
                  value = NA, min = 6, max = 9,
                  width = "170px"
                ),
                numericInput(
                  "conduct-hdl", "HDL cholesterol (mmol/L)",
                  value = NA, min = 1, max = 4,
                  width = "170px"
                ),
                actionButton(
                  "reset_char", "Enter duration", 
                  class = "btn-warning",
                  disabled = TRUE,
                  width = "160px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                ),
                div(style = "height: 15px;"),
                actionButton(
                  "predict_conduct", "Enter duration", 
                  class = "btn-success",
                  disabled = TRUE,
                  width = "160px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                )
              ),
              column(
                width = 3,
                numericInput(
                  "conduct-bmi", "BMI (kg/m2)",
                  value = NA, min = 19, max = 50,
                  width = "170px"
                ),
                numericInput(
                  "conduct-egfr", "eGFR (mmol/min/1.73m2)*",
                  value = NA, min = 60, max = 120,
                  width = "170px"
                ),
                numericInput(
                  "conduct-sbp", "Systolic BP (mmHg)*",
                  value = NA, min = 60, max = 140,
                  width = "170px"
                ),
                numericInput(
                  "conduct-totalc", "Total cholesterol (mmol/L)",
                  value = NA, min = 1, max = 4,
                  width = "170px"
                )
              ),
              column(
                width = 3,
                numericInput(
                  "conduct-conmed", "Concomitant medications",
                  value = NA, min = 0, max = 20,
                  width = "170px"
                ),
                numericInput(
                  "conduct-comorbs", "Comorbidities*",
                  value = NA, min = 0, max = 10,
                  width = "170px"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "conduct-sex", "Sex",
                  c("Female", "Male*"),
                  inline = TRUE,
                  selected = "Female"
                ),
                radioButtons(
                  "conduct-smoke", "Current smoker",
                  c("No", "Yes*"),
                  inline = TRUE,
                  selected = "No"
                ),
                radioButtons(
                  "conduct-race", "Race",
                  c("Asian*", "Black/African American*", "White", "Other"),
                  selected = "White"
                ),
                radioButtons(
                  "conduct-trttype", "Treatment group",
                  c("Placebo", "Active comparator"),
                  selected = "Placebo"
                )
              )
            )
          )
        ),
        fluidRow(
          width = 12,
          box(
            title = tags$b("Summary statistics"),
            width = 6,
            height = 462,
            div(style = "height: 380px;", DT::DTOutput("sum_conduct"))
          ),
          box(
            title = tags$b("Attrition risk over time"),
            width = 6,
            height = 462,
            plotOutput("plot_risk_conduct")
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