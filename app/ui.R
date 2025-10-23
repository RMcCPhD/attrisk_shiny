
# Define dashboard layout
dashboardPage(
  dashboardHeader(title = tags$b("AttRisk")),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs", selected = "home",
      
      # Home page --------------------------------------------------------------
      
      menuItem(
        "Home", tabName = "home", icon = icon("home")
      ),
      
      # User guide menu --------------------------------------------------------
      
      menuItem(
        "User guide", tabName = "guide", icon = icon("book"),
        # Guides
        menuSubItem("Trial design", "guide_design", icon = icon("square-caret-right")),
        menuSubItem("Trial conduct", "guide_conduct", icon = icon("square-caret-right"))
      ),
      
      # Design menu ------------------------------------------------------------
      
      menuItem(
        "Trial design", tabName = "predict_design", icon = icon("bolt"),
        # Go to page
        menuSubItem("Go to page", "predict_design", icon = icon("square-caret-right")),
        # User input: Show 5 trials from model set most similar to their trial
        checkboxInput("overlay_top5", "Show risk for 5 similar trials?", FALSE),
        # Conditional inputs that appear if checkbox selected
        conditionalPanel(
          condition = "input.overlay_top5",
          div(
            style = "margin:8px 0 12px 8px; padding:8px; border-left:3px solid #3c8dbc;",
            strong("Similar trials (NCT IDs) - risk"),
            tags$div(style = "font-size: 15px; margin-top:6px;", uiOutput("top5_ids"))
          )
        ),
        # User input: Save outputs (plots and/or summary)
        checkboxInput("save_outputs", "Save outputs?", FALSE),
        # Conditional inputs to select outputs to save
        conditionalPanel(
          condition = "input.save_outputs",
          checkboxGroupInput(
            "save_select", "Select outputs to save",
            c("Risk plot", "Instantaneous hazard plot", "Summary statistics")
          ),
          # Button to initiate save outputs
          actionButton(
            "savebtn", "Save outputs", class = "btn-success",
            disabled = TRUE
          )
        )
      ),
      
      # Conduct menu -----------------------------------------------------------
      
      menuItem(
        "Trial conduct", tabName = "predict_conduct", icon = icon("bolt"),
        menuSubItem("Go to page", "predict_conduct", icon = icon("square-caret-right")),
        checkboxInput("save_outputs2", "Save outputs?", FALSE),
        conditionalPanel(
          condition = "input.save_outputs2",
          checkboxGroupInput(
            "save_select2", "Select outputs to save",
            c("Risk plot", "Summary statistics")
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
    
    # Style sources (placeholders for grey-text, browser size)
    tags$head(
      tags$script(src = "style/input_mods.js"),
      tags$link(rel = "stylesheet", href = "style/tidy_boxes.css")
    ),
    
    tabItems(

      # Home page --------------------------------------------------------------
      
      tabItem(
        tabName = "home", 
        h2(tags$b("Welcome to AttRisk")),
        div(
          style = "max-width: 1000px; font-size: 20px;",
          includeMarkdown("www/mkdown/home.Rmd")
        )
      ),
      
      # User guide -------------------------------------------------------------
      
      # Design
      tabItem(
        tabName = "guide_design",
        h2(tags$b("User guide: Trial design")),
        tabBox(
          id = "design_guide_tabs",
          width = 1000,
          tabPanel(
            "Overview",
            div(
              style = "max-width: 1000px; font-size: 20px;",
              includeHTML("www/mkdown/design_overview.html")
            )
          ),
          tabPanel(
            "Functionality",
            div(
              style = "max-width: 1000px; font-size: 20px;",
              includeHTML("www/mkdown/design_func.html")
            )
          ),
          tabPanel(
            "Example use case",
            div(
              style = "max-width: 1000px; font-size: 20px;",
              includeHTML("www/mkdown/design_egs.html")
            )
          )
        )
      ),
      
      # Conduct
      tabItem(
        tabName = "guide_conduct",
        h2(tags$b("User guide: Trial conduct")),
        tabBox(
          id = "conduct_guide_tabs",
          width = 1000,
          tabPanel(
            "Overview",
            div(
              style = "max-width: 1000px; font-size: 20px;",
              includeHTML("www/mkdown/conduct_overview.html")
            )
          ),
          tabPanel(
            "Functionality",
            div(
              style = "max-width: 1000px; font-size: 20px;",
              includeHTML("www/mkdown/design_func.html")
            )
          ),
          tabPanel(
            "Example use case",
            div(
              style = "max-width: 1000px; font-size: 20px;",
              includeHTML("www/mkdown/design_egs.html")
            )
          )
        )
      ),
      
      # Trial design -----------------------------------------------------------
      
      tabItem(
        tabName = "predict_design",
        fluidRow(
          box(
            height = 470,
            title = tags$b("Trial design features"),
            div(
              id = "design_box",
              class = "box-scroller",
              column(
                width = 3,
                numericInput(
                  "design-duration", "Duration (weeks)",
                  value = NA, min = 12, max = 520,
                ),
                numericInput(
                  "design-arms", "Number of arms",
                  value = 2, min = 2, max = 6,
                ),
                radioButtons(
                  "design-sample_size", "Sample size",
                  c("Small (<500)", "Medium (500 - 999)", "Large (>=1000)"),
                  inline = FALSE,
                  selected = "Small (<500)"
                ),
                numericInput(
                  "design-est_sample", "Estimated sample size",
                  value = NA,
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "design-age_lower", "Lower age limits",
                  c("18yr", ">18yr"),
                  inline = TRUE,
                  selected = "18yr"
                ),
                radioButtons(
                  "design-bmi_lower", "Lower bmi limits",
                  c("None", "<=25 kg/m2"),
                  inline = TRUE,
                  selected = "None"
                ),
                radioButtons(
                  "design-egfr_lower", "Lower eGFR limits",
                  c("None", "<=30 ml/min/1.73m2", ">30 ml/min/1.73m2"),
                  inline = FALSE,
                  selected = "None"
                ),
                radioButtons(
                  "design-phase", "Phase",
                  c("3", "4"),
                  inline = TRUE,
                  selected = "3"
                ),
                radioButtons(
                  "design-n_facilities", "Number of sites",
                  c("<50", "50 - 99", ">=100"),
                  inline = FALSE,
                  selected = "<50"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "design-age_upper", "Upper age limits*",
                  c("70yr", ">70yr"),
                  inline = TRUE,
                  selected = "70yr"
                ),
                radioButtons(
                  "design-bmi_upper", "Upper bmi limits",
                  c("None", "<45 kg/m2"),
                  inline = TRUE,
                  selected = "None"
                ),
                radioButtons(
                  "design-egfr_upper", "Upper eGFR limits",
                  c("None", "<=60 ml/min/1.73m2", ">60 ml/min/1.73m2"),
                  inline = FALSE,
                  selected = "None"
                ),
                radioButtons(
                  "design-poms", "Number of POMs",
                  c("1", ">1"),
                  inline = TRUE,
                  selected = "1"
                ),
                checkboxGroupInput(
                  "design-trttype", "Comparators",
                  c("Placebo", "Active comparator"),
                  selected = "Placebo"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "design-blind", "Blinding type",
                  c("Open label", "Double", "Triple", "Quadruple"),
                  selected = "Open label"
                ),
                checkboxGroupInput(
                  "design-who_reg", "WHO regions involved",
                  c(
                    "African", "The Americas*",
                    "Eastern Mediterranean", "European",
                    "SE Asian", "Western Pacific"
                  )
                ),
                actionButton(
                  "reset_design", "Enter duration", 
                  class = "btn-danger",
                  disabled = TRUE,
                  width = "160px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                ),
                div(style = "height: 10px;"),
                actionButton(
                  "predict_design", "Enter duration", 
                  class = "btn-success",
                  disabled = TRUE,
                  width = "160px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                )
              )
            )
          ),
          box(
            title = tags$b("Attrition risk over time"),
            height = 470,
            width = 6,
            withSpinner(plotOutput("plot_risk_design"), type = 6, color = "#3c8dbc")
          )
        ),
        fluidRow(
          class = "row-freeze row-lock",
          width = 12,
          box(
            title = tags$b("Summary statistics"),
            width = 6,
            height = 470,
            withSpinner(DT::DTOutput("sum_design"), type = 6, color = "#3c8dbc")
          ),
          box(
            title = tags$b("Instantaneous attrition hazard (at each timepoint)"),
            height = 470,
            width = 6,
            withSpinner(plotOutput("plot_hazard_design"), type = 6, color = "#3c8dbc")
          )
        )
      ),
      
      # Trial conduct ----------------------------------------------------------
      
      tabItem(
        tabName = "predict_conduct",
        fluidRow(
          # Trial design features
          box(
            height = 470,
            title = tags$b("Trial design features"),
            div(
              id = "design2_box",
              class = "box-scroller",
              column(
                width = 3,
                numericInput(
                  "conduct-duration", "Duration (weeks)",
                  value = NA, min = 12, max = 520,
                ),
                numericInput(
                  "conduct-arms", "Number of arms",
                  value = 2, min = 2, max = 6,
                ),
                radioButtons(
                  "conduct-sample_size", "Sample size",
                  c("Small (<500)", "Medium (500 - 999)", "Large (>=1000)"),
                  inline = FALSE,
                  selected = "Small (<500)"
                ),
                numericInput(
                  "conduct-est_sample", "Estimated sample size",
                  value = NA,
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
                radioButtons(
                  "conduct-n_facilities", "Number of sites",
                  c("<50", "50 - 99", ">=100"),
                  inline = FALSE,
                  selected = ""
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
                  "conduct-blind", "Blinding type",
                  c("Open label", "Double", "Triple", "Quadruple"),
                  selected = "Open label"
                ),
                checkboxGroupInput(
                  "conduct-who_reg", "WHO regions involved",
                  c(
                    "African", "The Americas*",
                    "Eastern Mediterranean", "European",
                    "SE Asian", "Western Pacific"
                  )
                )
              )
            )
          ),
          # Participant baseline characteristics
          box(
            title = tags$b("Participant baseline characteristics"),
            height = 470,
            div(
              id = "char_box",
              class = "box-scroller",
              column(
                width = 3,
                numericInput(
                  "conduct-age", "Age (years)*",
                  value = NA, min = 18, max = 100,
                ),
                radioButtons(
                  "conduct-race", "Race",
                  c("Asian*", "Black/African American*", "White", "Other"),
                  selected = "White"
                ),
                div(style = "height: 15px;"),
                actionButton(
                  "reset_char", "Enter duration", 
                  class = "btn-warning",
                  disabled = TRUE,
                  width = "160px",
                  style = "font-weight: bold; color:white; font-size:125%%"
                ),
                div(style = "height: 15px;"),
                actionButton(
                  "reset_conduct", "Enter duration", 
                  class = "btn-danger",
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
                ),
                div(style = "height: 45px;")
              ),
              column(
                width = 3,
                radioButtons(
                  "conduct-sex", "Sex*",
                  c("Female", "Male"),
                  inline = TRUE,
                  selected = "Female"
                ),
                numericInput(
                  "conduct-t2dur", "T2DM duration (years)*",
                  value = NA, min = 0.01, max = 20,
                ),
                numericInput(
                  "conduct-bmi", "BMI (kg/m2)",
                  value = NA, min = 19, max = 50,
                ),
                numericInput(
                  "conduct-sbp", "Systolic BP (mmHg)*",
                  value = NA, min = 60, max = 140,
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "conduct-smoke", "Current smoker*",
                  c("No", "Yes"),
                  inline = TRUE,
                  selected = "No"
                ),
                numericInput(
                  "conduct-hba1c", "HbA1c (%)*",
                  value = NA, min = 6, max = 10,
                ),
                numericInput(
                  "conduct-hdl", "HDL cholesterol (mmol/L)",
                  value = NA, min = 1, max = 3,
                ),
                numericInput(
                  "conduct-comorbs", "Comorbidities*",
                  value = NA, min = 0, max = 10,
                )
              ),
              column(
                width = 3,
                div(style = "height: 61px;"),
                numericInput(
                  "conduct-egfr", "eGFR (mmol/min/1.73m2)*",
                  value = NA, min = 60, max = 120,
                ),
                numericInput(
                  "conduct-totalc", "Total cholesterol (mmol/L)",
                  value = NA, min = 4, max = 10,
                ),
                numericInput(
                  "conduct-conmed", "Concomitant medications",
                  value = NA, min = 0, max = 20,
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
            height = 470,
            withSpinner(DT::DTOutput("sum_conduct"), type = 6, color = "#3c8dbc")
          ),
          box(
            title = tags$b("Attrition risk over time"),
            height = 470,
            width = 6,
            withSpinner(plotOutput("plot_risk_conduct"), type = 6, color = "#3c8dbc")
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