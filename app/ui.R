
# Define dashboard layout
dashboardPage(
  dashboardHeader(title = tags$b("AttRisk T2DM")),
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
            "Example",
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
              includeHTML("www/mkdown/conduct_func.html")
            )
          ),
          tabPanel(
            "Example",
            div(
              style = "max-width: 1000px; font-size: 20px;",
              includeHTML("www/mkdown/conduct_egs.html")
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
                  "design_duration", "Duration (weeks)",
                  value = NA, min = 12, max = 520,
                ),
                numericInput(
                  "design_arms", "Number of arms",
                  value = NA, min = 2, max = 6,
                ),
                numericInput(
                  "design_n", "Number of participants",
                  value = NA, min = 100, max = 10000
                ),
                strong("Trial size"),
                textOutput("design_sample_size"),
                div(style = "height: 15px;"),
                radioButtons(
                  "design_phase", "Phase",
                  c("3", "4"),
                  inline = TRUE,
                  selected = "3"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "design_age_lower", "Lower age limits",
                  c("18yr", ">18yr"),
                  inline = TRUE,
                  selected = "18yr"
                ),
                radioButtons(
                  "design_bmi_lower", "Lower BMI limits",
                  c("None", "<=27 kg/m2"),
                  inline = TRUE,
                  selected = "None"
                ),
                radioButtons(
                  "design_egfr_lower", "Lower eGFR limits",
                  c("None", "<=30 ml/min/1.73m2", ">60 ml/min/1.73m2"),
                  inline = FALSE,
                  selected = "None"
                ),
                radioButtons(
                  "design_n_facilities", "Number of sites",
                  c("<50", "50 - 99", ">=100"),
                  inline = FALSE,
                  selected = "<50"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "design_age_upper", "Upper age limits*",
                  c("<=80yr", ">80yr"),
                  inline = TRUE,
                  selected = "<=80yr"
                ),
                radioButtons(
                  "design_bmi_upper", "Upper BMI limits",
                  c("None", ">30 kg/m2", ">40kg/m2"),
                  inline = TRUE,
                  selected = "None"
                ),
                radioButtons(
                  "design_poms", "Number of primary outcomes",
                  c("1", ">1"),
                  inline = TRUE,
                  selected = "1"
                ),
                checkboxGroupInput(
                  "design_trttype", "Comparators",
                  c("Placebo", "Active comparator"),
                  selected = "Placebo"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "design_blind", "Blinding type",
                  c("Open label", "Double", "Triple", "Quadruple"),
                  selected = "Open label"
                ),
                checkboxGroupInput(
                  "design_who_reg", "WHO regions involved",
                  c(
                    "African", "The Americas*",
                    "Eastern Mediterranean", "European",
                    "Southeast Asian", "Western Pacific"
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
                  "conduct_duration", "Duration (weeks)",
                  value = NA, min = 12, max = 520,
                ),
                numericInput(
                  "conduct_arms", "Number of arms",
                  value = NA, min = 2, max = 6,
                ),
                numericInput(
                  "conduct_n", "Number of participants",
                  value = NA, min = 100, max = 10000,
                ),
                strong("Trial size"),
                textOutput("conduct_sample_size"),
                div(style = "height: 15px;"),
                radioButtons(
                  "conduct_phase", "Phase",
                  c("3", "4"),
                  inline = TRUE,
                  selected = "3"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "conduct_age_lower", "Lower age limits",
                  c("18yr", ">18yr"),
                  inline = TRUE,
                  selected = "18yr"
                ),
                radioButtons(
                  "conduct_bmi_lower", "Lower BMI limits",
                  c("None", "<=27 kg/m2"),
                  inline = TRUE,
                  selected = "None"
                ),
                radioButtons(
                  "conduct_egfr_lower", "Lower eGFR limits",
                  c("None", "<=30 ml/min/1.73m2", ">60 ml/min/1.73m2"),
                  inline = FALSE,
                  selected = "None"
                ),
                radioButtons(
                  "conduct_n_facilities", "Number of sites",
                  c("<50", "50 - 99", ">=100"),
                  inline = FALSE,
                  selected = "<50"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "conduct_age_upper", "Upper age limits*",
                  c("<=80yr", ">80yr"),
                  inline = TRUE,
                  selected = "<=80yr"
                ),
                radioButtons(
                  "conduct_bmi_upper", "Upper BMI limits",
                  c("None", ">30 kg/m2", ">40kg/m2"),
                  inline = TRUE,
                  selected = "None"
                ),
                radioButtons(
                  "conduct_poms", "Number of primary outcomes",
                  c("1", ">1"),
                  inline = TRUE,
                  selected = "1"
                ),
                checkboxGroupInput(
                  "conduct_trttype", "Comparators",
                  c("Placebo", "Active comparator"),
                  selected = "Placebo"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "conduct_blind", "Blinding type",
                  c("Open label", "Double", "Triple", "Quadruple"),
                  selected = "Open label"
                ),
                checkboxGroupInput(
                  "conduct_who_reg", "WHO regions involved",
                  c(
                    "African", "The Americas*",
                    "Eastern Mediterranean", "European",
                    "SE Asian", "Western Pacific"
                  )
                ),
                div(style = "height: 4px;"),
                actionButton(
                  "reset_design2", "Enter duration", 
                  class = "btn-warning",
                  disabled = TRUE,
                  width = "160px",
                  style = "font-weight: bold; color:white; font-size:125%%"
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
                  "conduct_age", "Age (years)*",
                  value = NA, min = 18, max = 100,
                ),
                radioButtons(
                  "conduct_race", "Race",
                  c("Asian", "Black/African American*", "White", "Other"),
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
                  "conduct_sex", "Sex*",
                  c("Female", "Male"),
                  inline = TRUE,
                  selected = "Female"
                ),
                numericInput(
                  "conduct_t2dur", "T2DM duration (years)*",
                  value = NA, min = 0.01, max = 20,
                ),
                numericInput(
                  "conduct_bmi", "BMI (kg/m2)",
                  value = NA, min = 19, max = 50,
                ),
                numericInput(
                  "conduct_sbp", "Systolic BP (mmHg)*",
                  value = NA, min = 60, max = 140,
                )
              ),
              column(
                width = 3,
                radioButtons(
                  "conduct_smoke", "Current smoker*",
                  c("No", "Yes"),
                  inline = TRUE,
                  selected = "No"
                ),
                numericInput(
                  "conduct_hba1c", "HbA1c (%)*",
                  value = NA, min = 6, max = 10,
                ),
                numericInput(
                  "conduct_hdl", "HDL cholesterol (mmol/L)",
                  value = NA, min = 1, max = 3,
                ),
                numericInput(
                  "conduct_comorbs", "Comorbidities*",
                  value = NA, min = 0, max = 10,
                )
              ),
              column(
                width = 3,
                div(style = "height: 61px;"),
                numericInput(
                  "conduct_egfr", "eGFR (mmol/min/1.73m2)*",
                  value = NA, min = 15, max = 120,
                ),
                numericInput(
                  "conduct_totalc", "Total cholesterol (mmol/L)",
                  value = NA, min = 4, max = 10,
                ),
                numericInput(
                  "conduct_conmed", "Concomitant medications",
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