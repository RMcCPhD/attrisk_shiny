
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
    
    tags$head(
      tags$script(HTML(
        "    
        $(document).on('shiny:connected', function(){
          $('#mean_age').attr('placeholder', 'Mean (min: 18, max: 150)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_bmi').attr('placeholder', 'Mean (min: 18, max: 50)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_dbp').attr('placeholder', 'Mean (min: 60, max: 80)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_hdl').attr('placeholder', 'Mean (min: 1, max: 4)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_t2dur').attr('placeholder', 'Mean (min: 0.01, max: 50)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_hba1c').attr('placeholder', 'Mean (min: 6, max: 9)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_egfr').attr('placeholder', 'Mean (min: 60, max: 120)');
        });
        "
      ))
    ),
    
    tabItems(
      tabItem(
        tabName = "home", 
        div(
          style = "max-width: 750px; font-size: 15px;",
          uiOutput("home_title")
        ),
      ),
      
      tabItem(
        tabName = "guide",
        h2(tags$b("User guide"))
      ),
      
      tabItem(
        tabName = "design",
        h2(tags$b("Trial design")),
        
        fluidRow(
          box(
            title = tags$b("Select from the following design features"),
            column(
              width = 3,
              numericInput(
                "duration", "Duration (weeks)",
                value = NA, min = 2, max = 260,
                width = "125px"
              ),
              numericInput(
                "sample", "Sample size",
                value = NA, min = 30, max = 20000,
                width = "125px"
              ),
              numericInput(
                "facilities", "Number of sites",
                value = NA, min = 1, max = 500,
                width = "125px"
              ),
              numericInput(
                "arms", "Number of arms",
                value = NA, min = 2, max = 6,
                width = "125px"
              )
            ),
            
            column(
              width = 3,
              radioButtons(
                "age_lower", "Lower age limits",
                c("18yr", ">18yr"),
                inline = TRUE
              ),
              radioButtons(
                "age_upper", "Upper age limits",
                c("70yr", ">70yr"),
                inline = TRUE
              ),
              radioButtons(
                "bmi_lower", "Lower bmi limits",
                c("None", ">=20 kg/m2"),
                inline = TRUE
              ),
              radioButtons(
                "bmi_upper", "Upper bmi limits",
                c("None", "<45 kg/m2"),
                inline = TRUE
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
                ),
                inline = FALSE
              ),
              radioButtons(
                "poms", "Number of POMs",
                c("1", ">1"),
                inline = TRUE
              )
            ),
            
            column(
              width = 3,
              checkboxGroupInput(
                "trttype", "Comparators",
                c("Placebo", "Active comparator"),
                inline = FALSE
              ),
              radioButtons(
                "phase", "Phase",
                c("3", "4"),
                inline = TRUE
              ),
              radioButtons(
                "blind", "Blinding type",
                c("Open label", "Double", "Triple", "Quadruple"),
                inline = FALSE
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = tags$b("Visualise attrition over time"),
            tabBox(
              width = 12,
              tabPanel(
                tags$b("Attrition rates")
              ),
              tabPanel(
                tags$b("Accumulated attrition risk")
              ),
              tabPanel(
                tags$b("Accumulated attrition occurrence")
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "conduct",
        h2(tags$b("Trial conduct")),
        fluidRow(
          box(
            title = tags$b("Select from the following design features"),
            column(
              width = 3,
              numericInput(
                "duration", "Duration (weeks)",
                value = NA, min = 2, max = 260,
                width = "125px"
              ),
              numericInput(
                "sample", "Sample size",
                value = NA, min = 30, max = 20000,
                width = "125px"
              ),
              numericInput(
                "facilities", "Number of sites",
                value = NA, min = 1, max = 500,
                width = "125px"
              ),
              numericInput(
                "arms", "Number of arms",
                value = NA, min = 2, max = 6,
                width = "125px"
              )
            ),
            
            column(
              width = 3,
              radioButtons(
                "age_lower", "Lower age limits",
                c("18yr", ">18yr"),
                inline = TRUE
              ),
              radioButtons(
                "age_upper", "Upper age limits",
                c("70yr", ">70yr"),
                inline = TRUE
              ),
              radioButtons(
                "bmi_lower", "Lower bmi limits",
                c("None", ">=20 kg/m2"),
                inline = TRUE
              ),
              radioButtons(
                "bmi_upper", "Upper bmi limits",
                c("None", "<45 kg/m2"),
                inline = TRUE
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
                ),
                inline = FALSE
              ),
              radioButtons(
                "poms", "Number of POMs",
                c("1", ">1"),
                inline = TRUE
              )
            ),
            
            column(
              width = 3,
              checkboxGroupInput(
                "trttype", "Comparators",
                c("Placebo", "Active comparator"),
                inline = FALSE
              ),
              radioButtons(
                "phase", "Phase",
                c("3", "4"),
                inline = TRUE
              ),
              radioButtons(
                "blind", "Blinding type",
                c("Open label", "Double", "Triple", "Quadruple"),
                inline = FALSE
              )
            )
          ),
          
          box(
            title = tags$b("Select from the following baseline participant characteristics"),
            column(
              width = 4,
              numericInput(
                "mean_age", "Age (years)",
                value = NA, min = 18, max = 150,
                width = "200px"
              ),
              numericInput(
                "mean_bmi", "BMI (kg/m2)",
                value = NA, min = 19, max = 50,
                width = "200px"
              ),
              numericInput(
                "mean_dbp", "Diastolic BP (mmHg)",
                value = NA, min = 60, max = 80,
                width = "200px"
              ),
              numericInput(
                "mean_hdl", "HDL cholesterol (%)",
                value = NA, min = 1, max = 4,
                width = "200px"
              )
            ),
            column(
              width = 4,
              numericInput(
                "mean_t2dur", "T2DM duration (years)",
                value = NA, min = 0.01, max = 50,
                width = "200px"
              ),
              numericInput(
                "mean_hba1c", "HbA1c (%)",
                value = NA, min = 6, max = 9,
                width = "200px"
              ),
              numericInput(
                "mean_egfr", "eGFR (mmol/min/1.73m2)",
                value = NA, min = 60, max = 120,
                width = "200px"
              )
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
