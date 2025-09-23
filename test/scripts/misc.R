
tabItems(
  tabItem(
    tabName = "predict_design",
    h2(tags$b("Predict attrition risk for trial design")),
    fluidPage(
      box(
        fluidRow(
          div(
            
          )
        )
      ),
      box(
        title = tags$b("Trial design features"),
        width = 12,
        div(
          id = "design_box",
          fluidRow(
            column(
              width = 3,
              numericInput(
                "duration", "Duration (weeks)",
                value = NA, min = 4, max = 260,
                width = "140px"
              ),
              numericInput(
                "sample", "Sample size",
                value = NA, min = 30, max = 20000,
                width = "140px"
              ),
              numericInput(
                "facilities", "Number of sites",
                value = NA, min = 1, max = 500,
                width = "140px"
              ),
              numericInput(
                "arms", "Number of arms",
                value = NA, min = 2, max = 6,
                width = "140px"
              ),
              checkboxGroupInput(
                "trttype", "Comparators",
                c("Placebo", "Active comparator")
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
              radioButtons(
                "egfr_lower", "Upper eGFR limits",
                c("None", "<45 kg/m2"),
                inline = TRUE,
                selected = character(0)
              ),
              radioButtons(
                "egfr_upper", "Lower eGFR limits",
                c("None", "<45 kg/m2"),
                inline = TRUE,
                selected = character(0)
              )
            ),
            column(
              width = 3,
              checkboxGroupInput(
                "who_reg", "WHO regions involved",
                c(
                  "African", "The Americas",
                  "Eastern Medi.", "European",
                  "SE Asian", "Western Pacific"
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
        )
      ),
      fluidRow(
        width = 12,
        # Action buttons
        box(
          title = tags$b("Command centre"),
          width = 4,
          column(
            width = 6,
            actionButton(
              "reset_design", "Reset",
              width = "140px",
              class = "btn-warning",
              style = "font-weight: bold; color:white; font-size:130%"
            )
          ),
          column(
            width = 6,
            actionButton(
              "predict_design", "Predict risk",
              width = "140px",
              class = "btn-success",
              style = "font-weight: bold; color:white; font-size:130%"
            )
          )
        )
      ),
      fluidRow(
        width = 12,
        box(
          title = tags$b("Risk prediction"),
          width = 12,
          tabBox(
            width = 12,
            tabPanel("Attrition rates", plotOutput("plot_hazard_design")),
            tabPanel("Summary statistics", tableOutput("sum_design"))
          )
        )
      )
    )
  ),
  
  # User guide
  tabItem(
    tabName = "guide",
    h2(tags$b("User guide"))
  )
)
)
