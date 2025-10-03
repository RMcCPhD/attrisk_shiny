
# Empty layout
design_body <- dashboardBody(
  
  useShinyjs(),
  
  # Javascript placeholders and css customisation
  tags$head(
    tags$script(src = "mods/input_mods.js")
  ),
  
  tabItems(
    tabItem(
      tabName = "predict_design",
      h2(tags$b("Predict attrition risk for trial design")),
      
      # Intro text
      fluidRow(
        box(
          width = 12,
          p("Text")
        )
      ),
      
      # User inputs and command buttons
      fluidRow(
        box(
          title = tags$b("Trial design features"),
          width = 8
        ),
        box(
          title = tags$b("Command centre"),
          width = 4
        )
      ),
      
      # Risk prediction outputs
      fluidRow(
        box(
          title = tags$b("Risk prediction"),
          width = 8
        ),
        box(
          title = tags$b("Summary statistics"),
          width = 4
        )
      )
    )
  )
)