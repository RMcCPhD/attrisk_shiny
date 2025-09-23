
# Define layout of design dashboard
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
        box(width = 12, p("Text"))
      ),
      
      # User inputs
      box(
        title = tags$b("Trial design features"),
        width = 6,
        fluidRow(
          column(
            width = 3,
            numericInput(
              "duration", "Duration",
              value = NA, min = 4, max = 260,
              width = "140px"
            )
          ),
          column(
            width = 3,
            numericInput(
              "sample", "Sample size",
              value = NA, min = 30, max = 20000,
              width = "140px"
            )
          ),
          column(
            width = 3,
            numericInput(
              "facilities", "Number of sites",
              value = NA, min = 1, max = 500,
              width = "140px"
            )
          ),
          column(
            width = 3,
            numericInput(
              "arms", "Number of arms",
              value = NA, min = 2, max = 6,
              width = "140px"
            )
          )
        )
      )
    )
  )
)
      
#       fluidRow(
#         box(
#           title = tags$b("Trial design features"),
#           width = 6,
#           div(
#             id = "design_num",
#             numericInput(
#               "duration", "Duration (weeks)",
#               value = NA, min = 4, max = 260,
#               width = "140px"
#             ),
#             numericInput(
#               "sample", "Sample size",
#               value = NA, min = 30, max = 20000,
#               width = "140px"
#             ),
#             numericInput(
#               "facilities", "Number of sites",
#               value = NA, min = 1, max = 500,
#               width = "140px"
#             ),
#             numericInput(
#               "arms", "Number of arms",
#               value = NA, min = 2, max = 6,
#               width = "140px"
#             )
#           )
#         )
#       )
#     )
#   )
# )
      
#       # User inputs and command buttons
#       fluidRow(
#         box(
#           title = tags$b("Trial design features"),
#           width = 6,
#           div(
#             id = "design_box",
#             fluidRow(
#               column(
#                 width = 3,
# ,
#                 checkboxGroupInput(
#                   "trttype", "Comparators",
#                   c("Placebo", "Active comparator")
#                 )
#               ),
#               column(
#                 width = 3,
#                 radioButtons(
#                   "age_lower", "Lower age limits",
#                   c("18yr", ">18yr"),
#                   inline = TRUE,
#                   selected = character(0)
#                 ),
#                 radioButtons(
#                   "age_upper", "Upper age limits",
#                   c("70yr", ">70yr"),
#                   inline = TRUE,
#                   selected = character(0)
#                 ),
#                 radioButtons(
#                   "bmi_lower", "Lower bmi limits",
#                   c("None", ">=20 kg/m2"),
#                   inline = TRUE,
#                   selected = character(0)
#                 ),
#                 radioButtons(
#                   "bmi_upper", "Upper bmi limits",
#                   c("None", "<45 kg/m2"),
#                   inline = TRUE,
#                   selected = character(0)
#                 ),
#                 radioButtons(
#                   "egfr_lower", "Upper eGFR limits",
#                   c("None", "<45 kg/m2"),
#                   inline = TRUE,
#                   selected = character(0)
#                 ),
#                 radioButtons(
#                   "egfr_upper", "Lower eGFR limits",
#                   c("None", "<45 kg/m2"),
#                   inline = TRUE,
#                   selected = character(0)
#                 )
#               ),
#               column(
#                 width = 3,
#                 checkboxGroupInput(
#                   "who_reg", "WHO regions involved",
#                   c(
#                     "African", "The Americas",
#                     "Eastern Medi.", "European",
#                     "SE Asian", "Western Pacific"
#                   )
#                 ),
#                 radioButtons(
#                   "poms", "Number of POMs",
#                   c("1", ">1"),
#                   inline = TRUE,
#                   selected = character(0)
#                 )
#               ),
#               column(
#                 width = 3,
#                 radioButtons(
#                   "phase", "Phase",
#                   c("3", "4"),
#                   inline = TRUE,
#                   selected = character(0)
#                 ),
#                 radioButtons(
#                   "blind", "Blinding type",
#                   c("Open label", "Double", "Triple", "Quadruple"),
#                   selected = character(0)
#                 )
#               )
#             )
#           )
#         ),
#         box(
#           title = tags$b("Command centre"),
#           width = 4
#         )
#       ),
#       
#       # Risk prediction outputs
#       fluidRow(
#         box(
#           title = tags$b("Risk prediction"),
#           width = 8
#         ),
#         box(
#           title = tags$b("Summary statistics"),
#           width = 4
#         )
#       )
#     )
#   )
# )
  
