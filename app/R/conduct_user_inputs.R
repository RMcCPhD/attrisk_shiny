# Prepares user inputs for trial conduct.
# This contains both design features and participant characteristics in the
# refreshed attrisk model format.

tidy_inputs_conduct <- function(input) {
  eventReactive(input$predict_conduct, {
    bind_rows(
      attrisk_design_input_table(input, prefix = "conduct"),
      attrisk_conduct_characteristic_table(input)
    )
  })
}
