
# Prepares user inputs for trial conduct
tidy_inputs_conduct <- function(input) {
  eventReactive(input$predict_conduct, {
    bind_rows(
      attrisk_design_input_table(input, prefix = "conduct"),
      attrisk_conduct_characteristic_table(input)
    )
  })
}
