# Prepares user inputs for trial design.
# The current UI choices are translated into the refreshed attrisk model schema.

tidy_inputs <- function(input) {
  eventReactive(input$predict_design, {
    attrisk_design_input_table(input, prefix = "design")
  })
}
