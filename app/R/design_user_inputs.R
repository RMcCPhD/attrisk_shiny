# Prepares user inputs for trial design.
# The current UI choices are translated into the refreshed attrisk model schema.

tidy_inputs <- function(input) {
  # Rebuild the design input table only when the user requests a prediction.
  # The returned table contains display rows for exports and model rows for the
  # posterior prediction helpers.
  eventReactive(input$predict_design, {
    attrisk_design_input_table(input, prefix = "design")
  })
}
