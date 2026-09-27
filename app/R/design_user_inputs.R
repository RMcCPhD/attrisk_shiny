
# Prepares user inputs for trial design
tidy_inputs <- function(input) {
  eventReactive(input$predict_design, {
    attrisk_design_input_table(input, prefix = "design")
  })
}
