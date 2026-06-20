
# Prepares Gompertz predictions from refreshed attrisk design-stage outputs.
predict_design <- function(design_inputs) {
  reactive({
    input_param <- design_inputs()
    params <- attrisk_design_parameters(
      input_param,
      include_new_trial_error = TRUE
    )
    
    attrisk_prediction_table(
      log_rate = params$log_rate,
      shape = params$shape,
      input_param = input_param,
      include_hazard = TRUE
    )
  })
}
