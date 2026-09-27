
# Conduct-stage design component
conduct_design_params <- function(conduct_inputs) {
  reactive({
    input_param <- conduct_inputs()

    params <- attrisk_design_parameters(
      input_param,
      include_new_trial_error = FALSE
    )

    design_risk <- attrisk_prediction_table(
      log_rate = params$log_rate,
      shape = params$shape,
      input_param = input_param,
      include_hazard = FALSE
    )
    
    list(
      log_rate = params$log_rate,
      shape = params$shape,
      drp = dplyr::last(design_risk$drp_q50)
    )
  })
}
