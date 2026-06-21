# Conduct-stage design component.
#
# The conduct page first predicts attrition from trial design features, then the
# conduct calibration adjusts the log-rate for participant characteristics. This
# script prepares the pre-calibration Gompertz parameters.

conduct_design_params <- function(conduct_inputs) {
  reactive({
    input_param <- conduct_inputs()

    # Match the design page: use typical design-stage predictions for stability
    # in the app, rather than marginal predictions over a new trial random
    # effect and residual shape error.
    params <- attrisk_design_parameters(
      input_param,
      include_new_trial_error = FALSE
    )

    # Compute the design-only risk curve because downstream conduct summaries
    # still use the final design-stage dropout estimate as an internal quantity.
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
