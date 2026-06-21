
# Design-stage prediction wrapper.
#
# The server calls this after the user presses "Predict risk" on the trial
# design page. The heavy lifting lives in aaa_model_helpers.R: user inputs are
# already encoded to model covariates, then converted to Gompertz risk curves.
predict_design <- function(design_inputs) {
  reactive({
    input_param <- design_inputs()

    # Use typical predictions for the app interface: posterior parameter draws
    # are retained, but the new-trial random intercept and shape residual are
    # set to zero to avoid extremely wide proof-of-concept intervals.
    params <- attrisk_design_parameters(
      input_param,
      include_new_trial_error = FALSE
    )

    # Return a common prediction tibble with risk, hazard, and participant count
    # summaries over the requested follow-up period.
    attrisk_prediction_table(
      log_rate = params$log_rate,
      shape = params$shape,
      input_param = input_param,
      include_hazard = TRUE
    )
  })
}
