# Conduct-stage calibration-in-the-large.
#
# This script adjusts the design-stage log-rate using participant baseline
# characteristics. The adjustment combines the exported calibration shift with
# the participant-level linear predictor from the pooled conduct model.

predict_conduct <- function(tidy_inputs_conduct, conduct_inputs) {
  reactive({
    tidy_out <- tidy_inputs_conduct()
    input_param <- conduct_inputs()

    # Keep only participant-level model rows, join their pooled log-HRs, and
    # treat missing optional covariates as zero contribution on the model scale.
    input_lp <- input_param %>%
      filter(.data$role == "model", .data$param %in% attrisk_conduct_terms) %>%
      transmute(
        param = .data$param,
        value_prep = suppressWarnings(as.numeric(.data$value_prep))
      ) %>%
      right_join(attrisk_conduct_loghrs, by = "param") %>%
      mutate(value_prep = replace_na(.data$value_prep, 0))
    
    lp <- input_lp %>%
      summarise(lp = sum(.data$value_prep * .data$logHR, na.rm = TRUE)) %>%
      pull(lp)

    # Apply the calibration shift and participant-level linear predictor to the
    # design-stage log-rate. Shape is inherited from the design-stage model.
    log_rate <- tidy_out$log_rate
    shape <- tidy_out$shape

    log_rate_citl <- log_rate + attrisk_delta_mu + lp

    # Conduct output does not currently plot instantaneous hazard, so only risk
    # and participant count summaries are returned.
    attrisk_prediction_table(
      log_rate = log_rate_citl,
      shape = shape,
      input_param = input_param,
      include_hazard = FALSE
    )
  })
}
