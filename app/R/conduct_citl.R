# Get calibrated log-rate adjusted for the participant risk profile.

predict_conduct <- function(tidy_inputs_conduct, conduct_inputs) {
  reactive({
    tidy_out <- tidy_inputs_conduct()
    input_param <- conduct_inputs()
    
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
    
    log_rate <- tidy_out$log_rate
    shape <- tidy_out$shape
    
    log_rate_citl <- log_rate + attrisk_delta_mu + lp

    attrisk_prediction_table(
      log_rate = log_rate_citl,
      shape = shape,
      input_param = input_param,
      include_hazard = FALSE
    )
  })
}
