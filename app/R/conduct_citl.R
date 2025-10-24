
# Get calibrated log-rate (adjusted for participant risk profile)
predict_conduct <- function(tidy_inputs_conduct, conduct_inputs) {
  reactive({
    
    # Gather inputs from previous reactive functions
    tidy_out <- tidy_inputs_conduct()
    input_param <- conduct_inputs()
    
    # Gather inputs necessary for CITL
    imp_deltas <- readRDS("www/data/delta_draws.rds") # Delta draws
    imp_hr <- readRDS("www/data/sobol_lst.rds")       # Sobol data (i.e. logHR)
    
    # Get posterior mean for delta
    delta_mu <- rowMeans(imp_deltas)
    
    # Create characteristics map and logHR tibble
    char_nms <- tibble(param = colnames(imp_hr$Z))
    logHRs <- tibble(param = names(imp_hr$loghr), logHR = imp_hr$loghr)
    
    # Build linear predictor using user characteristic inputs
    input_lp <- char_nms %>%
      inner_join(select(input_param, param, value_prep)) %>%
      mutate(value_prep = as.numeric(value_prep)) %>%
      left_join(logHRs)
    
    lp <- input_lp %>% 
      mutate(lp = value_prep * logHR) %>%
      reframe(lp = sum(lp)) %>%
      pull(lp)
    
    # Get duration
    input_dur <- input_param %>% filter(param == "duration") %>% pull(value_prep)
    input_dur <- as.numeric(input_dur)
    
    # Build new log-rate and rate per draw
    log_rate <- tidy_out$log_rate                    # Log-rate (uncalibrated)
    log_rate_citl <- log_rate + delta_mu + lp        # Log-rate (calibrated)
    rate_citl <- exp(log_rate_citl)                  # Rate (calibrated)
    shape <- tidy_out$shape                          # Shape (unchanged)

    # Create time grid (in weeks)
    t_max_yrs <- input_dur
    t_yrs <- seq(0, t_max_yrs, length.out = 53)
    t_wks <- t_yrs * 52

    # Cumulative distribution (i.e. risk)
    phaz <- vapply(
      t_yrs,
      function(tt) flexsurv::pgompertz(tt, shape = shape, rate = rate_citl),
      numeric(length(rate_citl))
    )

    # Time-to-event
    t2e <- 1 - phaz

    # Quantiles
    rq <- qfun(phaz)
    t2eq <- qfun(t2e)

    # Counts (if inputted by user)
    N_input <- filter(input_param, param == "est_sample") %>% pull(value_prep)
    N <- as.numeric(N_input)

    # Completer/Dropout estimates (using estimated sample)
    cCount <- N * phaz
    atRisk <- N * t2e
    ccq <- qfun(cCount)
    arq <- qfun(atRisk)

    # Summarise in tibble
    sum_tbl <- tibble(
      t_yrs = t_yrs, t_wks = t_wks,
      compl_q50 = arq$md, drp_q50 = N - compl_q50,
      risk_q025 = rq$lo, risk_q50 = rq$md, risk_q975 = rq$hi
    )
    print(sum_tbl)
  })
}