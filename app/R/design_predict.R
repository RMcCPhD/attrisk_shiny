
# Prepares Gompertz parameters according to model specifications
# Estimates hazard, cumulative hazard and cumulative incidence using
# Gompertz parameters and scale of user-inputted time

predict_design <- function(design_inputs) {
  reactive({
    
    input_betas <- design_inputs()
    
    # Estimate rate ------------------------------------------------------------
    
    # Import summarised posterior draws
    a_imp_pos <- readRDS("www/data/sum_draws_shiny.rds")
    
    # Compute rate linear predictor
    rate_lp <- as.matrix(input_betas[,-1])
    rate_lp_mean <- sum(rate_lp[, "value"] * rate_lp[, "mean_param"])
    rate_lp_q2.5 <- sum(rate_lp[, "value"] * rate_lp[, "q2.5_param"])
    rate_lp_q97.5 <- sum(rate_lp[, "value"] * rate_lp[, "q97.5_param"])
    
    # Estimate rate
    est_rate <- a_imp_pos %>% 
      filter(param == "mu1") %>% 
      mutate(
        log_rate_mean = mean_param + rate_lp_mean[[1]],
        log_rate_q2.5 = mean_param + rate_lp_q2.5[[1]],
        log_rate_q97.5 = mean_param + rate_lp_q97.5[[1]],
        rate_mean = exp(log_rate_mean),
        rate_q2.5 = exp(log_rate_q2.5),
        rate_q97.5 = exp(log_rate_q97.5)
      ) %>% 
      pivot_longer(-param)
    
    # Linear predictor for shape -----------------------------------------------
    
    # Extract log-rate
    ext_lograte <- est_rate %>% 
      filter(grepl("log", name)) %>% 
      select(-param) %>% 
      rename(param = name)
    
    # Get user-inputted values
    ext_inputs <- input_betas %>% select(param, value)
    
    # Get shrinkage betas and names
    imp_shape_betas <- readRDS("www/data/lm_res_shrinkage.rds")
    
    get_shape_betas <- ext_lograte %>% 
      filter(param == "log_rate_mean") %>% 
      mutate(param = "rate") %>% 
      rbind(ext_inputs) %>% 
      left_join(
        read_csv("www/data/lm_res.csv") %>% 
          select(term) %>% 
          filter(!term == "(Intercept)") %>% 
          cbind(tibble(beta = imp_shape_betas$betas)) %>% 
          rename(param = term)
      )
    
    # Separate rate beta
    get_shape_rate_beta <- get_shape_betas %>% slice(1)
    
    # Get linear predictor for shape
    shape_lp <- get_shape_betas %>% 
      slice(-1) %>% 
      mutate(lp = value * beta) %>% 
      reframe(lp = sum(lp))
    
    # Get error from random sample, mean of 0 and scale of residSD
    get_error <- rnorm(1, mean = 0, sd = imp_shape_betas$resid_sd)
    
    # Estimate shape
    shape_beta <- get_shape_rate_beta$beta
    
    est_shape <- ext_lograte %>% 
      rename(rates = param) %>% 
      rowwise() %>% 
      mutate(shapes = value * shape_beta + shape_lp + get_error) %>% 
      ungroup()
    
    # Prepare Gompertz parameters ----------------------------------------------
    
    # Tidy parameters
    tidy_params <- est_shape %>% 
      mutate(
        shapes = unlist(shapes),
        rates = gsub("log_rate_", "", rates),
        rates_exp = exp(value)
      ) %>% 
      rename(
        est = rates,
        log_rates = value,
        rates = rates_exp
      ) %>% 
      select(est, log_rates, rates, shapes)
    
    # Sequence times from user input, get as tibble
    seq_times <- input_betas %>% filter(param == "duration") %>% select(value)
    seq_times <- seq_times$value
    seq_times <- tibble(t = seq(0, seq_times * 52))
    
    # Estimate hazard, cumulative risk and cumulative incidence
    est_gomp <- seq_times %>%
      left_join(tidy_params %>% crossing(seq_times)) %>% 
      arrange(t) %>% 
      mutate(
        haz = pmap_dbl(
          list(t = t, rate = rates, shape = shapes),
          function(t, rate, shape) {
            flexsurv::hgompertz(t, shape, rate)
          }
        ),
        H = pmap_dbl(
          list(t = t, rate = rates, shape = shapes),
          function(t, rate, shape) {
            flexsurv::Hgompertz(t, shape, rate)
          }
        ),
        S = exp(-H),
        cuminc = 1 - S
      )
  })
}