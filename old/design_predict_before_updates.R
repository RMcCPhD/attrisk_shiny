predict_design <- function(design_inputs) {
  reactive({
    
    # Prepared user inputs
    input_param <- design_inputs()
    
    # Posterior draws
    imp_mdl <- readRDS("www/data/design_draws.rds")
    
    # Variable names
    imp_df <- readRDS("www/data/design_df.rds")
    imp_nms <- colnames(imp_df$X)
    
    # Create map of names
    map_nms <- tibble(
      param = imp_nms,
      k = seq_along(imp_nms),
      beta_id = paste0("beta[", k, "]")
    )
    
    # Order user inputs
    input_order <- map_nms %>% left_join(input_param %>% select(param, value_prep))
    input_set <- which(!is.na(input_order$value_prep))
    input_vals <- input_order$value_prep[input_set]
    
    # Gompertz parameters ------------------------------------------------------
    
    # Posterior draws matrix
    draws_mat <- as_draws_matrix(as.array(imp_mdl))
    draws_input <- draws_mat[, input_order$beta_id[input_set], drop = FALSE]
    
    # Get rate linear predictor, compute rate for each draw
    draws_mu1 <- rowMeans(draws_mat[, grepl("^mu1", colnames(draws_mat))])
    mu1 <- as.numeric(draws_mu1)
    rate_lp <- as.numeric(draws_input %*% as.numeric(input_vals))
    log_rate <- mu1 + rate_lp
    rate <- exp(log_rate)
    
    # Get shape linear predictor, compute shape for each draw
    shape_betas <- read_csv("www/data/lm_res.csv") %>% 
      transmute(term, beta = estimate) %>% 
      filter(!term == "(Intercept)")
    
    shape_betas_shr <- readRDS("www/data/lm_res_shrinkage.rds")
    shape_betas <- shape_betas %>% mutate(beta = shape_betas_shr$betas)
    
    beta_rate <- filter(shape_betas, term == "rate")$beta
    
    shape_lp <- input_order %>% 
      select(term = param, x = value_prep) %>% 
      inner_join(filter(shape_betas, term != "rate")) %>% 
      mutate(x = as.numeric(x)) %>% 
      filter(!is.na(x)) %>% 
      summarise(lp = sum(x * beta)) %>% 
      pull(lp)
    
    draws_a_raw <- rowMeans(draws_mat[, grepl("^a", colnames(draws_mat))])
    resid <- rowMeans(draws_mat[, grepl("error", colnames(draws_mat))])
    shape <- log_rate * beta_rate + shape_lp + resid
    
    # Hazard estimates ---------------------------------------------------------
    
    # Get time sequence
    t_max <- as.numeric(filter(input_param, param == "duration")$value_prep)
    t_years <- seq(0, t_max, length.out = 53)
    t_weeks <- t_years * 52
    
    # Estimate hazard at each time point
    haz <- vapply(
      t_years, 
      function(t) flexsurv::hgompertz(t, shape, rate), 
      numeric(length(shape))
    )
    
    # Summarise (2.5th, 50th, 97.5th)
    sum_haz <- apply(haz, 2, quantile, c(0.025, 0.5, 0.975)) %>% 
      t() %>% 
      as_tibble() %>% 
      mutate(t = t_weeks)
    # 
    # # Summary tables -----------------------------------------------------------
    # 
    # # Cumulative hazard and risk at horizon
    # t_end <- tail(t_years, 1)
    # 
    # cumhaz_end <- if_else(
    #   abs(shape) < 1e-6,
    #   rate * t_end,
    #   (rate / shape) * (exp(shape * t_end) - 1)
    # )
    # 
    # p_end <- 1 - exp(-cumhaz_end)
    # risk_sum <- quantile(p_end, c(0.025, 0.50, 0.975), na.rm = TRUE)
    # 
    # # Attrition thresholds
    # p_over <- function(th) mean(p_end > th, na.rm = TRUE)
    # 
    # risk_p <- c(
    #   p5 = prob_over(0.05), 
    #   p10 = prob_over(0.10), 
    #   p15 = prob_over(0.15),
    #   p20 = prob_over(0.20),
    #   p25 = prob_over(0.25),
    #   p30 = prob_over(0.30)
    # )
    # 
    # # Expected counts for planned sample
    # N_input <- input_order %>% filter(param == "n_log") %>% pull(value_prep)
    # N <- if (length(N_input) && !is.na(N_input)) round(exp(N_input)) else NA
    # 
    # count_sum <- if (!is.na(N)) {
    #   quantile(
    #     rbinom(length(p_end), N, p_end),
    #     c(0.025, 0.975), na.rm = TRUE
    #   )
    # } else {
    #   c(NA, NA, NA)
    # }
    # 
    # # Start and end hazard rates
    
  })
}