
# Prepares Gompertz parameters according to model specifications
# Estimates hazard, cumulative hazard and cumulative incidence using
# Gompertz parameters and scale of user-inputted time

# User-selected option to show five similar trials
# Identify five trials that have similar design features (among those that
# are significant or marginal), extract posterior samples for rate and shape, 
# and estimate risk and instantaneous hazard

library(shiny)
library(shinydashboard)
library(tidyverse)
library(posterior)
library(DT)

# Obtain quantiles
qfun <- function(M) {
  M <- as.matrix(M)
  qs <- apply(
    M, 2, function(x) stats::quantile(x, c(.025, .5, .975), na.rm = TRUE
  ))
  list(
    lo = as.numeric(qs[1, ]), 
    md = as.numeric(qs[2, ]), 
    hi = as.numeric(qs[3, ])
  )
}

# Gather materials -------------------------------------------------------------

# Import posterior draws and trial-level dataset
imp_mdl <- readRDS("www/data/design_draws.rds") # Posterior draws
imp_df <- readRDS("www/data/design_df.rds")     # Trial dataset (for var names)
imp_trials <- readRDS("www/data/trials.rds")    # Trial data (for top5 trials)

# Shape coefficients
shape_betas_df <- readr::read_csv("www/data/lm_res.csv") %>% 
  transmute(term, beta = estimate) %>%
  filter(term != "(Intercept)")

shape_betas_shr <- readRDS("www/data/lm_res_shrinkage.rds") # LASSO betas
shape_betas_df$beta <- shape_betas_shr$betas

# Separate rate beta for shape
beta_rate <- filter(shape_betas_df, term == "rate")$beta
shape_cov_betas <- filter(shape_betas_df, term != "rate")

# Get residual SD for shape
resid_sd <- shape_betas_shr$resid_sd

# Create name map
imp_nms <- colnames(imp_df$X)
map_nms <- tibble(
  param = imp_nms,
  k = seq_along(imp_nms),
  beta_id = paste0("beta[", k, "]")
)

# Get draws matrix
draws_mat <- as_draws_matrix(as.array(imp_mdl))
colnms <- colnames(draws_mat)

# Helper: Get draws matrix columns safely
get_mat_cols <- function(rx) {
  cols <- grep(rx, coln, value = TRUE)
  if (length(cols) == 0) stop("No columns matched: ", rx)
  draws_mat[, cols, drop = FALSE]
}

# Pull core parameters
beta_cols   <- grep("^beta\\[", colnms, value = TRUE)
mu_mu1_col  <- grep("^mu_mu1$", colnms, value = TRUE)
tau_mu1_col <- grep("^tau_mu1$", colnms, value = TRUE)

beta_draws    <- draws_mat[, beta_cols, drop = FALSE] # [S x K]
mu_mu1_draws  <- as.numeric(draws_mat[, mu_mu1_col])  # [S]
tau_mu1_draws <- as.numeric(draws_mat[, tau_mu1_col]) # [S]

# Reactive: Build risk estimates, dependent on user inputs ---------------------

predict_design <- function(design_inputs) {
  reactive({
    
    # Get prepared user inputs
    input_param <- design_inputs()
    
    # Match inputs to betas
    input_order <- map_nms %>% 
      left_join(input_param %>% select(param, value_prep))
    
    # Safely get variables
    idx <- which(!is.na(input_order$value_prep))
    if (length(idx) == 0) {
      shiny::validate(shiny::need(FALSE, "No valid inputs supplied."))
    }
    
    x_vec <- as.numeric(input_order$value_prep[idx]) # [K]
    beta_sel <- beta_draws[, input_order$beta_id[idx], drop = FALSE] # [S x K]
    
    # Fixed effects
    S <- nrow(beta_sel)
    z_new <- 0
    err_new <- 0
    
    # Build log-rate and rate per draw
    mu1_new <- mu_mu1_draws + tau_mu1_draws * z_new # Baseline log-rate
    rate_lp <- as.numeric(beta_sel %*% x_vec)       # Rate lp
    log_rate <- mu1_new + rate_lp                   # Log-rate
    rate <- exp(log_rate)                           # Rate
    
    # Build shape
    x_shape <- input_order %>% 
      select(term = param, x = value_prep) %>% 
      inner_join(shape_cov_betas) %>% 
      mutate(x = as.numeric(x)) %>% 
      filter(!is.na(x))
    
    shape_lp <- if (nrow(x_shape)) sum(x_shape$x * x_shape$beta) else 0
    shape <- rate * beta_rate + shape_lp + err_new # [S]
    
    # Outputs
    # Create time grid (in weeks)
    t_max_yrs <- as.numeric(filter(input_param, param == "duration")$value_prep)
    shiny::req(!is.na(t_max_yrs), t_max_yrs > 0)
    t_yrs <- seq(0, t_max_yrs, length.out = round(t_max_yrs * 52))
    t_wks <- t_yrs * 52
    
    # Instantaneous hazard
    haz <- vapply(
      t_yrs,
      function(tt) flexsurv::hgompertz(tt, shape = shape, rate = rate),
      numeric(length(rate))
    )
    
    # Cumulative distribution (i.e. risk)
    phaz <- vapply(
      t_yrs,
      function(tt) flexsurv::pgompertz(tt, shape = shape, rate = rate),
      numeric(length(rate))
    )
    
    # Time-to-event
    t2e <- 1 - phaz
    
    # Quantiles
    rq <- qfun(phaz)
    hq <- qfun(haz)
    t2eq <- qfun(t2e)
    
    # Counts (if inputted by user)
    N_input <- filter(input_param, param == "n") %>% pull(value_prep)
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
      risk_q025 = rq$lo, risk_q50 = rq$md, risk_q975 = rq$hi,
      hazard_q025 = hq$lo, hazard_q50 = hq$md, hazard_q975 = hq$hi
    )
  })
}
