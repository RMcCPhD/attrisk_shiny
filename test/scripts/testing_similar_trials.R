
# Test pulling out trials with the same inputs as the user-defined trial

options(scipen = 999)
rm(list = ls())
gc()

library(tidyverse)
library(posterior)

# Import design draws and dataset
imp_draws <- readRDS("app/www/data/design_draws.rds")
imp_df <- readRDS("app/www/data/design_df.rds")
imp_trials <- readRDS("app/www/data/trials.rds")

# Mock up user inputs
input_param <- tribble(
  ~name,        ~value,                     ~value_prep,        ~param,
  "duration",    "36",                       "0.692307692307692", "duration",
  "n_log",       "5.52146091786225",         "5.52146091786225",  "n_log",
  "n_facilities","2.99573227355399",         "2.99573227355399",  "n_facilities",
  "n_arms",      "2",                        "1",                 "n_arms",
  "age_lower",   "18yr",                     "0",                 "age_lower",
  "age_upper",   "70yr",                     "0",                 "age_upper",
  "min_bmi",     "None",                     "0",                 "min_bmi",
  "max_bmi",     "None",                     "0",                 "max_bmi",
  "min_egfr",    "None",                     "0",                 "min_egfr",
  "max_egfr",    "None",                     "0",                 "max_egfr",
  "who_reg",     "Regions of the Americas*", "1",                 "reg_am",
  "who_reg",     "European region*",         "1",                 "reg_eur",
  "who_reg",     "Western Pacific region",   "1",                 "reg_wpac",
  "one_pom",     "1",                        "0",                 "one_pom",
  "trttype",     "NA",                       "0",                 "trttype",
  "phase",       "4",                        "1",                 "phase",
  "blinding",    "Quadruple",                "1",                 "blindingquadruple"
)

# Shape coefficients
shape_betas_df <- readr::read_csv("app/www/data/lm_res.csv") %>% 
  transmute(term, beta = estimate) %>%
  filter(term != "(Intercept)")

shape_betas_shr <- readRDS("app/www/data/lm_res_shrinkage.rds") # LASSO betas
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
draws_mat <- imp_draws
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

beta_draws   <- draws_mat[, beta_cols, drop = FALSE] # [S x K]
mu_mu1_draws  <- as.numeric(draws_mat[, mu_mu1_col])  # [S]
tau_mu1_draws <- as.numeric(draws_mat[, tau_mu1_col]) # [S]

# Build user trial posteriors --------------------------------------------------

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

# Random effects for new arm
S <- nrow(beta_sel)
z_new <- 0 # Test: Removing random effects (option to add these tbc)
err_new <- 0
# z_new <- rnorm(S)
# err_new <- rnorm(S, 0, resid_sd)

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

# Find trials in set similar to user trial features ----------------------------

# Based on features that were significant or marginal predictors of
# Gompertz rate - logistic regression model fitted to trial set to create a
# weighting used to calculate the distance between features

X <- imp_df$X # [J x K]
X <- X[, map_nms$param, drop = FALSE]
colnames(X) <- map_nms$param

# Target features
targets <- c(
  "age_upper", "max_egfr", "duration", "phase",
  "n_log", "n_facilities", "reg_am", "reg_eur"
)

# Build long tibble of trial set features and user features
trial_long <- as_tibble(X) %>% 
  mutate(trial_id = imp_trials$trial_id) %>% 
  pivot_longer(-trial_id) %>% 
  rename(param = name) %>% 
  filter(param %in% targets) %>% 
  mutate(source = "trial")

user_long <- input_param %>% 
  select(param, value_prep) %>% 
  filter(param %in% targets) %>% 
  transmute(
    trial_id = "user",
    param,
    value = as.numeric(value_prep),
    source = "user"
  )

long_all <- bind_rows(trial_long, user_long)

# Logistic regression model of design features
trial_lm_df <- imp_trials %>% select(trial_id, arm_n, attr, all_of(targets))
trial_lm_f <- as.formula(paste0("cbind(attr, arm_n) ~ ", paste0(targets, collapse = " + ")))
trial_lm <- glm(trial_lm_f, data = trial_lm_df, family = "binomial")

trial_lm_res <- broom::tidy(trial_lm) %>% 
  filter(!term == "(Intercept)") %>% 
  mutate(
    p.value = round(p.value, 3),
    odds = exp(estimate)
  ) %>% 
  rename(param = term) %>% 
  transmute(
    param = param,
    wgt = abs(odds)
  ) %>% 
  mutate(wgt = wgt/sum(wgt))

# Join weighted odds to tibble of features
long_wgt <- long_all %>% left_join(trial_lm_res)

# Compute weighted distance to user trial features
user_vec <- long_wgt %>%
  filter(trial_id == "user") %>%
  select(param, user_std = value)

trial_scores <- long_wgt %>%
  filter(source == "trial") %>%
  select(trial_id, param, trial_std = value, wgt) %>%
  left_join(user_vec, by = "param") %>%
  mutate(contrib = wgt * (trial_std - user_std)^2) %>%
  group_by(trial_id) %>%
  summarise(dist_score = sum(contrib, na.rm = TRUE), .groups = "drop") %>%
  arrange(dist_score)

# Take 5 closest trials
top5_design <- trial_scores %>% slice_head(n = 5)
top5_design

# Extract similar trial posteriors ---------------------------------------------

# Create a lookup for the model trial set
# Used to identify posterior parameters for each (e.g. z_mu1[X])
design_lkp <- imp_trials %>% 
  select(trial_id) %>% 
  mutate(id = row_number()) %>% 
  filter(trial_id %in% top5_design$trial_id) %>% 
  left_join(
    imp_df$X %>% 
      as_tibble() %>% 
      mutate(trial_id = imp_trials$trial_id) %>% 
      filter(trial_id %in% top5_design$trial_id)
  )

# Build matrix of trial-specific posterior parameters
extract_indexed <- function(draws_mat, prefix, ids) {
  pats <- sprintf("^%s\\[%s\\]$", prefix, ids)      # e.g. "^mu1\\[12\\]$"
  cols <- vapply(pats, function(p) {
    idx <- grep(p, colnms)
    if (length(idx) != 1) stop("Column not found or ambiguous for pattern: ", p)
    idx
  }, integer(1))
  draws_mat[, cols, drop = FALSE]
}

# Get shape and rate for top 5 trials
top5_rate <- extract_indexed(draws_mat, "b", design_lkp$id)
top5_shape <- extract_indexed(draws_mat, "a", design_lkp$id)

# Hazard estimates for user trial ----------------------------------------------

# Build time
t_max_yrs <- as.numeric(filter(input_param, param == "duration")$value_prep)
t_yrs <- seq(0, t_max_yrs, length.out = 53)
t_wks <- t_yrs * 52

# Instantaneous hazard
haz <- vapply(
  t_yrs,
  function(tt) flexsurv::hgompertz(tt, shape = shape, rate = rate),
  numeric(length(rate))
)

# Cumulative hazard (for getting risk)
chaz <- vapply(
  t_yrs,
  function(tt) flexsurv::Hgompertz(tt, shape = shape, rate = rate),
  numeric(length(rate))
)

# Risk
risk <- 1 - exp(-chaz)

# Time-to-event
t2e <- exp(-chaz)

# Quantiles
qfun <- function(M) {
  M <- as.matrix(M)
  qs <- apply(M, 2, function(x) stats::quantile(x, c(.025, .5, .975), na.rm = TRUE))
  list(lo = as.numeric(qs[1, ]), md = as.numeric(qs[2, ]), hi = as.numeric(qs[3, ]))
}

rq <- qfun(risk)
hq <- qfun(haz)
t2eq <- qfun(t2e)

# Counts (if inputted by user)
N_input <- input_order %>% filter(param == "n_log") %>% pull(value_prep)
N_input <- as.numeric(N_input)
N <- if (!is.na(N_input)) as.integer(round(exp(N_input))) else NA_integer_

# Get outputs
cCount <- N * risk
atRisk <- N * t2e
ccq <- qfun(cCount)
arq <- qfun(atRisk)

# Summarise in tibble
sum_tbl <- tibble(
  trial_id = "user",
  t_wks = t_wks,
  risk_q025 = rq$lo, risk_q50 = rq$md, risk_q975 = rq$hi,
  hazard_q025 = hq$lo, hazard_q50 = hq$md, hazard_q975 = hq$hi,
  surv_q025 = t2eq$lo,  surv_q50 = t2eq$md,  surv_q975 = t2eq$hi,
  drp_q50 = N - arq$md,
  at_risk_q025 = arq$lo, at_risk_q50 = arq$md, at_risk_q975 = arq$hi,
  cdrop_q025 = ccq$lo, cdrop_q50 = ccq$md, cdrop_q975 = ccq$hi
)

# Hazard estimates for 5 most similar trials -----------------------------------

top5cols <- ncol(top5_rate)
top5_lst <- lapply(seq_len(top5cols), function(j) {
  rate_j <- top5_rate[, j]
  shape_j <- top5_shape[, j]
  id <- design_lkp$trial_id[j]
  N <- round(exp(design_lkp$n_log[j]))
  t_yrs <- seq(0, design_lkp$duration[j], length.out = 53)
  t_wks <- t_yrs * 52
  
  # Instantaneous hazard
  haz <- vapply(
    t_yrs,
    function(tt) flexsurv::hgompertz(tt, shape = shape_j, rate = rate_j),
    numeric(length(rate))
  )
  
  # Cumulative hazard (for getting risk)
  chaz <- vapply(
    t_yrs,
    function(tt) flexsurv::Hgompertz(tt, shape = shape_j, rate = rate_j),
    numeric(length(rate))
  )
  
  # Risk
  risk <- 1 - exp(-chaz)
  
  # Time-to-event
  t2e <- exp(-chaz)
  rq <- qfun(risk)
  hq <- qfun(haz)
  t2eq <- qfun(t2e)
  
  # Get outputs
  cCount <- N * risk
  atRisk <- N * t2e
  ccq <- qfun(cCount)
  arq <- qfun(atRisk)
  
  # Summarise in tibble
  sum_tbl <- tibble(
    trial_id = id,
    t_wks = t_wks,
    risk_q025 = rq$lo, risk_q50 = rq$md, risk_q975 = rq$hi,
    hazard_q025 = hq$lo, hazard_q50 = hq$md, hazard_q975 = hq$hi,
    surv_q025 = t2eq$lo,  surv_q50 = t2eq$md,  surv_q975 = t2eq$hi,
    drp_q50 = N - arq$md,
    at_risk_q025 = arq$lo, at_risk_q50 = arq$md, at_risk_q975 = arq$hi,
    cdrop_q025 = ccq$lo, cdrop_q50 = ccq$md, cdrop_q975 = ccq$hi
  )
})

# Get plots --------------------------------------------------------------------

# Bind summary data
sum_all <- bind_rows(sum_tbl, top5_lst)

# Plot hazard for main trial
sum_all %>% 
  filter(trial_id == "user") %>% 
  ggplot(aes(x = t_wks, y = risk_q50, ymin = risk_q025, ymax = risk_q975)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, fill = "steelblue", colour = "black", linetype = "dashed") +
  geom_line(
    data = sum_all %>% filter(trial_id != "user"), 
    aes(x = t_wks, y = risk_q50, colour = trial_id)
  ) +
  labs(x = "Follow-up time (weeks)", y = "Hazard rate") +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  theme_classic()
