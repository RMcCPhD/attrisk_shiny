
# Test with individual posterior draws
rm(list = ls())
gc()

library(tidyverse)
library(posterior)

# Import fitted design model
imp_mdl <- readRDS("../attrisk/mars/design_model/outputs/design_fit.rds")

# Import dataset
imp_df <- readRDS("../attrisk/mars/design_model/latest_stan_design.rds")
imp_nms <- colnames(imp_df$X)

# Map names
map_nms <- tibble(
  param = imp_nms,
  k = seq_along(imp_nms),
  beta_id = paste0("beta[", k, "]")
)

# Create user input data -------------------------------------------------------

# List of inputs
# Multiple choice are comma-collapsed in server (e.g. WHO regions)
inputs <- list(
  duration     = 52,
  n_log        = 500,
  n_facilities = 100,
  n_arms       = 2,
  age_lower    = "18yr",
  age_upper    = "70yr",
  min_bmi      = "None",
  max_bmi      = "None",
  min_egfr     = "<=30 ml/min/1.73m2",
  max_egfr     = ">60 ml/min/1.73m2",
  who_reg      = c("Regions of the Americas", "European region"),
  one_pom      = "1",
  trttype      = c("Placebo", "Active comparator"),
  phase        = "3",
  blinding     = "Double"
)

# Create tibble
input_tbl <- as_tibble(bind_rows(inputs)) %>%
  mutate(
    duration = if_else(is.na(duration), 12, duration),
    across(c(who_reg, trttype), ~ if_else(. == "", NA, .)),
    across(c(who_reg, trttype), ~ paste0(unique(.), collapse = ", ")),
    across(everything(), as.character)
  ) %>%
  pivot_longer(everything()) %>%
  filter(value != "NA") %>% 
  distinct()

# Tidy values for user inputs to match beta names
# Conditions ignored if input is NA
input_tidy <- input_tbl %>% 
  mutate(
    value_prep = case_when(
      # Duration in years
      name == "duration" ~ as.character(as.numeric(value)/52),
      # Number of arms converted to ordinal variable
      name == "n_arms" & value == "2" & !is.na(value) ~ "0",
      name == "n_arms" & value == "3" & !is.na(value)  ~ "1",
      name == "n_arms" & value == "4" & !is.na(value)  ~ "2",
      name == "n_arms" & value > "4" & !is.na(value)  ~ "3",
      # Age limits as binary value
      name == "age_lower" & grepl(">", value) & !is.na(value)  ~ "1",
      name == "age_lower" & !grepl(">", value) & !is.na(value)  ~ "0",
      name == "age_upper" & grepl(">", value) & !is.na(value)  ~ "1",
      name == "age_upper" & !grepl(">", value) & !is.na(value)  ~ "0",
      # BMI limits as binary value
      (name == "min_bmi" | name == "max_bmi") & value != "None" & !is.na(value)  ~ "1",
      (name == "min_bmi" | name == "max_bmi") & value == "None" & !is.na(value)  ~ "0",
      # eGFR limits as binary value
      (name == "min_egfr" | name == "max_egfr") & value != "None" & !is.na(value)  ~ "1",
      (name == "min_egfr" | name == "max_egfr") & value == "None" & !is.na(value)  ~ "0",
      # Phase as binary values
      name == "phase" & value == "3" ~ "0",
      name == "phase" & value == "4" ~ "1",
      # POM as ordinal variable
      name == "one_pom" & value != "1" & !is.na(value)  ~ "1",
      name == "one_pom" & value == "1" & !is.na(value)  ~ "0",
      # Blinding as level name in beta dataset
      name == "blinding" & value == "Open label"  ~ "0",
      name == "blinding" & !is.na(value) ~ "1",
      # WHO regions as binary values
      name == "who_reg" & !is.na(value) ~ "1",
      TRUE ~ value
    )
  )

# Prepare param variable for joining with betas
input_param <- input_tidy %>% 
  mutate(value = if_else(name == "trttype", NA, value)) %>% 
  separate_rows(value, sep = ", ") %>% 
  mutate(
    param = case_when(
      !name %in% c("who_reg", "blinding") ~ name,
      # Blinding
      name == "blinding" & value == "Open label" ~ NA,
      name == "blinding" & value == "Double" ~ "blindingdouble",
      name == "blinding" & value == "Triple" ~ "blindingtriple",
      name == "blinding" & value == "Quadruple" ~ "blindingquadruple",
      # WHO regions
      name == "who_reg" & grepl("Africa", value) ~ "reg_afr",
      name == "who_reg" & grepl("Americas", value) ~ "reg_am",
      name == "who_reg" & grepl("Medi", value) ~ "reg_em",
      name == "who_reg" & grepl("Europ", value) ~ "reg_eur",
      name == "who_reg" & grepl("South", value) ~ "reg_sea",
      name == "who_reg" & grepl("Pacific", value) ~ "reg_wpac"
    )
  ) %>% 
  filter(!is.na(param))

# Order inputs
input_order <- map_nms %>% left_join(input_param %>% select(param, value_prep))
input_set <- which(!is.na(input_order$value_prep))
input_vals <- input_order$value_prep[input_set]

# Posterior draw-level estimation ----------------------------------------------

# Get draws matrix for input variables
draws_mat <- as_draws_matrix(as.array(imp_mdl))
draws_input <- draws_mat[, input_order$beta_id[input_set], drop = FALSE]

# Get rate linear predictor and compute rate
draws_mu1 <- rowMeans(draws_mat[, grepl("^mu1", colnames(draws_mat))])
mu1 <- as.numeric(draws_mu1)
rate_lp <- as.numeric(draws_input %*% as.numeric(input_vals))
log_rate <- mu1 + rate_lp
rate <- exp(log_rate)

# Get shape lp and compute shape
shape_betas <- read_csv("app/www/data/lm_res.csv") %>% 
  transmute(term, beta = estimate) %>% 
  filter(!term == "(Intercept)")

shape_betas_shr <- readRDS("app/www/data/lm_res_shrinkage.rds")
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

# Hazard estimates -------------------------------------------------------------

t <- seq(0, 1, by = 0.01)
haz <- vapply(
  t, function(t) flexsurv::hgompertz(t, shape, rate), numeric(length(shape))
)
sum_haz <- apply(haz, 2, quantile, c(0.025, 0.5, 0.975))

# Get time sequence
t_max <- filter(input_param, param == "duration")$value_prep
t_max <- as.numeric(t_max)
t_seq <- t_max / 52
t <- seq(0, t_max, by = t_seq)

haz <- vapply(
  t, function(t) flexsurv::hgompertz(t, shape, rate), numeric(length(shape))
)
sum_haz <- apply(haz, 2, quantile, c(0.025, 0.5, 0.975))

t(sum_haz) %>% 
  as_tibble() %>% 
  mutate(t = t) %>% 
  ggplot(aes(x = t, y= `50%`, ymin = `2.5%`, ymax = `97.5%`)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, fill = "steelblue", colour = "black", linetype = "dashed") +
  labs(x = "Follow-up time (weeks)", y = "Hazard rate") +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  theme_classic()
