
# Test environment for server features
library(tidyverse)

# Create user input data -------------------------------------------------------

# List of inputs
# Multiple choice are comma-collapsed in server (e.g. WHO regions)
inputs <- list(
  duration     = 26,
  n_log        = log(2000),
  n_facilities = log(50),
  n_arms       = 4L,
  age_lower    = "18yr",
  age_upper    = "70yr",
  min_bmi      = "None",
  max_bmi      = "None",
  who_reg      = c("Regions of the Americas", "European region"),
  one_pom      = 2L,
  trttype      = c("Placebo", "Active comparator"),
  phase        = 3L,
  blinding     = "Open label"
)

# Create tibble
input_tbl <- as_tibble(bind_rows(inputs)) %>%
  mutate(
    duration = if_else(is.na(duration), 26, duration),
    across(c(who_reg, trttype), ~ if_else(. == "", NA, .)),
    across(c(who_reg, trttype), ~ paste0(unique(.), collapse = ", ")),
    across(everything(), as.character)
  ) %>%
  pivot_longer(everything()) %>%
  filter(value != "NA") %>% 
  distinct() %>%
  arrange()

# Tidy values for user inputs to match beta names
# Conditions ignored if input is NA
input_tidy <- input_tbl %>% 
  mutate(
    value_prep = case_when(
      # Duration in years
      name == "duration" ~ as.character(as.numeric(value)/52),
      # Number of arms converted to ordinal variable
      name == "n_arms" & value == "2" & !is.na(value) ~ "1",
      name == "n_arms" & value == "3" & !is.na(value)  ~ "2",
      name == "n_arms" & value == "4" & !is.na(value)  ~ "3",
      name == "n_arms" & value > "4" & !is.na(value)  ~ "4",
      # Treatment type as binary value
      name == "trttype" & grepl("Active", value) & !is.na(value)  ~ "1",
      name == "trttype" & !grepl("Active", value) & !is.na(value)  ~ "0",
      # Age limits as binary value
      name == "age_lower" & grepl(">", value) & !is.na(value)  ~ "1",
      name == "age_lower" & !grepl(">", value) & !is.na(value)  ~ "0",
      name == "age_upper" & grepl(">", value) & !is.na(value)  ~ "1",
      name == "age_upper" & !grepl(">", value) & !is.na(value)  ~ "0",
      # BMI limits as binary value
      (name == "min_bmi" | name == "max_bmi") & value != "None" & !is.na(value)  ~ "1",
      (name == "min_bmi" | name == "max_bmi") & value == "None" & !is.na(value)  ~ "0",
      # Phase as binary values
      name == "phase" & value == "3" ~ "0",
      name == "phase" & value == "4" ~ "1",
      # POM as ordinal variable
      name == "one_pom" & value != "1" & !is.na(value)  ~ "1",
      name == "one_pom" & value == "1" & !is.na(value)  ~ "0",
      # Blinding as level name in beta dataset
      name == "blinding" & value == "Open label"  ~ NA,
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
  filter(!is.na(value_prep))

# Join betas
input_betas <- input_param %>% 
  select(param, value = value_prep) %>% 
  left_join(
    readRDS("AttRisk/www/data/sum_draws_shiny.rds") %>% 
      filter(!param %in% c("a", "mu1"))
  ) %>% 
  mutate(value = as.numeric(value))

# Estimate rate ----------------------------------------------------------------

# Import summarised posterior draws
a_imp_pos <- readRDS("AttRisk/www/data/sum_draws_shiny.rds")

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

# Linear predictor for shape ---------------------------------------------------

# Extract log-rate
ext_lograte <- est_rate %>% 
  filter(grepl("log", name)) %>% 
  select(-param) %>% 
  rename(param = name)

# Get user-inputted values
ext_inputs <- input_betas %>% select(param, value)

# Get shrinkage betas and names
imp_shape_betas <- readRDS("data/lm_res_shrinkage.rds")

get_shape_betas <- ext_lograte %>% 
  filter(param == "log_rate_mean") %>% 
  mutate(param = "rate") %>% 
  rbind(ext_inputs) %>% 
  left_join(
    read_csv("data/lm_res.csv") %>% 
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

# Prepare Gompertz parameters --------------------------------------------------

# Tidy parameters
# Exponentiate rate
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
seq_times <- tibble(t = seq(0, seq_times, by = 0.1))

# Estimate hazard, cumulative risk and cumulative incidence
est_gomp <- seq_times %>%
  left_join(tidy_params %>% crossing(seq_times)) %>% 
  arrange(t) %>% 
  mutate(
    haz = unlist(
      pmap(
        list(t = t, rate = rates, shape = shapes),
        function(t, rate, shape) {
          flexsurv::hgompertz(t, shape, rate)
        }
      )
    ),
    chaz = unlist(
      pmap(
        list(t = t, rate = rates, shape = shapes),
        function(t, rate, shape) {
          flexsurv::Hgompertz(t, shape, rate)
        }
      )
    ),
    incid = unlist(
      pmap(
        list(t = t, rate = rates, shape = shapes),
        function(t, rate, shape) {
          flexsurv::pgompertz(t, shape, rate)
        }
      )
    )
  )

# Plots
plot_haz <- est_gomp %>% 
  select(est, t, haz) %>% 
  pivot_wider(names_from = "est", values_from = "haz") %>% 
  ggplot(aes(x = t*52, y = mean)) +
  geom_line() +
  scale_x_continuous(n.breaks = 8) +
  labs(x = "Duration in weeks", y = "Hazard rate") +
  theme_bw()

plot_chaz <- est_gomp %>% 
  select(est, t, chaz) %>% 
  pivot_wider(names_from = "est", values_from = "chaz") %>% 
  ggplot(aes(x = t*52, y = mean)) +
  geom_line() +
  scale_x_continuous(n.breaks = 8) +
  labs(x = "Duration in weeks", y = "Accumulated hazard rate") +
  theme_bw()

plot_incid <- est_gomp %>% 
  select(est, t, incid) %>% 
  pivot_wider(names_from = "est", values_from = "incid") %>% 
  ggplot(aes(x = t*52, y = mean)) +
  geom_step() +
  scale_x_continuous(n.breaks = 8) +
  labs(x = "Duration in weeks", y = "Accumulated incidence") +
  theme_bw()

