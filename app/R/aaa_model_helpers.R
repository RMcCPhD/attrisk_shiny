

library(tidyverse)
library(posterior)


`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

attrisk_data_path <- function(file) file.path("www", "data", file)

attrisk_design_draws <- readRDS(attrisk_data_path("design_draws.rds"))
attrisk_design_df <- readRDS(attrisk_data_path("design_df.rds"))
attrisk_trials <- readRDS(attrisk_data_path("trials.rds"))
attrisk_shape_rate <- readRDS(attrisk_data_path("shape_rate.rds"))

attrisk_conduct_scale <- readr::read_csv(
  attrisk_data_path("conduct_scale.csv"),
  show_col_types = FALSE
)
attrisk_delta_draws <- readRDS(attrisk_data_path("delta_draws.rds"))
attrisk_sobol <- readRDS(attrisk_data_path("sobol_lst.rds"))

attrisk_design_terms <- colnames(attrisk_design_df$X)
attrisk_design_map <- tibble(
  param = attrisk_design_terms,
  k = seq_along(attrisk_design_terms),
  beta_id = paste0("beta[", k, "]")
)

attrisk_draws_mat <- as_draws_matrix(attrisk_design_draws)
attrisk_draw_names <- colnames(attrisk_draws_mat)

attrisk_missing_betas <- setdiff(attrisk_design_map$beta_id, attrisk_draw_names)
if (length(attrisk_missing_betas) > 0) {
  stop("Missing design beta draws: ", paste(attrisk_missing_betas, collapse = ", "))
}

attrisk_beta_draws <- attrisk_draws_mat[, attrisk_design_map$beta_id, drop = FALSE]
attrisk_mu_mu1_draws <- as.numeric(attrisk_draws_mat[, "mu_mu1"])
attrisk_tau_mu1_draws <- as.numeric(attrisk_draws_mat[, "tau_mu1"])

attrisk_shape_alpha <- attrisk_shape_rate$shape_alpha %||% attrisk_design_df$alpha_shape
attrisk_shape_rate_beta <- attrisk_shape_rate$rate_beta %||% attrisk_design_df$beta_rate
attrisk_shape_uncertainty <- attrisk_shape_rate$uncertainty %||% attrisk_design_df$resid_sd

attrisk_delta_mu <- rowMeans(attrisk_delta_draws, na.rm = TRUE)
attrisk_conduct_terms <- names(attrisk_sobol$loghr)
attrisk_conduct_loghrs <- tibble(
  param = attrisk_conduct_terms,
  logHR = as.numeric(attrisk_sobol$loghr)
)

qfun <- function(M) {
  M <- as.matrix(M)
  qs <- apply(
    M, 2,
    function(x) stats::quantile(x, c(.025, .5, .975), na.rm = TRUE)
  )
  list(
    lo = as.numeric(qs[1, ]),
    md = as.numeric(qs[2, ]),
    hi = as.numeric(qs[3, ])
  )
}

attrisk_first <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)
  x[[1]]
}

attrisk_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  if (all(is.na(x)) || identical(x, character(0))) return(NA_character_)
  paste(x[!is.na(x) & nzchar(x)], collapse = ", ")
}

attrisk_num <- function(x) {
  suppressWarnings(as.numeric(attrisk_first(x)))
}

attrisk_input_row <- function(group, name, label, value, value_prep = value,
                              display_value = value, order = 999) {
  tibble(
    role = "input",
    group = group,
    name = name,
    label = label,
    value = as.character(value),
    value_prep = as.character(value_prep),
    display_value = as.character(display_value),
    param = NA_character_,
    display_order = order
  )
}

attrisk_model_row <- function(param, value, group = "model") {
  tibble(
    role = "model",
    group = group,
    name = param,
    label = NA_character_,
    value = NA_character_,
    value_prep = as.character(value),
    display_value = NA_character_,
    param = param,
    display_order = 999
  )
}

attrisk_model_rows <- function(values, group = "model") {
  keep <- !is.na(values)
  values <- values[keep]
  if (length(values) == 0) return(tibble())
  purrr::imap_dfr(values, ~ attrisk_model_row(.y, .x, group = group))
}

attrisk_n_cat <- function(n) {
  case_when(
    is.na(n) ~ NA_character_,
    n < 250 ~ "<250",
    n < 500 ~ "250-499",
    n < 750 ~ "500-749",
    n < 1000 ~ "750-999",
    TRUE ~ ">=1000"
  )
}

attrisk_arm_n_cat <- function(n, n_arms) {
  arm_n <- n / n_arms
  case_when(
    is.na(arm_n) ~ NA_character_,
    arm_n < 100 ~ "<100",
    arm_n < 200 ~ "100-199",
    arm_n < 300 ~ "200-299",
    TRUE ~ ">=300"
  )
}

attrisk_sites_cat <- function(value) {
  value <- attrisk_chr(value)
  case_when(
    is.na(value) ~ NA_character_,
    value %in% c("<50", "50 - 99", "<100") ~ "<100",
    value %in% c("100 - 199", "100-199", ">=100") ~ "100-199",
    value %in% c(">=200", "200+") ~ ">=200",
    TRUE ~ value
  )
}

attrisk_trttype <- function(value) {
  value <- attrisk_chr(value)
  has_placebo <- !is.na(value) && grepl("Placebo", value, ignore.case = TRUE)
  has_active <- !is.na(value) && grepl("Active", value, ignore.case = TRUE)
  case_when(
    has_placebo && has_active ~ "placebo_active",
    has_active ~ "active",
    TRUE ~ "placebo"
  )
}

attrisk_masking <- function(value) {
  value <- attrisk_chr(value)
  case_when(
    is.na(value) ~ "open_label",
    grepl("double", value, ignore.case = TRUE) ~ "double",
    grepl("triple", value, ignore.case = TRUE) ~ "triple",
    grepl("quad", value, ignore.case = TRUE) ~ "quadruple",
    TRUE ~ "open_label"
  )
}

attrisk_upper_age <- function(value) {
  value <- attrisk_chr(value)
  case_when(
    is.na(value) ~ "none",
    grepl("^>70|>80", value) ~ ">80",
    TRUE ~ "80"
  )
}

attrisk_lower_age <- function(value) {
  value <- attrisk_chr(value)
  if (!is.na(value) && grepl(">18", value)) ">18" else "18"
}

attrisk_upper_bmi <- function(value) {
  value <- attrisk_chr(value)
  case_when(
    is.na(value) || value == "None" ~ "none",
    grepl("30|35", value) ~ ">30",
    TRUE ~ ">40"
  )
}

attrisk_lower_bmi <- function(value) {
  value <- attrisk_chr(value)
  if (!is.na(value) && value != "None") "<27" else "none"
}

attrisk_lower_egfr <- function(min_value, max_value) {
  min_value <- attrisk_chr(min_value)
  max_value <- attrisk_chr(max_value)
  case_when(
    !is.na(min_value) && grepl("<=30|15|30", min_value) ~ "<30",
    !is.na(min_value) && min_value != "None" ~ "<60",
    !is.na(max_value) && max_value != "None" ~ "<60",
    TRUE ~ "none"
  )
}

attrisk_region_values <- function(value) {
  value <- attrisk_chr(value)
  c(
    region_of_the_americas = !is.na(value) && grepl("Americas", value, ignore.case = TRUE),
    european_region = !is.na(value) && grepl("European", value, ignore.case = TRUE),
    western_pacific_region = !is.na(value) && grepl("Pacific", value, ignore.case = TRUE),
    african_region = !is.na(value) && grepl("African", value, ignore.case = TRUE),
    south_east_asia_region = !is.na(value) && grepl("SE Asian|South", value, ignore.case = TRUE),
    eastern_mediterranean_region = !is.na(value) && grepl("Mediterranean", value, ignore.case = TRUE)
  ) %>%
    as.integer()
}

attrisk_design_input_table <- function(input, prefix = "design") {
  get <- function(id) input[[paste0(prefix, "_", id)]]

  duration_wks <- attrisk_num(get("duration"))
  n_arms <- attrisk_num(get("arms"))
  n <- attrisk_num(get("n"))
  arm_n <- n / n_arms

  trt <- get("trttype")
  trt_level <- attrisk_trttype(trt)
  n_cat <- attrisk_n_cat(n)
  arm_n_cat <- attrisk_arm_n_cat(n, n_arms)
  sites_cat <- attrisk_sites_cat(get("n_facilities"))
  poms_cat <- if_else(attrisk_chr(get("poms")) == ">1", 1, 0)
  phase <- if_else(attrisk_chr(get("phase")) == "4", 1, 0)
  n_arms_cat <- if_else(!is.na(n_arms) & n_arms > 2, 1, 0)
  masking <- attrisk_masking(get("blind"))
  upper_age <- attrisk_upper_age(get("age_upper"))
  lower_age <- attrisk_lower_age(get("age_lower"))
  upper_bmi <- attrisk_upper_bmi(get("bmi_upper"))
  lower_bmi <- attrisk_lower_bmi(get("bmi_lower"))
  lower_egfr <- attrisk_lower_egfr(get("egfr_lower"), get("egfr_upper"))
  who_regions <- attrisk_chr(get("who_reg"))
  if (is.na(who_regions) || !nzchar(who_regions)) who_regions <- "None selected"

  display_rows <- bind_rows(
    attrisk_input_row("design", "duration", "Duration (weeks)", duration_wks,
                      duration_wks / 52, duration_wks, 10),
    attrisk_input_row("design", "number_of_arms", "Number of arms", n_arms,
                      n_arms, n_arms, 20),
    attrisk_input_row("design", "n", "Number of participants", n, n, n, 30),
    attrisk_input_row("design", "n_cat", "Trial size category", n_cat, n_cat, n_cat, 40),
    attrisk_input_row("design", "arm_n", "Estimated participants per arm",
                      round(arm_n, 1), arm_n, round(arm_n, 1), 50),
    attrisk_input_row("design", "arm_n_cat", "Arm size category",
                      arm_n_cat, arm_n_cat, arm_n_cat, 60),
    attrisk_input_row("design", "n_sites_cat", "Number of sites category",
                      sites_cat, sites_cat, sites_cat, 70),
    attrisk_input_row("design", "n_poms_cat", "Number of primary outcomes",
                      attrisk_chr(get("poms")), poms_cat, attrisk_chr(get("poms")), 80),
    attrisk_input_row("design", "phase", "Phase",
                      attrisk_chr(get("phase")), phase, attrisk_chr(get("phase")), 90),
    attrisk_input_row("design", "trttype", "Comparator",
                      attrisk_chr(trt), trt_level, attrisk_chr(trt), 100),
    attrisk_input_row("design", "masking", "Masking",
                      attrisk_chr(get("blind")), masking, attrisk_chr(get("blind")), 110),
    attrisk_input_row("design", "who_regions", "WHO regions",
                      who_regions, who_regions, who_regions, 120),
    attrisk_input_row("eligibility", "lower_age", "Lower age limit",
                      attrisk_chr(get("age_lower")), lower_age, lower_age, 130),
    attrisk_input_row("eligibility", "upper_age", "Upper age limit",
                      attrisk_chr(get("age_upper")), upper_age, upper_age, 140),
    attrisk_input_row("eligibility", "lower_bmi", "Lower BMI restriction",
                      attrisk_chr(get("bmi_lower")), lower_bmi, lower_bmi, 150),
    attrisk_input_row("eligibility", "upper_bmi", "Upper BMI restriction",
                      attrisk_chr(get("bmi_upper")), upper_bmi, upper_bmi, 160),
    attrisk_input_row("eligibility", "lower_egfr", "eGFR restriction",
                      paste(na.omit(c(attrisk_chr(get("egfr_lower")), attrisk_chr(get("egfr_upper")))),
                            collapse = ", "),
                      lower_egfr, lower_egfr, 170)
  )

  design_values <- c(
    trttypeactive = as.integer(trt_level == "active"),
    trttypeplacebo_active = as.integer(trt_level == "placebo_active"),
    `n_cat250-499` = as.integer(n_cat == "250-499"),
    `n_cat500-749` = as.integer(n_cat == "500-749"),
    `n_cat750-999` = as.integer(n_cat == "750-999"),
    `n_cat>=1000` = as.integer(n_cat == ">=1000"),
    `arm_n_cat100-199` = as.integer(arm_n_cat == "100-199"),
    `arm_n_cat200-299` = as.integer(arm_n_cat == "200-299"),
    `arm_n_cat>=300` = as.integer(arm_n_cat == ">=300"),
    n_arms_cat = n_arms_cat,
    `n_sites_cat100-199` = as.integer(sites_cat == "100-199"),
    `n_sites_cat>=200` = as.integer(sites_cat == ">=200"),
    n_poms_cat = poms_cat,
    phase = phase,
    maskingdouble = as.integer(masking == "double"),
    maskingtriple = as.integer(masking == "triple"),
    maskingquadruple = as.integer(masking == "quadruple"),
    attrisk_region_values(get("who_reg")),
    upper_age80 = as.integer(upper_age == "80"),
    `upper_age>80` = as.integer(upper_age == ">80"),
    `lower_age>18` = as.integer(lower_age == ">18"),
    `upper_bmi>30` = as.integer(upper_bmi == ">30"),
    `upper_bmi>40` = as.integer(upper_bmi == ">40"),
    `lower_bmi<27` = as.integer(lower_bmi == "<27"),
    `lower_egfr<30` = as.integer(lower_egfr == "<30"),
    `lower_egfr<60` = as.integer(lower_egfr == "<60")
  )

  bind_rows(
    display_rows,
    attrisk_model_rows(design_values, group = "design_model")
  )
}

attrisk_scale_conduct <- function(name, value) {
  if (is.na(value)) return(NA_real_)
  scale_row <- attrisk_conduct_scale %>% filter(.data$name == !!name)
  if (nrow(scale_row) == 0) return(value)
  (value - scale_row$mu[[1]]) / scale_row$d[[1]]
}

attrisk_conduct_characteristic_table <- function(input) {
  age10_raw <- attrisk_num(input$conduct_age) / 10
  durn_raw <- attrisk_num(input$conduct_t2dur)
  hdl_raw <- attrisk_num(input$conduct_hdl)
  total_raw <- attrisk_num(input$conduct_totalc)
  race <- attrisk_chr(input$conduct_race)

  display_rows <- bind_rows(
    attrisk_input_row("characteristics", "age10", "Age (years)",
                      attrisk_num(input$conduct_age), age10_raw,
                      attrisk_num(input$conduct_age), 200),
    attrisk_input_row("characteristics", "sex", "Sex",
                      attrisk_chr(input$conduct_sex),
                      if_else(attrisk_chr(input$conduct_sex) == "Male", 1, 0),
                      attrisk_chr(input$conduct_sex), 210),
    attrisk_input_row("characteristics", "race", "Race",
                      race, race, race, 220),
    attrisk_input_row("characteristics", "durn", "T2DM duration (years)",
                      durn_raw, durn_raw, durn_raw, 230),
    attrisk_input_row("characteristics", "hba1c", "HbA1c (%)",
                      attrisk_num(input$conduct_hba1c),
                      attrisk_num(input$conduct_hba1c),
                      attrisk_num(input$conduct_hba1c), 240),
    attrisk_input_row("characteristics", "chol_hdl", "HDL cholesterol (mmol/L)",
                      hdl_raw, hdl_raw, hdl_raw, 250),
    attrisk_input_row("characteristics", "bmi", "BMI (kg/m2)",
                      attrisk_num(input$conduct_bmi),
                      attrisk_num(input$conduct_bmi),
                      attrisk_num(input$conduct_bmi), 260),
    attrisk_input_row("characteristics", "egfr", "eGFR (ml/min/1.73m2)",
                      attrisk_num(input$conduct_egfr),
                      attrisk_num(input$conduct_egfr),
                      attrisk_num(input$conduct_egfr), 270),
    attrisk_input_row("characteristics", "sbp", "Systolic BP (mmHg)",
                      attrisk_num(input$conduct_sbp),
                      attrisk_num(input$conduct_sbp),
                      attrisk_num(input$conduct_sbp), 280),
    attrisk_input_row("characteristics", "chol_total", "Total cholesterol (mmol/L)",
                      total_raw, total_raw, total_raw, 290),
    attrisk_input_row("characteristics", "comorbs", "Comorbidities",
                      attrisk_num(input$conduct_comorbs),
                      attrisk_num(input$conduct_comorbs),
                      attrisk_num(input$conduct_comorbs), 300),
    attrisk_input_row("characteristics", "conmeds", "Concomitant medications",
                      attrisk_num(input$conduct_conmed),
                      attrisk_num(input$conduct_conmed),
                      attrisk_num(input$conduct_conmed), 310),
    attrisk_input_row("characteristics", "cursmoke", "Current smoker",
                      attrisk_chr(input$conduct_smoke),
                      if_else(attrisk_chr(input$conduct_smoke) == "Yes", 1, 0),
                      attrisk_chr(input$conduct_smoke), 320)
  )

  conduct_values <- c(
    age10 = attrisk_scale_conduct("age10", age10_raw),
    sex = if_else(attrisk_chr(input$conduct_sex) == "Male", 1, 0),
    bmi = attrisk_scale_conduct("bmi", attrisk_num(input$conduct_bmi)),
    durn = attrisk_scale_conduct("durn", durn_raw),
    sbp = attrisk_scale_conduct("sbp", attrisk_num(input$conduct_sbp)),
    chol_hdl = attrisk_scale_conduct("chol_hdl", hdl_raw),
    chol_total = attrisk_scale_conduct("chol_total", total_raw),
    hba1c = attrisk_scale_conduct("hba1c", attrisk_num(input$conduct_hba1c)),
    egfr = attrisk_scale_conduct("egfr", attrisk_num(input$conduct_egfr)),
    cursmoke = if_else(attrisk_chr(input$conduct_smoke) == "Yes", 1, 0),
    asian = as.integer(race == "Asian*"),
    other = as.integer(race == "Other"),
    black_aa = as.integer(race == "Black/African American*"),
    comorbs = attrisk_num(input$conduct_comorbs),
    conmeds = attrisk_num(input$conduct_conmed)
  )

  bind_rows(
    display_rows,
    attrisk_model_rows(conduct_values, group = "conduct_model")
  )
}

attrisk_model_values <- function(input_param, terms) {
  values <- setNames(rep(0, length(terms)), terms)

  supplied <- input_param %>%
    filter(.data$role == "model", .data$param %in% terms) %>%
    transmute(
      param = .data$param,
      value_num = suppressWarnings(as.numeric(.data$value_prep))
    ) %>%
    filter(is.finite(.data$value_num)) %>%
    group_by(.data$param) %>%
    summarise(value_num = max(.data$value_num), .groups = "drop")

  if (nrow(supplied) > 0) {
    values[supplied$param] <- supplied$value_num
  }

  values
}

attrisk_input_value <- function(input_param, name) {
  raw_value <- input_param %>%
    filter(.data$role == "input", .data$name == !!name) %>%
    pull(.data$value_prep) %>%
    attrisk_first()

  value <- suppressWarnings(as.numeric(raw_value))
  if (!is.finite(value)) NA_real_ else value
}

attrisk_design_parameters <- function(input_param, include_new_trial_error = TRUE) {
  x_vec <- attrisk_model_values(input_param, attrisk_design_terms)
  S <- nrow(attrisk_beta_draws)

  z_rate <- if (isTRUE(include_new_trial_error)) stats::rnorm(S) else rep(0, S)
  rate_lp <- as.numeric(attrisk_beta_draws %*% x_vec)
  log_rate <- attrisk_mu_mu1_draws + attrisk_tau_mu1_draws * z_rate + rate_lp
  rate <- exp(log_rate)

  z_shape <- if (isTRUE(include_new_trial_error)) stats::rnorm(S) else rep(0, S)
  shape <- attrisk_shape_alpha + attrisk_shape_rate_beta * rate +
    attrisk_shape_uncertainty * z_shape

  list(log_rate = log_rate, rate = rate, shape = shape, x = x_vec)
}

attrisk_gompertz_cdf <- function(t, shape, rate) {
  cum_hazard <- ifelse(
    abs(shape) < 1e-4,
    rate * t,
    rate / shape * expm1(shape * t)
  )
  pmin(pmax(-expm1(-cum_hazard), 0), 1)
}

attrisk_gompertz_hazard <- function(t, shape, rate) {
  rate * exp(shape * t)
}

attrisk_time_grid <- function(input_param) {
  t_max_yrs <- attrisk_input_value(input_param, "duration")
  shiny::req(!is.na(t_max_yrs), t_max_yrs > 0)
  n_points <- max(2, round(t_max_yrs * 52))
  t_yrs <- seq(0, t_max_yrs, length.out = n_points)
  tibble(t_yrs = t_yrs, t_wks = t_yrs * 52)
}

attrisk_prediction_table <- function(log_rate, shape, input_param,
                                     include_hazard = TRUE) {
  rate <- exp(log_rate)
  times <- attrisk_time_grid(input_param)

  phaz <- vapply(
    times$t_yrs,
    function(tt) attrisk_gompertz_cdf(tt, shape = shape, rate = rate),
    numeric(length(rate))
  )

  t2e <- 1 - phaz
  rq <- qfun(phaz)
  t2eq <- qfun(t2e)

  N <- attrisk_input_value(input_param, "n")
  shiny::req(!is.na(N), N > 0)

  c_count <- N * phaz
  at_risk <- N * t2e
  ccq <- qfun(c_count)
  arq <- qfun(at_risk)

  out <- tibble(
    t_yrs = times$t_yrs,
    t_wks = times$t_wks,
    compl_q50 = arq$md,
    drp_q50 = N - compl_q50,
    risk_q025 = rq$lo,
    risk_q50 = rq$md,
    risk_q975 = rq$hi
  )

  if (isTRUE(include_hazard)) {
    haz <- vapply(
      times$t_yrs,
      function(tt) attrisk_gompertz_hazard(tt, shape = shape, rate = rate),
      numeric(length(rate))
    )
    hq <- qfun(haz)
    out <- out %>%
      mutate(
        hazard_q025 = hq$lo,
        hazard_q50 = hq$md,
        hazard_q975 = hq$hi
      )
  }

  out
}

attrisk_feature_table <- function(inputs, value_col_name) {
  inputs %>%
    filter(.data$role == "input") %>%
    mutate(
      display_value = na_if(.data$display_value, ""),
      display_value = replace_na(.data$display_value, "Not specified")
    ) %>%
    arrange(.data$display_order) %>%
    distinct(.data$label, .data$display_value, .keep_all = TRUE) %>%
    transmute(
      !!value_col_name := .data$label,
      Values = .data$display_value
    )
}
