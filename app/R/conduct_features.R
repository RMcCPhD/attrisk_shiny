
# Save a table of trial design features and characteristics that generated outputs
c_features <- function(conduct_inputs) {
  conduct_inputs %>%
    select(features_chars = name, value, value_prep) %>% 
    mutate(
      value = case_when(
        features == "sample" 
        & as.numeric(value_prep) < 500 ~ "Small (<500)",
        features == "sample" 
        & as.numeric(value_prep) >= 500 
        & as.numeric(value_prep)  < 1000 ~ "Medium (500 – 999)",
        features == "sample" 
        & as.numeric(value_prep) >= 1000 ~ "Large (≥1000)",
        is.na(value) & value_prep == "0" ~ "Placebo",
        is.na(value) & value_prep == "1" ~ "Active comparator",
        is.na(value) & value_prep == "2" ~ "Placebo, Active comparator",
        TRUE ~ value
      ),
      features_chars = if_else(features_chars == "one_pom", "n_poms", features_chars)
    ) %>% 
    select(-value_prep) %>% 
    group_by(features_chars) %>% 
    mutate(value = paste0(value, collapse = ", ")) %>% 
    ungroup() %>% 
    distinct() %>% 
    mutate(
      features_chars = factor(
        features_chars,
        levels = c(
          "duration", "n_arms", "n", "sample",
          "age_lower", "age_upper", "min_bmi", "max_bmi",
          "min_egfr", "max_egfr", "phase", "n_poms",
          "n_facilities", "trttype",
          "blinding", "who_reg",
          "age10", "sex", "race", "current",
          "t2dur", "hba1c", "egfr",
          "bmi", "hdl", "total", "sbp",
          "comorbs", "n_atc"
        )
      )
    ) %>% 
    arrange(features_chars) %>% 
    mutate(
      features_chars = case_match(
        features_chars,
        "duration" ~ "Duration (weeks)",
        "n_arms" ~ "Number of arms",
        "n" ~ "Number of participants",
        "sample" ~ "Trial size",
        "n_facilities" ~ "Number of sites",
        "age_lower" ~ "Lower age limits",
        "age_upper" ~ "Upper age limits*",
        "min_bmi" ~ "Lower BMI limits",
        "max_bmi" ~ "Upper BMI limits",
        "min_egfr" ~ "Lower eGFR limits",
        "max_egfr" ~ "Upper eGFR limits",
        "who_reg" ~ "WHO region",
        "n_poms" ~ "Number of primary outcomes",
        "trttype" ~ "Comparator",
        "phase" ~ "Phase",
        "blinding" ~ "Blinding",
        "age10" ~ "Age (years)*",
        "t2dur" ~ "T2DM duration (years)*",
        "hba1c" ~ "HbA1c (%)*",
        "hdl" ~ "HDL cholesterol (mmol/L)",
        "bmi" ~ "BMI (kg/m2)",
        "egfr" ~ "eGFR (ml/min/1.73m2)*",
        "sbp" ~ "Systolic blood pressure (mmHg)*",
        "total" ~ "Total cholesterol (mmol/L)",
        "comorbs" ~ "Number of comorbidities",
        "n_atc" ~ "Number of concomitant medications*",
        "sex" ~ "Sex*",
        "current" ~ "Current smoker*",
        "race" ~ "Race"
      )
    )
}