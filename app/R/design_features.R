
# Save a table of trial design features that generated outputs
d_features <- function(design_inputs) {
  design_inputs %>% 
    select(features = name, value, value_prep) %>% 
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
      features = if_else(features == "one_pom", "n_poms", features)
    ) %>% 
    select(-value_prep) %>% 
    group_by(features) %>% 
    mutate(value = paste0(value, collapse = ", ")) %>% 
    ungroup() %>% 
    distinct() %>% 
    mutate(
      features = factor(
        features,
        levels = c(
          "duration", "n_arms", "n", "sample",
          "age_lower", "age_upper", "min_bmi", "max_bmi",
          "min_egfr", "max_egfr", "phase", "n_poms",
          "n_facilities", "trttype",
          "blinding", "who_reg"
        )
      )
    ) %>% 
    arrange(features) %>% 
    mutate(
      features = case_match(
        features,
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
        "who_reg" ~ "WHO regions",
        "n_poms" ~ "Number of primary outcomes",
        "trttype" ~ "Comparator",
        "phase" ~ "Phase",
        "blinding" ~ "Blinding"
      )
    ) %>% 
    rename(`Design features` = features, Values = value)
}