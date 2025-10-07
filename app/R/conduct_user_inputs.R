
# Prepares user inputs for trial conduct (design features and participant chars)
# Tidying inputs to match naming used for posterior draws
# Formats values according to model covariate specifications

# Build tibble of design feature inputs
tidy_design_inputs <- function(input) {
  eventReactive(input$predict_conduct, {
    
    # Get list of inputs
    inputs <- list(
      duration     = input$"conduct-duration",
      n_log        = log(input$"conduct-sample"),
      n_facilities = log(input$"conduct-facilities"),
      n_arms       = input$"conduct-arms",
      age_lower    = input$"conduct-age_lower",
      age_upper    = input$"conduct-age_upper",
      min_bmi      = input$"conduct-bmi_lower",
      max_bmi      = input$"conduct-bmi_upper",
      min_egfr     = input$"conduct-egfr_lower",
      max_egfr     = input$"conduct-egfr_upper",
      who_reg      = paste0(input$"conduct-who_reg", collapse = ", "),
      one_pom      = input$"conduct-poms",
      trttype      = paste0(input$"conduct-trttype", collapse = ", "),
      phase        = input$"conduct-phase",
      blinding     = input$"conduct-blind"
    )
    
    # Create tibble
    # Set default duration of 26 weeks if missing
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
          # Transform sample size and number of facilities
          name == "n_log" ~ as.character(as.numeric(value)),
          name == "n_facilities" ~ as.character(as.numeric(value)),
          # Number of arms converted to ordinal variable
          name == "n_arms" & value == "2" ~ "1",
          name == "n_arms" & value == "3" ~ "2",
          name == "n_arms" & value == "4" ~ "3",
          name == "n_arms" & !value %in% c("2", "3", "4") & !is.na(value) ~ "4",
          # Treatment type as ordinal
          name == "trttype" & value == "Placebo"  ~ "0",
          name == "trttype" & value == "Active comparator" ~ "1",
          name == "trttype" & grepl("Placebo", value) & grepl("Active", value)  ~ "2",
          # Age limits as binary value
          name == "age_lower" & value == "18yr" ~ "0",
          name == "age_lower" & value == ">18yr" ~ "1",
          name == "age_upper" & value == "70yr" ~ "0",
          name == "age_upper" & value == ">70yr" ~ "1",
          # BMI limits as binary value
          (name == "min_bmi" | name == "max_bmi") & value != "None"  ~ "1",
          (name == "min_bmi" | name == "max_bmi") & value == "None" ~ "0",
          # eGFR limits as binary variable
          name == "min_egfr" & value == "None" ~ "0",
          name == "min_egfr" & value == "<=30 ml/min/1.73m2" ~ "1",
          name == "min_egfr" & value == ">30 ml/min/1.73m2"  ~ "2",
          name == "max_egfr" & value == "None" ~ "0",
          name == "max_egfr" & value == "<=60 ml/min/1.73m2" ~ "1",
          name == "max_egfr" & value == ">60 ml/min/1.73m2"  ~ "2",
          # Phase as binary values
          name == "phase" & value == "3" ~ "0",
          name == "phase" & value == "4" ~ "1",
          # POM as bindary variable
          name == "one_pom" & value != "1" & !is.na(value)  ~ "1",
          name == "one_pom" & value == "1" & !is.na(value)  ~ "0",
          # Blinding as level name in beta dataset
          name == "blinding" & value == "Open label"  ~ "0",
          name == "blinding" & !is.na(value) & value != "Open label" ~ "1",
          # WHO regions as binary values
          name == "who_reg" & !is.na(value) ~ "1",
          TRUE ~ value
        )
      )
    
    # Prepare param variable for joining with betas
    design_param <- input_tidy %>%
      mutate(value = if_else(name == "trttype", NA_character_, value)) %>%
      separate_rows(value, sep = ", ") %>%
      mutate(
        param = case_when(
          !name %in% c("who_reg", "blinding") ~ name,
          # Blinding
          name == "blinding" & value == "Open label" ~ "blindingopenlabel",
          name == "blinding" & value == "Double" ~ "blindingdouble",
          name == "blinding" & value == "Triple" ~ "blindingtriple",
          name == "blinding" & value == "Quadruple" ~ "blindingquadruple",
          # WHO regions
          name == "who_reg" & grepl("Africa", value) ~ "reg_afr",
          name == "who_reg" & grepl("Americas", value) ~ "reg_am",
          name == "who_reg" & grepl("Medi", value) ~ "reg_em",
          name == "who_reg" & grepl("Europ", value) ~ "reg_eur",
          name == "who_reg" & grepl("SE", value) ~ "reg_sea",
          name == "who_reg" & grepl("Pacific", value) ~ "reg_wpac"
        )
      ) %>%
      filter(!is.na(value_prep))
  })
}

# Build tibble of participant characteristic inputs
tidy_char_inputs <- function(input) {
  eventReactive(input$predict_conduct, {
    
    # Get list of inputs
    inputs <- list(
      age     = input$"conduct-age",    
      t2dur   = input$"conduct-t2dur",     
      hba1c   = input$"conduct-hba1c",
      hdl     = input$"conduct-hdl",
      bmi     = input$"conduct-bmi",
      egfr    = input$"conduct-egfr",
      sbp     = input$"conduct-sbp",    
      totalc  = input$"conduct-totalc",    
      conmed  = input$"conduct-conmed",   
      comorbs = input$"conduct-comorbs",
      sex     = input$"conduct-sex",
      smoke   = input$"conduct-smoke",
      race    = input$"conduct-race",
      trt     = input$"conduct-trttype"
    )
    
    # Create tibble
    input_tbl <- as_tibble(bind_rows(inputs)) %>%
      mutate(
        across(everything(), as.character)
      ) %>%
      pivot_longer(everything()) %>%
      filter(value != "NA") %>% 
      distinct() %>%
      arrange()
    
    # Tidy values for user inputs to match beta names
    # Conditions ignored if input is NA
    conduct_param <- input_tbl %>%
      mutate(
        value_prep = case_when(
          # Scale age (in decades), t2dur, bmi, egfr and sbp (SDs)
          name == "age" ~ as.character(as.numeric(value)/10),
          name == "t2dur" ~ as.character(as.numeric(value)/6.14),
          name == "bmi" ~ as.character(as.numeric(value)/5.57),
          name == "egfr" ~ as.character(as.numeric(value)/20.59),
          name == "sbp" ~ as.character(as.numeric(value)/15.94),
          # Sex, smoke and trt as integers
          name == "sex"   & value == "Female" ~ "0",
          name == "sex"   & value == "Male" ~ "1",
          name == "smoke" & value == "No" ~ "0",
          name == "smoke" & value == "Yes" ~ "1",
          name == "trt"   & value == "Placebo" ~ "0",
          name == "trt"   & value == "Active comparator" ~ "1",
          # Race as integers (since can only select a single race)
          name == "race"  & value == "White" ~ "0",
          name == "race"  & value != "White" & (value != "" | !is.na(value)) ~ "1",
          TRUE ~ value
        )
      )
  })
}
