
# Prepares user inputs for trial conduct (design features and participant chars)
# Tidying inputs to match naming used for posterior draws
# Formats values according to model covariate specifications

tidy_inputs_conduct <- function(input) {
  eventReactive(input$predict_conduct, {
    
    # Get list of inputs
    inputs <- list(
      # Design features
      duration     = input$"conduct-duration",
      n_arms       = input$"conduct-arms",
      est_sample   = input$"conduct-est_sample",
      sample       = input$"conduct-sample_size",
      n_facilities = input$"conduct-n_facilities",
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
      blinding     = input$"conduct-blind",
      # Baseline characteristics
      age10   = input$"conduct-age",    
      t2dur   = input$"conduct-t2dur",     
      hba1c   = input$"conduct-hba1c",
      hdl     = input$"conduct-hdl",
      bmi     = input$"conduct-bmi",
      egfr    = input$"conduct-egfr",
      sbp     = input$"conduct-sbp",    
      total   = input$"conduct-totalc",    
      conmed  = input$"conduct-conmed",   
      n_atc   = input$"conduct-comorbs",
      sex     = input$"conduct-sex",
      current = input$"conduct-smoke",
      race    = input$"conduct-race"
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
    input_tidy <- input_tbl %>%
      mutate(
        value_prep = case_when(
          # Duration in years
          name == "duration" ~ as.character(as.numeric(value)/52),
          # Sample size - ordinal
          name == "sample" & value == "Small (<500)" ~ "0",
          name == "sample" & value == "Medium (500 - 999)" ~ "1",
          name == "sample" & value == "Large (>=1000)" ~ "2",
          # Number of facilities - ordinals
          name == "n_facilities" & value == "<50" ~ "0",
          name == "n_facilities" & value == "50 - 99" ~ "1",
          name == "n_facilities" & value == ">=100" ~ "2",
          # Number of arms - ordinal
          name == "n_arms" & value == "2" ~ "0",
          name == "n_arms" & value == "3" ~ "1",
          name == "n_arms" & value == "4" ~ "2",
          name == "n_arms" & !value %in% c("2", "3", "4") & !is.na(value) ~ "3",
          # Treatment type - ordinal
          name == "trttype" & value == "Placebo"  ~ "0",
          name == "trttype" & value == "Active comparator" ~ "1",
          name == "trttype" & grepl("Placebo", value) & grepl("Active", value)  ~ "2",
          # Age limits - binary
          name == "age_lower" & value == "18yr" ~ "0",
          name == "age_lower" & value == ">18yr" ~ "1",
          name == "age_upper" & value == "70yr" ~ "0",
          name == "age_upper" & value == ">70yr" ~ "1",
          # BMI limits - binary
          (name == "min_bmi" | name == "max_bmi") & value != "None"  ~ "1",
          (name == "min_bmi" | name == "max_bmi") & value == "None" ~ "0",
          # eGFR limits - binary
          name == "min_egfr" & value == "None" ~ "0",
          name == "min_egfr" & value == "<=30 ml/min/1.73m2" ~ "1",
          name == "min_egfr" & value == ">30 ml/min/1.73m2"  ~ "2",
          name == "max_egfr" & value == "None" ~ "0",
          name == "max_egfr" & value == "<=60 ml/min/1.73m2" ~ "1",
          name == "max_egfr" & value == ">60 ml/min/1.73m2"  ~ "2",
          # Phase - binary
          name == "phase" & value == "3" ~ "0",
          name == "phase" & value == "4" ~ "1",
          # POM - binary
          name == "one_pom" & value != "1" & !is.na(value)  ~ "1",
          name == "one_pom" & value == "1" & !is.na(value)  ~ "0",
          # Blinding as factor (i.e. individual binary variables)
          name == "blinding" & value == "Open label"  ~ "0",
          name == "blinding" & !is.na(value) & value != "Open label" ~ "1",
          # WHO regions as factor
          name == "who_reg" & (!is.na(value) | value == "") ~ "0",
          # Scale age (in decades), t2dur, bmi, egfr and sbp (SDs)
          name == "age10" ~ as.character(as.numeric(value)/10),
          name == "t2dur" ~ as.character(as.numeric(value)/6.14),
          name == "bmi" ~ as.character(as.numeric(value)/5.57),
          name == "egfr" ~ as.character(as.numeric(value)/20.59),
          name == "sbp" ~ as.character(as.numeric(value)/15.94),
          # Sex, smoke and trt as integers
          name == "sex"   & value == "Female" ~ "0",
          name == "sex"   & value == "Male" ~ "1",
          name == "current" & value == "No" ~ "0",
          name == "current" & value == "Yes" ~ "1",
          # Race as integers (since can only select a single race)
          name == "race"  & value == "White" ~ "0",
          name == "race"  & value != "White" & (value != "" | !is.na(value)) ~ "1",
          TRUE ~ value
        )
      )
    
    # Prepare param variable for joining with betas
    input_param <- input_tidy %>%
      mutate(value = if_else(name == "trttype", NA_character_, value)) %>%
      separate_rows(value, sep = ", ") %>%
      mutate(
        param = case_when(
          !name %in% c("who_reg", "blinding", "race") ~ name,
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
          name == "who_reg" & grepl("Pacific", value) ~ "reg_wpac",
          # Race
          name == "race" & value == "White" ~ "white",
          name == "race" & value == "Black/African American*" ~ "black_aa",
          name == "race" & value == "Asian*" ~ "asian",
          name == "race" & value == "Other" ~ "other"
        )
      ) %>%
      filter(!is.na(value_prep))
  })
}
