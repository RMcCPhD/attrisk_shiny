
# Prepares user inputs
# Tidying inputs to match naming used for posterior draws
# Formats values according to model covariate specifications

# Build tibble of user inputs upon predict design being activated
tidy_inputs <- function(input) {
  eventReactive(input$predict_design, {
    
    # Get list of inputs
    inputs <- list(
      duration     = input$duration,
      n_log        = log(input$sample),
      n_facilities = log(input$facilities),
      n_arms       = input$arms,
      age_lower    = input$age_lower,
      age_upper    = input$age_upper,
      min_bmi      = input$bmi_lower,
      max_bmi      = input$bmi_upper,
      min_egfr     = input$egfr_lower,
      max_egfr     = input$egfr_upper,
      who_reg      = paste0(input$who_reg, collapse = ", "),
      one_pom      = input$poms,
      trttype      = paste0(input$trttype, collapse = ", "),
      phase        = input$phase,
      blinding     = input$blind
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
          name == "n_arms" & value == "2" & !is.na(value) ~ "1",
          name == "n_arms" & value == "3" & !is.na(value)  ~ "2",
          name == "n_arms" & value == "4" & !is.na(value)  ~ "3",
          name == "n_arms" & !value %in% c("2", "3", "4") ~ "4",
          # Treatment type as ordinal
          name == "trttype" & value == "Placebo"  ~ "0",
          name == "trttype" & value == "Active comparator" ~ "1",
          name == "trttype" & grepl("Placebo", value) & grepl("Active", value)  ~ "2",
          # Age limits as binary value
          name == "age_lower" & grepl(">", value) & !is.na(value)  ~ "1",
          name == "age_lower" & !grepl(">", value) & !is.na(value)  ~ "0",
          name == "age_upper" & grepl(">", value) & !is.na(value)  ~ "1",
          name == "age_upper" & !grepl(">", value) & !is.na(value)  ~ "0",
          # BMI limits as binary value
          (name == "min_bmi" | name == "max_bmi") & value != "None" & !is.na(value)  ~ "1",
          (name == "min_bmi" | name == "max_bmi") & value == "None" & !is.na(value)  ~ "0",
          # eGFR limits as binary variable
          (name == "min_egfr" | name == "max_egfr") & value != "None" & !is.na(value)  ~ "1",
          (name == "min_egfr" | name == "max_egfr") & value == "None" & !is.na(value)  ~ "0",
          # Phase as binary values
          name == "phase" & value == "3" ~ "0",
          name == "phase" & value == "4" ~ "1",
          # POM as bindary variable
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
          name == "who_reg" & grepl("SE", value) ~ "reg_sea",
          name == "who_reg" & grepl("Pacific", value) ~ "reg_wpac"
        )
      ) %>%
      filter(!is.na(value_prep))
  })
}