
# Prepares user inputs for trial design
# Tidying inputs to match naming used for posterior draws
# Formats values according to model covariate specifications

# Build tibble of user inputs upon predict design being activated
tidy_inputs <- function(input) {
  eventReactive(input$predict_design, {
    
    # Get list of inputs
    inputs <- list(
      duration     = input$"design-duration",
      n_arms       = input$"design-arms",
      est_sample   = input$"design-est_sample",
      sample       = input$"design-sample",
      n_facilities = input$"design-n_facilities",
      age_lower    = input$"design-age_lower",
      age_upper    = input$"design-age_upper",
      min_bmi      = input$"design-bmi_lower",
      max_bmi      = input$"design-bmi_upper",
      min_egfr     = input$"design-egfr_lower",
      max_egfr     = input$"design-egfr_upper",
      who_reg      = paste0(input$"design-who_reg", collapse = ", "),
      one_pom      = input$"design-poms",
      trttype      = paste0(input$"design-trttype", collapse = ", "),
      phase        = input$"design-phase",
      blinding     = input$"design-blind"
    )
    
    # Create tibble of inputs
    input_tbl <- as_tibble(bind_rows(inputs)) %>%
      mutate(
        across(c(who_reg, trttype), ~ if_else(. == "", NA_character_, .)),
        across(c(who_reg, trttype), ~ paste0(unique(.), collapse = ", ")),
        across(everything(), as.character)
      ) %>%
      pivot_longer(everything()) %>%
      filter(value != "NA") %>% 
      distinct() %>%
      arrange()
    
    # Transform values
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
          name == "n_arms" & value == "2" ~ "1",
          name == "n_arms" & value == "3" ~ "2",
          name == "n_arms" & value == "4" ~ "3",
          name == "n_arms" & !value %in% c("2", "3", "4") & !is.na(value) ~ "4",
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
          name == "who_reg" & !is.na(value) ~ "1",
          TRUE ~ value
        )
      )
    
    # Prepare param variable for joining with betas
    input_param <- input_tidy %>%
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
    
    print(input_param)
  })
}