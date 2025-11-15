
library(shiny)
library(shinydashboard)
library(tidyverse)
library(posterior)
library(DT)

# Define server logic
function(input, output, session) {
  
  # Disclaimer -----------------------------------------------------------------
  
  # Load disclaimer text
  disclaimer_text <- readLines("www/text/disclaimer.txt") %>% paste(collapse = "<br>")
  
  # Show disclaimer, app loads once user acknowledges
  observeEvent(TRUE, {
    shinyalert(
      title = "Disclaimer",
      type  = "warning",
      closeOnClickOutside = FALSE,
      showCancelButton = FALSE,
      confirmButtonText = "I understand",
      confirmButtonCol = "#3c8dbc",
      size = "m",
      text = HTML(disclaimer_text),
      html = TRUE
    )
  }, once = TRUE)

  # Design page conditions -----------------------------------------------------
  
  # Update action buttons (disabled -> usable) based on inputs
  observe({
    have_duration <- isTruthy(input$"design_duration")
    have_arms <- isTruthy(input$"design_arms")
    have_n <- isTruthy(input$"design_n")
    have_all <- have_duration & have_arms & have_n
    inputs_valid <- design_iv$is_valid()
    enable_buttons <- have_all && inputs_valid
    
    # Toggle enabled/disabled
    shinyjs::toggleState("predict_design", condition = enable_buttons)
    shinyjs::toggleState("reset_design", condition = enable_buttons)
    
    # Choose labels based on what input is missing (duration, sample size)
    lab_needed <- if (!have_duration) {
      "Enter duration"
    } else if (!have_arms) {
      "Enter arms"
    } else if (!have_n) {
      "Enter sample size"
    } else if (have_all & !inputs_valid) {
      "Fix value(s)"
    } else {
      "Predict (design)"
    }
    
    # Predict button
    updateActionButton(
      session, "predict_design",
      label = if (enable_buttons) "Predict risk" else lab_needed
    )
    
    # Reset button
    updateActionButton(
      session, "reset_design",
      label = if (enable_buttons) "Reset everything" else lab_needed
    )
  })
  
  # Display warning if numeric inputs fall outside of possible range
  design_value_limits <- list(
    "design_duration"   = c(12, 520),
    "design_facilities" = c(1, 500),
    "design_arms"       = c(2, 6),
    "design_n"          = c(100, 10000)
  )
  
  design_iv <- shinyvalidate::InputValidator$new()
  for (id in names(design_value_limits)) {
    lims <- design_value_limits[[id]]
    design_iv$add_rule(
      id,
      shinyvalidate::sv_between(
        lims[1], lims[2],
        allow_na = TRUE,
        message_fmt = paste0("Enter a value between ", lims[1], " and ", lims[2]
      )
      )
    )
  }
  
  design_iv$enable()
  
  # Assign a sample size category based on user input for actual sample size
  # Defaults:
  # 250 (for reference - small (<500), 750 (medium (500-999), 1250 (large (>1000)))
  output$design_sample_size <- renderText({
    val <- input$design_n
    if (is.null(val) || is.na(val)) return("Enter number of participants")
    case_when(
      val < 500 ~ "Small (<500)",
      val >= 500 & val < 1000 ~ "Medium (500 – 999)",
      val >= 1000 ~ "Large (≥1000)"
    )
  })
  
  # Conditions to remove outputs when reset button is pressed
  design_plots_visible <- reactiveVal(FALSE)
  design_sum_visible <- reactiveVal(FALSE)
  
  observeEvent(input$predict_design, {
    design_plots_visible(TRUE)
    design_sum_visible(TRUE)
  })
  
  observeEvent(input$reset_design, {
    design_plots_visible(FALSE)
    design_sum_visible(FALSE)
    updateCheckboxInput(session, "overlay_top5", value = FALSE)
    updateCheckboxInput(session, "save_outputs", value = FALSE)
    updateCheckboxInput(session, "save_select", value = FALSE)
  })
  
  # Predict design -------------------------------------------------------------
  
  # Clear inputs when reset buttons are used
  observeEvent(input$reset_design, {
    session$sendCustomMessage("resetBox", "design_box")
  })
  
  # Prepare user inputs
  design_inputs <- tidy_inputs(input)
  
  # Get Gompertz predictions
  design_predict <- predict_design(design_inputs)
  
  # Get risk estimates for 5 most similar trials (in terms of design features)
  top5 <- predict_top5(design_inputs, reactive(input$overlay_top5))
  
  # Print ctgov for 5 most similar trials in sidebar (if selected)
  output$top5_ids <- renderUI({
    req(input$overlay_top5)
    d5 <- top5()
    req(!is.null(d5), nrow(d5) > 0)
    
    # Get the 5 unique trial IDs and risk in the order they were selected
    ids <- d5 %>% select(trial_id, risk_q50)
    
    # Build a neat ordered list of links to ClinicalTrials.gov
    tags$ol(
      style = "padding-left: 18px; margin: 0;",
      lapply(seq_len(nrow(ids)), function(i) {
        
        id <- ids$trial_id[i]
        risk <- ids$risk_q50[i]
        risk_txt <- sprintf("%.2f%%", risk)
        
        tags$li(
          tags$a(
            href = paste0("https://clinicaltrials.gov/study/", id),
            target = "_blank",
            style = "text-decoration:none;",
            sprintf("%s - %s", id, risk_txt)
          )
        )
      })
    )
  })
  
  # Render plots
  output$plot_risk_design <- renderPlot({
    req(design_plots_visible())
    design_risk_plot(
      design_df = design_predict(), 
      top5_df   = top5(), 
      show_top5 = isTRUE(input$overlay_top5)
    )
  })
  
  output$plot_hazard_design <- renderPlot({
    req(design_plots_visible())
    design_haz_plot(design_predict())
  })
  
  # Render summary table
  output$sum_design <- DT::renderDT({
    req(design_sum_visible())
    design_sum(design_predict())$dt
  })
  
  # Save outputs user has selected
  init_save_outputs(
    input, output, session,
    design_predict  = design_predict,
    top5            = top5,
    risk_plot_fun   = design_risk_plot,
    hazard_plot_fun = design_haz_plot,
    sum_fun         = design_sum,
    design_inputs,
    overlay_flag    = reactive(isTRUE(input$overlay_top5))
  )
  
  # Conduct page conditions ----------------------------------------------------
  
  # Update action buttons (disabled -> usable) based on inputs
  observe({
    
    # Conditions
    have_duration <- isTruthy(input$"conduct_duration")
    have_arms <- isTruthy(input$"conduct_arms")
    have_n <- isTruthy(input$"conduct_n")
    have_age <- isTruthy(input$"conduct_age")
    have_all <- have_duration & have_arms & have_n & have_age
    inputs_valid <- conduct_iv$is_valid()
    enable_buttons <- have_all && inputs_valid
    
    # Toggle enabled/disabled
    shinyjs::toggleState("predict_conduct", condition = enable_buttons)
    shinyjs::toggleState("reset_design2", condition = enable_buttons)
    shinyjs::toggleState("reset_char", condition = enable_buttons)
    shinyjs::toggleState("reset_conduct", condition = enable_buttons)
    
    # Choose labels based on what input is missing (duration, sample size)
    lab_needed <- if (!have_duration) {
      "Enter duration"
    } else if (!have_n) {
      "Enter sample size"
    } else if (!have_arms) {
      "Enter arms"
    } else if (!have_age) {
      "Enter age"
    } else if (have_all & !inputs_valid) {
      "Fix value(s)"
    } else {
      "Predict risk"
    }
    
    # Predict button
    updateActionButton(
      session, "predict_conduct",
      label = if (enable_buttons) "Predict risk" else lab_needed
    )
    
    # Reset buttons
    updateActionButton(
      session, "reset_design2",
      label = if (enable_buttons) "Reset design features" else lab_needed
    )
    
    updateActionButton(
      session, "reset_char",
      label = if (enable_buttons) "Reset characteristics" else lab_needed
    )
    
    updateActionButton(
      session, "reset_conduct",
      label = if (enable_buttons) "Reset everything" else lab_needed
    )
  })
  
  # Display warning if numeric inputs fall outside of possible range
  conduct_value_limits <- list(
    "conduct_duration"   = c(12, 520),
    "conduct_arms"       = c(2, 6),
    "conduct_n"          = c(100, 10000),
    "conduct_age"        = c(18, 100),
    "conduct_t2dur"      = c(0.01, 20),
    "conduct_hba1c"      = c(6, 10),
    "conduct_hdl"        = c(1, 4),
    "conduct_bmi"        = c(18, 50),
    "conduct_egfr"       = c(15, 120),
    "conduct_sbp"        = c(90, 200),
    "conduct_totalc"     = c(4, 10),
    "conduct_conmed"     = c(0, 20),
    "conduct_comorbs"    = c(0, 10)
  )
  
  conduct_iv <- shinyvalidate::InputValidator$new()
  for (id in names(conduct_value_limits)) {
    lims <- conduct_value_limits[[id]]
    conduct_iv$add_rule(
      id,
      shinyvalidate::sv_between(
        lims[1], lims[2],
        allow_na = TRUE,
        message_fmt = paste0("Enter a value between ", lims[1], " and ", lims[2]
        )
      )
    )
  }
  
  conduct_iv$enable()
  
  # Assign a sample size category based on user input for actual sample size
  # Defaults:
  # 250 (for reference - small (<500), 750 (medium (500-999), 1250 (large (>1000)))
  output$conduct_sample_size <- renderText({
    val <- input$conduct_n
    if (is.null(val) || is.na(val)) return("Enter number of participants")
    case_when(
      val < 500 ~ "Small (<500)",
      val >= 500 & val < 1000 ~ "Medium (500 – 999)",
      val >= 1000 ~ "Large (≥1000)"
    )
  })
  
  # Conditions to remove outputs when reset button is pressed
  conduct_plots_visible <- reactiveVal(FALSE)
  conduct_sum_visible <- reactiveVal(FALSE)
  
  observeEvent(input$predict_conduct, {
    conduct_plots_visible(TRUE)
    conduct_sum_visible(TRUE)
  })
  
  observeEvent(input$reset_conduct, {
    conduct_plots_visible(FALSE)
    conduct_sum_visible(FALSE)
    updateCheckboxInput(session, "save_outputs2", value = FALSE)
    updateCheckboxInput(session, "save_select2", value = FALSE)
  })
  
  # Predict conduct ------------------------------------------------------------
  
  # Clear inputs when reset buttons are used
  observeEvent(input$reset_design2, {
    session$sendCustomMessage("resetBox2", "design2_box")
  })
  
  observeEvent(input$reset_char, {
    session$sendCustomMessage("resetBox3", "char_box")
  })
  
  observeEvent(input$reset_conduct, {
    session$sendCustomMessage("resetBox2", "design2_box")
    session$sendCustomMessage("resetBox3", "char_box")
  })
  
  # Prepare inputs
  conduct_inputs <- tidy_inputs_conduct(input)
  
  # Get pre-CITL Gompertz parameters
  conduct_pre <- conduct_design_params(conduct_inputs)
  
  # CITL and risk estimates
  conduct_citl <- predict_conduct(conduct_pre, conduct_inputs)
  
  # Render plots
  output$plot_risk_conduct <- renderPlot({
    req(conduct_plots_visible())
    conduct_risk_plot(
      conduct_df = conduct_citl()
    )
  })
  
  # Render summary table
  output$sum_conduct <- DT::renderDT({
    req(conduct_sum_visible())
    conduct_sum(conduct_citl())$dt
  })
  
  # Save outputs user has selected
  init_save_outputs2(
    input, output, session,
    conduct_predict = conduct_citl,
    risk_plot_fun   = conduct_risk_plot,
    sum_fun         = conduct_sum,
    conduct_inputs
  )
  
  # User guide -----------------------------------------------------------------
  
  # Title
  output$predict_intro <- renderUI({
    predict_path <- "www/text/predict_intro.txt"
    imp_predict <- readLines(predict_path)
    tagList(lapply(imp_predict, p))
  })
  
  # About ----------------------------------------------------------------------
  
  output$about_attrisk <- renderUI({
    about_path <- "www/text/about.txt"
    imp_about <- readLines(about_path)
    tagList(lapply(imp_about, p))
  })
  
  # Authors text
  output$authors_text <- renderUI({
    authors_path <- "www/text/authors.txt"
    imp_authors <- readLines(authors_path)
    HTML(
      paste0(
        "<ul>", 
        paste(sprintf("<li>%s</li>", imp_authors), collapse = ""), 
        "</ul>"
      )
    )
  })
  
  # Feedback and suggestions
  output$fback_sugg <- renderUI({
    fback_path <- "www/text/feedback_suggestions.txt"
    imp_fback <- readLines(fback_path)
    tagList(lapply(imp_fback, p))
  })
  
}