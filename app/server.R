
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
    have_duration <- isTruthy(input$"design-duration")
    
    # Toggle enabled/disabled
    shinyjs::toggleState("predict_design", condition = have_duration)
    shinyjs::toggleState("reset_design", condition = have_duration)
    
    # Choose labels based on what input is missing (duration, sample size)
    lab_needed <- if (!have_duration) {
      "Enter duration"
    } else {
      "Predict (design)"
    }
    
    # Predict button
    updateActionButton(
      session, "predict_design",
      label = if (have_duration) "Predict risk" else lab_needed
    )
    
    # Reset button
    updateActionButton(
      session, "reset_design",
      label = if (have_duration) "Reset everything" else lab_needed
    )
  })
  
  # Force minimum/maximum values for numeric inputs where violated
  value_limits <- list(
    "design-duration"   = c(12, 520),
    "design-facilities" = c(1, 500),
    "design-arms"       = c(2, 6)
  )
  
  # Clamp input if thresholds exceeded
  lapply(names(value_limits), function(id) {
    observeEvent(input[[id]], ignoreInit = TRUE, {
      val <- input[[id]]
      if (is.null(val) || is.na(val)) return(NULL)
      low <- value_limits[[id]][1]
      high <- value_limits[[id]][2]
      
      if (val < low) {
        updateNumericInput(session, id, value = low)
      } else if (val > high) {
        updateNumericInput(session, id, value = high)
      }
    })
  })
  
  # Assign an estimated sample size value based on sample size input
  # Defaults:
  # 250 (for reference - small (<500), 750 (medium (500-999), 1250 (large (>1000)))
  
  # Set defaults
  sample_defaults <- list(
    "Small (<500)"        = list(value = 250,  min = 30,   max = 499),
    "Medium (500 - 999)"  = list(value = 750,  min = 500,  max = 999),
    "Large (>=1000)"      = list(value = 1250, min = 1000, max = 10000)
  )
  
  # Apply defaults upon app loading with reference set (small)
  observe({
    req(isTruthy(input$`design-sample_size`))
    d <- sample_defaults[[input$`design-sample_size`]]
    updateNumericInput(
      session, "design-est_sample",
      value = d$value, min = d$min, max = d$max
    )
  })
  
  # Update default when radio changes
  observeEvent(input$`design-sample_size`, {
    d <- sample_defaults[[input$`design-sample_size`]]
    current <- isolate(input$`design-est_sample`)
    
    # Keep the user's value if it fits, otherwise use default
    new_value <- if (!is.null(current) && !is.na(current) &&
                     current >= d$min && current <= d$max) current else d$value
    
    updateNumericInput(
      session, "design-est_sample",
      value = new_value, min = d$min, max = d$max
    )
  })
  
  # Clamp user entries to bounds for each radio button
  observeEvent(input$`design-est_sample`, {
    d <- sample_defaults[[ input$`design-sample_size` ]]
    val <- input$`design-est_sample`
    
    # If box is cleared, reset to default for radio button
    if (is.null(val) || is.na(val) || val == "") {
      updateNumericInput(session, "design-est_sample", value = d$value)
      return()
    }
    
    # Otherwise clamp to boundaries
    clamped <- max(d$min, min(d$max, val))
    if (!identical(val, clamped)) {
      updateNumericInput(session, "design-est_sample", value = clamped)
    }
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
    overlay_flag    = reactive(isTRUE(input$overlay_top5))
  )
  
  # Conduct page conditions ----------------------------------------------------
  
  # Update action buttons (disabled -> usable) based on inputs
  observe({
    have_duration <- isTruthy(input$"conduct-duration")
    have_age <- isTruthy(input$"conduct-age")
    have_both <- have_duration & have_age
    
    # Toggle enabled/disabled
    shinyjs::toggleState("predict_conduct", condition = have_both)
    shinyjs::toggleState("reset_char", condition = have_both)
    shinyjs::toggleState("reset_conduct", condition = have_both)
    
    # Choose labels based on what input is missing (duration, sample size)
    lab_needed <- if (!have_duration) {
      "Enter duration"
    } else if (!have_age) {
      "Enter age"
    } else {
      "Predict risk"
    }
    
    # Predict button
    updateActionButton(
      session, "predict_conduct",
      label = if (have_both) "Predict risk" else lab_needed
    )
    
    # Reset button
    updateActionButton(
      session, "reset_char",
      label = if (have_both) "Reset characteristics" else lab_needed
    )
    
    updateActionButton(
      session, "reset_conduct",
      label = if (have_both) "Reset everything" else lab_needed
    )
  })
  
  # Force minimum/maximum values for numeric inputs where violated
  conduct_value_limits <- list(
    "conduct-duration"   = c(12, 520),
    "conduct-arms"       = c(2, 6),
    "conduct-age"        = c(18, 100),
    "conduct-t2dur"      = c(0.01, 5),
    "conduct-hba1c"      = c(6, 10),
    "conduct-hdl"        = c(1, 3),
    "conduct-bmi"        = c(19, 50),
    "conduct-egfr"       = c(60, 120),
    "conduct-sbp"        = c(60, 140),
    "conduct-totalc"     = c(4, 10),
    "conduct-conmed"     = c(0, 20),
    "conduct-comorbs"    = c(0, 10)
  )
  
  # Clamp input if thresholds exceeded
  lapply(names(conduct_value_limits), function(id) {
    observeEvent(input[[id]], ignoreInit = TRUE, {
      val <- input[[id]]
      if (is.null(val) || is.na(val)) return(NULL)
      low <- conduct_value_limits[[id]][1]
      high <- conduct_value_limits[[id]][2]

      if (val < low) {
        updateNumericInput(session, id, value = low)
      } else if (val > high) {
        updateNumericInput(session, id, value = high)
      }
    })
  })
  
  # Apply defaults upon app loading with reference set (small)
  observe({
    req(isTruthy(input$`conduct-sample_size`))
    d <- sample_defaults[[input$`conduct-sample_size`]]
    updateNumericInput(
      session, "conduct-est_sample",
      value = d$value, min = d$min, max = d$max
    )
  })
  
  # Update default when radio changes
  observeEvent(input$`conduct-sample_size`, {
    d <- sample_defaults[[ input$`conduct-sample_size` ]]
    current <- isolate(input$`conduct-est_sample`)
    
    # Keep the user's value if it fits, otherwise use default
    new_value <- if (!is.null(current) && !is.na(current) &&
                     current >= d$min && current <= d$max) current else d$value
    
    updateNumericInput(
      session, "conduct-est_sample",
      value = new_value, min = d$min, max = d$max
    )
  })
  
  # Clamp user entries to bounds for each radio button
  observeEvent(input$`conduct-est_sample`, {
    d <- sample_defaults[[ input$`conduct-sample_size` ]]
    val <- input$`conduct-est_sample`
    
    # If box is cleared, reset to default for radio button
    if (is.null(val) || is.na(val) || val == "") {
      updateNumericInput(session, "conduct-est_sample", value = d$value)
      return()
    }
    
    # Otherwise clamp to boundaries
    clamped <- max(d$min, min(d$max, val))
    if (!identical(val, clamped)) {
      updateNumericInput(session, "conduct-est_sample", value = clamped)
    }
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
    updateCheckboxInput(session, "overlay_top5", value = FALSE)
  })
  
  # Predict conduct ------------------------------------------------------------
  
  # Clear inputs when reset buttons are used
  observeEvent(input$reset_char, {
    session$sendCustomMessage("resetBox2", "design2_box")
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
    sum_fun         = conduct_sum
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