
library(shiny)
library(shinydashboard)
library(tidyverse)
library(posterior)
library(DT)

# Define server logic
function(input, output, session) {
  
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
    d <- sample_defaults[[ input$`design-sample_size` ]]
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
  plots_visible <- reactiveVal(FALSE)
  sum_visible <- reactiveVal(FALSE)
  
  observeEvent(input$predict_design, {
    plots_visible(TRUE)
    sum_visible(TRUE)
  })
  
  observeEvent(input$reset_design, {
    plots_visible(FALSE)
    sum_visible(FALSE)
    updateCheckboxInput(session, "overlay_top5", value = FALSE)
  })
  
  # Predict design -------------------------------------------------------------
  
  # Clear inputs when reset buttons are used
  observeEvent(input$reset_design, {
    session$sendCustomMessage("resetBox2", "design_box")
  })
  
  observeEvent(input$reset_char, {
    session$sendCustomMessage("resetBox3", "char_box")
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
    
    # Get the 5 unique trial IDs in the order they were selected
    ids <- d5 %>% pull(trial_id)
    
    # Build a neat ordered list of links to ClinicalTrials.gov
    tags$ol(
      style = "padding-left: 18px; margin: 0;",
      lapply(ids, function(id) {
        tags$li(
          tags$a(href = paste0("https://clinicaltrials.gov/study/", id),
                 target = "_blank",
                 style = "text-decoration:none;",
                 id)
        )
      })
    )
  })
  
  # Render plots
  output$plot_risk_design <- renderPlot({
    req(plots_visible())
    design_risk_plot(
      design_df = design_predict(), 
      top5_df   = top5(), 
      show_top5 = isTRUE(input$overlay_top5)
    )
  })
  
  output$plot_hazard_design <- renderPlot({
    req(plots_visible())
    design_haz_plot(design_predict())
  })
  
  # Render summary table
  output$sum_design <- DT::renderDT({
    req(sum_visible())
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
    have_duration_conduct <- isTruthy(input$"conduct-duration")
    have_sample_conduct   <- isTruthy(input$"conduct-sample")
    have_age_conduct      <- isTruthy(input$"conduct-age")
    
    ready_conduct <- have_duration_conduct && have_sample_conduct && have_age_conduct
    
    # Toggle enabled/disabled
    shinyjs::toggleState("predict_conduct", condition = ready_conduct)
    shinyjs::toggleState("reset_design2", condition = ready_conduct)
    shinyjs::toggleState("reset_char", condition = ready_conduct)
    
    # Choose labels based on what input is missing (duration, sample size)
    missing_conduct <- c(
      if (!have_duration_conduct) "duration",
      if (!have_sample_conduct) "sample size",
      if (!have_age_conduct) "age"
    )
    
    lab_needed_conduct <- if ("duration" %in% missing_conduct) {
      paste("Enter duration")
    } else if ("sample size" %in% missing_conduct) {
      paste("Enter sample size")
    } else {
      paste("Enter age")
    }
    
    # Predict button
    updateActionButton(
      session, "predict_conduct",
      label = if (ready_conduct) "Predict risk" else lab_needed_conduct
    )
    
    # Reset buttons
    updateActionButton(
      session, "reset_design2",
      label = if (ready_conduct) "Reset design features" else lab_needed_conduct
    )
    
    updateActionButton(
      session, "reset_char",
      label = if (ready_conduct) "Reset characteristics" else lab_needed_conduct
    )
  })
  
  # Force minimum/maximum values for numeric inputs where violated
  conduct_value_limits <- list(
    "conduct-duration"   = c(12, 520),
    "conduct-sample"     = c(30, 1000),
    "conduct-facilities" = c(1, 500),
    "conduct-arms"       = c(2, 6),
    "conduct-age"        = c(18, 100),
    "conduct-t2dur"      = c(0.01, 50),
    "conduct-hba1c"      = c(6, 9),
    "conduct-hdl"        = c(1, 4),
    "conduct-bmi"        = c(19, 50),
    "conduct-egfr"       = c(60, 120),
    "conduct-sbp"        = c(60, 140),
    "conduct-totalc"     = c(1, 4),
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
  
  # Predict conduct ------------------------------------------------------------
  
  # Clear inputs when reset buttons are used
  observeEvent(input$reset_design2, {
    session$sendCustomMessage("resetBox2", "design2_box")
  })
  
  # Prepare inputs
  conduct_inputs <- eventReactive(input$predict_conduct, {
    bind_rows(tidy_design_inputs(input), tidy_char_inputs(input))
  })
  print(conduct_inputs)
  
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