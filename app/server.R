
library(shiny)
library(shinydashboard)
library(tidyverse)
library(posterior)
library(DT)

# Define server logic
function(input, output, session) {
  
  # Sourced scripts for server functionality
  source("R/design_user_inputs.R") # Prepare inputs, join betas
  source("R/design_predict.R") # Gompertz predictions
  
  # Design page conditions -----------------------------------------------------
  
  # Update action buttons (disabled -> usable) based on inputs
  observe({
    have_duration <- isTruthy(input$duration)
    have_sample   <- isTruthy(input$sample)
    ready         <- have_duration && have_sample
    
    # Toggle enabled/disabled
    shinyjs::toggleState("predict_design", condition = ready)
    shinyjs::toggleState("reset_design", condition = ready)
    
    # Choose labels based on what input is missing (duration, sample size)
    lab_needed <- if (!have_duration && !have_sample) {
      "Enter duration"
    } else if (!have_duration) {
      "Enter duration"
    } else if (!have_sample) {
      "Enter sample size"
    } else {
      "Predict (design)"
    }
    
    # Predict button
    updateActionButton(
      session, "predict_design",
      label = if (ready) "Predict (design)" else lab_needed
    )
    
    # Reset button
    updateActionButton(
      session, "reset_design",
      label = if (ready) "Reset design features" else lab_needed
    )
  })
  
  # Force minimum/maximum values for numeric inputs where violated
  value_limits <- list(
    duration   = c(12, 52),
    sample     = c(30, 1000),
    facilities = c(1, 500),
    arms       = c(2, 6),
    mean_age   = c(18, 100),
    mean_t2dur = c(0.01, 50),
    mean_hba1c = c(6, 9),
    mean_hdl   = c(1, 4),
    mean_bmi   = c(19, 50),
    mean_egfr  = c(60, 120),
    mean_sbp   = c(60, 140),
    mean_total = c(1, 4),
    con_med    = c(0, 20),
    comorbs    = c(0, 10)
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
  
  # Home -----------------------------------------------------------------------
  
  # Introductory text for predict
  output$predict_intro <- renderUI({
    predict_path <- "www/text/predict_intro.txt"
    imp_predict <- readLines(predict_path)
    tagList(lapply(imp_predict, p))
  })
  
  # Predict design -------------------------------------------------------------
  
  # Clear inputs when reset buttons are used
  observeEvent(input$reset_design, {
    session$sendCustomMessage("resetBox", "design_box")
  })
  
  # observeEvent(input$reset_conduct, {
  #   session$sendCustomMessage("resetBox", "conduct_box")
  # })
  
  # Prepare user inputs
  design_inputs <- tidy_inputs(input)
  
  # Get Gompertz predictions
  design_predict <- predict_design(design_inputs)
  
  # Render plots
  output$plot_cind_design <- renderPlot({
    design_predict() %>%
      mutate(
        across(contains("risk_"), ~ round(. * 100, 2))
      ) %>% 
      ggplot(aes(x = t_wks, y = risk_q50, ymin = risk_q025, ymax = risk_q975)) +
      geom_line() +
      geom_ribbon(
        alpha = 0.2,
        fill = "steelblue",
        colour = "black",
        linetype = "dashed"
      ) +
      labs(x = "Follow-up time (weeks)", y = "Attrition risk (%)") +
      scale_x_continuous(n.breaks = 6) +
      scale_y_continuous(n.breaks = 6) +
      theme_classic() +
      theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20, face = "bold")
      )
  })
  
  output$plot_hazard_design <- renderPlot({
    design_predict() %>%
      ggplot(aes(x = t_wks, y = hazard_q50)) +
      geom_line() +
      labs(x = "Follow-up time (weeks)", y = "Instantaneous hazard") +
      scale_x_continuous(n.breaks = 6) +
      scale_y_continuous(n.breaks = 6) +
      theme_classic() +
      theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20, face = "bold")
      )
  })
  
  # Render summary table (nothing renders using renderDT, only renderTable)
  output$sum_design <- DT::renderDT({
    df <- design_predict() %>%
      select(t_wks, drp_q50, contains(c("surv", "risk", "cdrop"))) %>%
      mutate(
        across(c(, starts_with("at_risk")), ~ round(.)),
        across(starts_with("risk"), ~ round(. * 100, 2)),
        across(c(contains(c("surv", "cdrop")), risk_q025:risk_q975), ~ round(., 2)),
        across(c(t_wks, drp_q50), as.integer),
        t2e = paste0(surv_q50, " [", surv_q025, ", ", surv_q975, "]"),
        risk = paste0(risk_q50, " [", risk_q025, ", ", risk_q975, "]")
      ) %>% 
      select(t_wks, at_risk_q50, drp_q50, risk_q50) %>% 
      rename(
        "Follow-up time (weeks)" = t_wks,
        "Participants at risk" = at_risk_q50,
        "Non-completers" = drp_q50,
        "Attrition risk (%)" = risk_q50,
      )
    
    DT::datatable(
      df,
      rownames = FALSE,
      class = "compact stripe hover cell-border",
      lazyRender = TRUE,
      options = list(
        scrollY = 320,
        scrollX = FALSE,
        scrollCollapse = TRUE,
        paging = FALSE,
        searching = FALSE,
        fixedHeader = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = "dt-head-center dt-body-center", targets = "_all")
        )
      )
    )
  })
  
  # Predict conduct ------------------------------------------------------------
  
  # Clear inputs when reset buttons are used
  observeEvent(input$reset_design, {
    session$sendCustomMessage("resetBox", "design_box")
  })
  
  observeEvent(input$reset_conduct, {
    session$sendCustomMessage("resetBox", "conduct_box")
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