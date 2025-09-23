
library(shiny)
library(shinydashboard)
library(tidyverse)

# Define server logic
function(input, output, session) {
  
  # Sourced scripts for server functionality
  source("www/scripts/design_user_inputs.R") # Prepare inputs, join betas
  source("www/scripts/design_predict.R") # Gompertz predictions
  
  # Conditionally enable or disable action buttons -----------------------------
  
  observe({
    
    # Get conduct inputs
    conduct_inputs <- c(
      input$mean_age,
      input$mean_t2dur,
      input$mean_hba1c,
      input$mean_hdl,
      input$mean_bmi,
      input$mean_egfr,
      input$mean_sbp,
      input$mean_total,
      input$con_med,
      input$comorbs,
      input$sex,
      input$smoke,
      input$race
    )
    
    # Conditions for design and conduct action buttons
    design_ready <- !is.na(input$duration) & input$duration != ""
    conduct_ready <- any(!is.na(conduct_inputs) & conduct_inputs != "")
    
    # Design action buttons
    if (design_ready) {
      shinyjs::enable("predict_design")
      shinyjs::enable("reset_design")
      updateActionButton(session, "predict_design", "Predict risk")
      updateActionButton(session, "reset_design", "Reset inputs")
    } else {
      shinyjs::disable("predict_design")
      shinyjs::disable("reset_design")
      updateActionButton(session, "predict_design", "Enter duration")
      updateActionButton(session, "reset_design", "Enter duration")
    }
  })
  
  # Force minimum/maximum values for numeric inputs where violated -------------
  
  # Value thresholds
  value_limits <- list(
    duration   = c(12, 260),
    sample     = c(30, 20000),
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
  
  # Predict design -------------------------------------------------------------
  
  # Clear inputs when reset buttons are used
  observeEvent(input$reset_design, {
    session$sendCustomMessage("resetBox", "design_box")
  })
  
  observeEvent(input$reset_conduct, {
    session$sendCustomMessage("resetBox", "conduct_box")
  })
  
  # Prepare user inputs
  design_inputs <- tidy_inputs(input)
  
  # Get Gompertz predictions
  design_predict <- predict_design(design_inputs)
  
  # Render plots in tab boxes
  output$plot_hazard_design <- renderPlot({
    design_predict() %>% 
      select(est, t, haz) %>% 
      pivot_wider(names_from = "est", values_from = "haz") %>% 
      ggplot(aes(x = t*52, y = mean)) +
      geom_line() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Hazard rate") +
      theme_bw()
  })
  
  # Print in console to check it works (comment out after)
  observeEvent(input$predict_design, {
    print(design_predict())
  })
}
