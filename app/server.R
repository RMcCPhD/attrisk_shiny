
library(shiny)
library(shinydashboard)
library(tidyverse)

# Define server logic
function(input, output, session) {
  
  # Sourced scripts for server functionality
  source("R/design_user_inputs.R") # Prepare inputs, join betas
  source("R/design_predict.R") # Gompertz predictions
  
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
    design_ready2 <- !is.na(input$duration2) & input$duration2 != ""
    conduct_ready <- any(!is.na(conduct_inputs) & conduct_inputs != "")
    
    # Design action buttons
    if (design_ready) {
      shinyjs::enable("predict_design")
      shinyjs::enable("reset_design")
      updateActionButton(session, "predict_design", "Predict (design)")
      updateActionButton(session, "reset_design", "Reset design features")
    } else {
      shinyjs::disable("predict_design")
      shinyjs::disable("reset_design")
      updateActionButton(session, "predict_design", "Enter duration")
      updateActionButton(session, "reset_design", "Enter duration")
    }
    
    if (design_ready2) {
      shinyjs::enable("reset_design2")
      updateActionButton(session, "reset_design2", "Reset design features")
    } else {
      shinyjs::disable("reset_design2")
      updateActionButton(session, "reset_design2", "Enter duration")
    }
    
    # Conduct action buttons
    if (conduct_ready && design_ready2) {
      shinyjs::enable("predict_conduct")
      shinyjs::enable("reset_conduct")
      updateActionButton(session, "predict_conduct", "Predict (conduct)")
      updateActionButton(session, "reset_conduct", "Reset characteristics")
    } else if (design_ready2 && !conduct_ready) {
      shinyjs::disable("predict_conduct")
      shinyjs::disable("reset_conduct")
      updateActionButton(session, "predict_conduct", "Enter a characteristic")
      updateActionButton(session, "reset_conduct", "Enter a characteristic")
    } else {
      shinyjs::disable("predict_conduct")
      shinyjs::disable("reset_conduct")
      updateActionButton(session, "predict_conduct", "Enter duration")
      updateActionButton(session, "reset_conduct", "Enter duration")
    }
  })
  
  # Force minimum/maximum values for numeric inputs where violated -------------
  
  # Value thresholds
  value_limits <- list(
    duration   = c(12, 52),
    sample     = c(30, 500),
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
  
  observeEvent(input$reset_conduct, {
    session$sendCustomMessage("resetBox", "conduct_box")
  })
  
  # Prepare user inputs
  design_inputs <- tidy_inputs(input)
  
  # Get Gompertz predictions
  design_predict <- predict_design(design_inputs)
  
  # Render plot
  output$plot_hazard_design <- renderPlot({
    design_predict() %>% 
      select(est, t, haz) %>% 
      pivot_wider(names_from = "est", values_from = "haz") %>% 
      mutate(val = mean/max(mean)) %>% 
      ggplot(aes(x = t, y = val)) +
      geom_line() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Follow-up time (weeks)", y = "Scaled to highest point (1 = peak)") +
      theme_classic() +
      theme(
        axis.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold")
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
  
  # Prepare user inputs
  design_inputs <- tidy_inputs(input)
  
  # Get Gompertz predictions
  design_predict <- predict_design(design_inputs)
  
  # Render plot
  output$plot_hazard_conduct <- renderPlot({
    design_predict() %>% 
      select(est, t, haz) %>% 
      pivot_wider(names_from = "est", values_from = "haz") %>% 
      mutate(val = mean/max(mean)) %>% 
      ggplot(aes(x = t * 52, y = val)) +
      geom_line() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Scaled attrition rate") +
      theme_bw()
  })
  
  # Render summary statistics
  output$sum_design <- renderTable({
    design_predict() %>% 
      filter(est == "mean") %>% 
      select(est, t, haz, S, H) %>% 
      pivot_wider(names_from = "est", values_from = "haz") %>% 
      mutate(t = t) %>% 
      rename(
        `Follow-up time (weeks)` = t
      ) %>% 
      mutate(
        `Sample` = input$sample,
        `Participants at risk of attrition` = round(`Sample` * S),
        `Number of attrition events` = round(Sample * (lag(S, default = 1) - S)),
        `Cumulative attrition events` = round(`Sample` * (1 - S))
      ) %>% 
      select(-S, -Sample) %>% 
      select(
        `Follow-up time (weeks)`, `Participants at risk of attrition`,
        `Number of attrition events`, `Cumulative attrition events`
      )
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