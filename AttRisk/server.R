
library(shiny)
library(shinydashboard)
library(tidyverse)

# Define server logic
function(input, output, session) {
  
  # Sourced scripts for server functionality
  source("www/scripts/design_user_inputs.R") # Prepare inputs, join betas
  source("www/scripts/design_predict.R") # Gompertz predictions
  
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
  
  # Render plots in tab boxes
  output$plot_hazard <- renderPlot({
    design_predict() %>% 
      select(est, t, haz) %>% 
      pivot_wider(names_from = "est", values_from = "haz") %>% 
      ggplot(aes(x = t*52, y = mean)) +
      geom_line() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Hazard rate") +
      theme_bw()
  })
  
  output$plot_chaz <- renderPlot({
    design_predict() %>% 
      select(est, t, chaz) %>% 
      pivot_wider(names_from = "est", values_from = "chaz") %>% 
      ggplot(aes(x = t*52, y = mean)) +
      geom_line() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Accumulated hazard rate") +
      theme_bw()
  })
  
  output$plot_cind <- renderPlot({
    design_predict() %>% 
      select(est, t, incid) %>% 
      pivot_wider(names_from = "est", values_from = "incid") %>% 
      ggplot(aes(x = t*52, y = mean)) +
      geom_step() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Accumulated incidence") +
      theme_bw()
  })
  
  # Print in console to check it works (comment out after)
  observeEvent(input$predict_design, {
    print(design_predict())
  })
  
  # Predict conduct ------------------------------------------------------------
  
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
