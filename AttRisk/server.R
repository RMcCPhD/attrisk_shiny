
library(shiny)
library(shinydashboard)
library(tidyverse)

# Define server logic
function(input, output, session) {
  
  # Introductory text for predict
  output$predict_intro <- renderUI({
    predict_path <- "www/text/predict_intro.txt"
    imp_predict <- readLines(predict_path)
    tagList(lapply(imp_predict, p))
  })
  
  # Clear inputs when reset buttons are used
  observeEvent(input$reset_design, {
    session$sendCustomMessage("resetBox", "design_box")
  })
  
  observeEvent(input$reset_conduct, {
    session$sendCustomMessage("resetBox", "conduct_box")
  })
  
  # About AttRisk
  output$about_attrisk <- renderUI({
    about_path <- "www/text/about.txt"
    imp_about <- readLines(about_path)
    tagList(lapply(imp_about, p))
  })
  
  # Authors text
  output$authors_text <- renderUI({
    authors_path <- "www/text/authors.txt"
    imp_authors <- readLines(authors_path)
    HTML(paste0("<ul>", paste(sprintf("<li>%s</li>", imp_authors), collapse = ""), "</ul>"))
  })
  
  # Feedback and suggestions
  output$fback_sugg <- renderUI({
    fback_path <- "www/text/feedback_suggestions.txt"
    imp_fback <- readLines(fback_path)
    tagList(lapply(imp_fback, p))
  })
  
}
