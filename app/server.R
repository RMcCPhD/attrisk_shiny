
library(shiny)
library(shinydashboard)
library(tidyverse)
library(posterior)
library(DT)

# Define server logic
function(input, output, session) {
  
  # Sourced scripts for server functionality
  source("R/design_user_inputs.R") # Prepare inputs, join betas
  source("R/design_predict.R")     # Gompertz predictions
  source("R/conduct_user_inputs.R")
  # source("R/conduct_predict.R")
  
  # Design page conditions -----------------------------------------------------
  
  # Update action buttons (disabled -> usable) based on inputs
  observe({
    have_duration <- isTruthy(input$"design-duration")
    have_sample   <- isTruthy(input$"design-sample")
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
      label = if (ready) "Predict risk" else lab_needed
    )
    
    # Reset button
    updateActionButton(
      session, "reset_design",
      label = if (ready) "Reset design features" else lab_needed
    )
  })
  
  # Force minimum/maximum values for numeric inputs where violated
  value_limits <- list(
    "design-duration"   = c(12, 520),
    "design-sample"     = c(30, 1000),
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
  # lapply(names(conduct_value_limits), function(id) {
  #   observeEvent(input[[id]], ignoreInit = TRUE, {
  #     val <- input[[id]]
  #     if (is.null(val) || is.na(val)) return(NULL)
  #     low <- conduct_value_limits[[id]][1]
  #     high <- conduct_value_limits[[id]][2]
  #     
  #     if (val < low) {
  #       updateNumericInput(session, id, value = low)
  #     } else if (val > high) {
  #       updateNumericInput(session, id, value = high)
  #     }
  #   })
  # })
  
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
    d5 <- bind_rows(top5())
    req(!is.null(d5), nrow(d5) > 0)
    
    # Get the 5 unique trial IDs in the order they were selected
    ids <- d5 %>% distinct(trial_id) %>% pull(trial_id)
    
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
    p <- design_predict() %>%
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
    
    if (isTRUE(input$overlay_top5)) {
      get_top5 <- top5()
      top5_df <- bind_rows(get_top5) %>% 
        mutate(across(contains("risk_"), ~ round(. * 100, 2)))

      p <- p + 
        geom_line(
          data = top5_df, 
          aes(x = t_wks, y = risk_q50, group = trial_id),
          colour = "salmon"
        )
    }
    p
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
  
  # Save design outputs --------------------------------------------------------
  
  # Create outputs downloadable by user
  observe({
    shinyjs::toggleState(
      "savebtn",
      condition = isTRUE(input$save_outputs) && length(input$save_select) > 0
    )
  })
  
  # Helpers for generating plots to save
  make_risk_plot <- function(user_df, top5_df = NULL, overlay = FALSE) {
    ggplot(user_df, aes(x = t_wks, y = risk_q50, ymin = risk_q025, ymax = risk_q975)) +
      geom_line() +
      geom_ribbon(alpha = 0.2, fill = "steelblue", colour = "black", linetype = "dashed") +
      labs(x = "Follow-up time (weeks)", y = "Attrition risk (%)") +
      scale_x_continuous(n.breaks = 6) +
      scale_y_continuous(n.breaks = 6) +
      theme_classic() +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20, face = "bold")) +
      # Add in median risk for 5 similar trials if selected by user
      { if (overlay && !is.null(top5_df) && nrow(top5_df))
        geom_line(data = top5_df, aes(y = risk_q50, group = trial_id), colour = "salmon")
        else NULL }
  }
  
  make_hazard_plot <- function(user_df, top5_df = NULL, overlay = FALSE) {
    ggplot(user_df, aes(x = t_wks, y = hazard_q50)) +
      geom_line() +
      labs(x = "Follow-up time (weeks)", y = "Instantaneous hazard") +
      scale_x_continuous(n.breaks = 6) +
      scale_y_continuous(n.breaks = 6) +
      theme_classic() +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20, face = "bold")) +
      { if (overlay && !is.null(top5_df) && nrow(top5_df))
        geom_line(data = top5_df, aes(y = hazard_q50, group = trial_id), colour = "salmon")
        else NULL }
  }
  
  # Paths NULL by default
  lastSaveDir <- reactiveVal(NULL)
  lastZipPath <- reactiveVal(NULL)
  savedStamp  <- reactiveVal(NULL)
  
  zip_dir <- function(dir_path) {
    zip_path <- tempfile(fileext = ".zip")
    dir_path <- normalizePath(dir_path, winslash = "/", mustWork = TRUE)
    files_to_zip <- list.files(dir_path, recursive = TRUE, full.names = TRUE)
    
    if (requireNamespace("zip", quietly = TRUE)) {
      owd <- getwd()
      on.exit(setwd(owd), add = TRUE)
      setwd(dirname(dir_path))
      zip::zipr(zipfile = zip_path, files = basename(dir_path))
    } else {
      owd <- getwd(); on.exit(setwd(owd), add = TRUE)
      setwd(dirname(dir_path))
      utils::zip(zipfile = zip_path, files = basename(dir_path))
    }
    zip_path
  }
  
  output$download_saved <- downloadHandler(
    filename = function() {
      paste0("attrisk_outputs_", savedStamp() %||% "bundle", ".zip")
    },
    content = function(file) {
      zp <- lastZipPath()
      sd <- lastSaveDir()
      req(!is.null(zp), file.exists(zp))
      
      # Stream the prepared zip to the browser temp path file
      ok <- file.copy(zp, file, overwrite = TRUE)
      
      # Clean up artifacts server-side
      if (isTRUE(ok)) {
        # Remove the zipped folder and timestamped folders after download
        try(unlink(zp), silent = TRUE)
        if (!is.null(sd) && dir.exists(sd)) {
          try(unlink(sd, recursive = TRUE, force = TRUE), silent = TRUE)
        }
        # Clear directory if left empty
        parent <- dirname(sd %||% "")
        if (!is.null(parent) && nzchar(parent) &&
            dir.exists(parent) && length(list.files(parent)) == 0) {
          try(unlink(parent, recursive = FALSE, force = TRUE), silent = TRUE)
        }
        
        # Clear paths to prevent redownload after another click
        lastZipPath(NULL)
        lastSaveDir(NULL)
        savedStamp(NULL)
      }
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$savebtn, {
    req(isTRUE(input$save_outputs))
    req(length(input$save_select) > 0)
    
    u <- design_predict()
    req(!is.null(u), nrow(u) > 0)
    
    overlay_flag <- isTRUE(input$overlay_top5)
    d5 <- NULL
    if (overlay_flag) {
      d5 <- tryCatch(bind_rows(top5()), error = function(e) NULL)
      if (!is.null(d5) && nrow(d5) > 0) d5 <- arrange(d5, trial_id, t_wks)
    }
    
    # Create a timestamped folder
    stamp   <- format(Sys.time(), "%Y%m%d-%H%M%S")
    savedStamp(stamp)
    baseDir <- "saved_outputs"
    saveDir <- file.path(baseDir, stamp)
    dir.create(saveDir, recursive = TRUE, showWarnings = FALSE)
    lastSaveDir(saveDir)
    
    # Prepare outputs (plots and summary tables, conditional on checkbox)
    withProgress(message = "Generating outputs…", value = 0, {
      if ("Risk plot" %in% input$save_select) {
        incProgress(0.33, detail = "Risk plot")
        u_plot <- u %>% mutate(across(contains("risk_"), ~ round(. * 100, 2)))
        d5_plot <- if (!is.null(d5)) d5 %>% mutate(across(contains("risk_"), ~ round(. * 100, 2))) else NULL
        rp <- make_risk_plot(u_plot, d5_plot, overlay = overlay_flag)
        ggsave(filename = file.path(saveDir, "risk_plot.png"), plot = rp, width = 8, height = 5, dpi = 300)
      }

      if ("Instantaneous hazard plot" %in% input$save_select) {
        incProgress(0.33, detail = "Hazard plot")
        hp <- make_hazard_plot(u, d5)
        ggsave(filename = file.path(saveDir, "hazard_plot.png"), plot = hp, width = 8, height = 5, dpi = 300)
      }
      
      if ("Summary statistics" %in% input$save_select) {
        incProgress(0.34, detail = "Summary statistics")
        df <- u %>%
          select(t_wks, drp_q50, contains(c("surv", "risk", "cdrop")), starts_with("at_risk")) %>%
          mutate(
            across(starts_with("at_risk"), round),
            across(starts_with("risk"), ~ round(. * 100, 2)),
            across(c(contains("surv"), contains("cdrop")), ~ round(., 2)),
            drp_q50 = as.integer(round(drp_q50)),
            t_wks   = as.integer(round(t_wks))
          ) %>%
          select(t_wks, at_risk_q50, drp_q50, risk_q50) %>% 
          rename(
            "Follow-up time (weeks)" = t_wks,
            "Participants at risk" = at_risk_q50,
            "Non-completers" = drp_q50,
            "Attrition risk (%)" = risk_q50,
          )
        readr::write_csv(df, file.path(saveDir, "summary_statistics.csv"))
      }
    })
    
    # Zip folder of outputs and save filepath
    message("Saved files: ", paste(list.files(saveDir), collapse = ", "))
    zip_path <- zip_dir(saveDir)
    lastZipPath(zip_path)
    
    # Modal: Show user download is ready (with button to download)
    file_list <- list.files(saveDir)
    showModal(
      modalDialog(
        title = "Your outputs are ready",
        tagList(
          p("Click the button below to download the files."),
          tags$strong("Included files:"),
          tags$ul(lapply(file_list, tags$li)),
          div(style = "margin-top: 12px;",
              downloadButton("download_saved", "Download zip")
          )
        ),
        easyClose = TRUE,
        footer = tagList(modalButton("Close"))
      )
    )
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