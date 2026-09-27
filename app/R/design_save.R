
# Design-stage output packaging
zip_dir <- function(dir_path) {
  zip_path <- tempfile(fileext = ".zip")
  dir_path <- normalizePath(dir_path, winslash = "/", mustWork = TRUE)
  if (requireNamespace("zip", quietly = TRUE)) {
    owd <- getwd()
    on.exit(setwd(owd), add = TRUE)
    setwd(dirname(dir_path))
    zip::zipr(zipfile = zip_path, files = basename(dir_path))
  } else {
    owd <- getwd()
    on.exit(setwd(owd), add = TRUE)
    setwd(dirname(dir_path))
    utils::zip(zipfile = zip_path, files = basename(dir_path))
  }
  zip_path
}

init_save_outputs <- function(
    input, output, session,
    design_predict,           # Risk tibble
    top5,                     # Tibble of 5 most similar trials
    risk_plot_fun,            # Risk plot function
    hazard_plot_fun,          # Inst. hazard plot function
    sum_fun,                  # Risk summary function
    design_inputs,
    overlay_flag = reactive(isTRUE(input$overlay_top5)),
    base_dir = "saved_outputs"
) {
  lastSaveDir <- reactiveVal(NULL)
  lastZipPath <- reactiveVal(NULL)
  savedStamp  <- reactiveVal(NULL)
  
  observe({
    shinyjs::toggleState(
      "savebtn",
      condition = isTRUE(input$save_outputs) && length(input$save_select) > 0
    )
  })
  
  output$download_saved <- downloadHandler(
    filename = function() paste0("attrisk_outputs_design_", savedStamp() %||% "bundle", ".zip"),
    content = function(file) {
      zp <- lastZipPath()
      sd <- lastSaveDir()
      
      on.exit({
        if (!is.null(zp) && file.exists(zp)) {
          try(unlink(zp), silent = TRUE)
        }
        if (!is.null(sd) && dir.exists(sd)) {
          try(unlink(sd, recursive = TRUE, force = TRUE), silent = TRUE)
        }
        parent <- dirname(sd %||% "")
        if (!is.null(parent) && nzchar(parent) &&
            dir.exists(parent) && length(list.files(parent)) == 0) {
          try(unlink(parent, recursive = FALSE, force = TRUE), silent = TRUE)
        }
        lastZipPath(NULL)
        lastSaveDir(NULL)
        savedStamp(NULL)
      }, add = TRUE)
      
      req(!is.null(zp), file.exists(zp))
      file.copy(zp, file, overwrite = TRUE)
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$savebtn, {
    req(isTRUE(input$save_outputs), length(input$save_select) > 0)
    
    u <- design_predict()
    di <- design_inputs()
    req(!is.null(u), nrow(u) > 0)
    
    overlay <- isTRUE(overlay_flag())
    d5 <- NULL
    if (overlay) {
      d5 <- tryCatch(top5(), error = function(e) NULL)
      if (!is.null(d5) && nrow(d5) > 0) d5 <- arrange(d5, trial_id, t_wks)
    }
    
    stamp <- format(Sys.time(), "%Y-%m-%d")
    savedStamp(stamp)
    
    saveDir <- normalizePath(file.path(base_dir, stamp), winslash = "/", mustWork = FALSE)
    dir.create(saveDir, recursive = TRUE, showWarnings = FALSE)
    lastSaveDir(saveDir)
    
    withProgress(message = "Generating outputs...", value = 0, {
      
      if ("Risk plot" %in% input$save_select) {
        incProgress(0.33, detail = "Risk plot")
        rp <- risk_plot_fun(u, top5_df = d5, show_top5 = overlay)
        ggsave(
          file.path(saveDir, "risk_plot.png"), 
          plot = rp, 
          width = 8, height = 5, dpi = 300
        )
      }
      
      if (overlay) {
        d5_save <- d5 %>% 
          mutate(duration = round(duration, 2)) %>% 
          rename(
            `Ctgov ID` = trial_id,
            `Sample (N)` = n,
            `Total attrition (N)` = attr,
            `Attrition risk (%)` = risk_q50,
            `Follow-up (years)` = duration,
            `Follow-up (weeks)` = t_wks
          )
        write_csv(d5_save, file.path(saveDir, "5_similar_trials.csv"))
      }
      
      if ("Instantaneous hazard plot" %in% input$save_select) {
        incProgress(0.33, detail = "Hazard plot")
        hp <- hazard_plot_fun(u)
        ggsave(
          file.path(saveDir, "hazard_plot.png"), 
          plot = hp, 
          width = 8, height = 5, dpi = 300
        )
      }
      
      if ("Summary statistics" %in% input$save_select) {
        incProgress(0.34, detail = "Summary statistics")
        df <- sum_fun(u)$df
        write_csv(df, file.path(saveDir, "summary_statistics.csv"))
      }
      
      df2 <- d_features(di)
      write_csv(df2, file.path(saveDir, "design_features.csv"))
      
    })
    
    zip_path <- zip_dir(saveDir)
    lastZipPath(normalizePath(zip_path, winslash = "/", mustWork = TRUE))
    file_list <- list.files(saveDir)

    showModal(
      modalDialog(
        title = "Your outputs are ready",
        tagList(
          p("Click the button below to download the files."),
          p("If the download is cancelled, close this window and click 'Save outputs' again."),
          tags$strong("Included files:"),
          tags$ul(lapply(file_list, tags$li)),
          div(
            style = "margin-top: 12px;", 
            downloadButton("download_saved", "Download zip")
          )
        ),
        easyClose = TRUE,
        footer = tagList(modalButton("Close"))
      )
    )
  })
  
  invisible(NULL)
}
