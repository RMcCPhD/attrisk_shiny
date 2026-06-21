
# Conduct-stage output packaging.
#
# These helpers collect the selected conduct outputs, write them to a temporary
# folder, zip that folder, and expose the zip through a Shiny download button.

zip_dir <- function(dir_path) {
  # Create a zip archive from a generated output directory. The zip package is
  # preferred for Windows path handling, with utils::zip as a fallback.
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

init_save_outputs2 <- function(
    input, output, session,
    conduct_predict,          # Risk tibble
    risk_plot_fun,            # Risk plot function
    sum_fun,                  # Risk summary function
    conduct_inputs,           # User inputs (design features, characteristics)
    base_dir = "saved_outputs"
) {
  # Track the latest temporary output folder and archive until the user has
  # downloaded them or the modal is dismissed.
  lastSaveDir <- reactiveVal(NULL)
  lastZipPath <- reactiveVal(NULL)
  savedStamp  <- reactiveVal(NULL)
  
  # Activate the save button only when the user has opted in and selected at
  # least one output.
  observe({
    shinyjs::toggleState(
      "savebtn2",
      condition = isTRUE(input$save_outputs2) && length(input$save_select2) > 0
    )
  })
  
  output$download_saved2 <- downloadHandler(
    filename = function() paste0("attrisk_outputs_conduct_", savedStamp() %||% "bundle", ".zip"),
    content = function(file) {
      zp <- lastZipPath()
      sd <- lastSaveDir()
      
      # After the browser receives the archive, clean up temporary app-side
      # files so repeated saves do not accumulate stale folders.
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
  
  observeEvent(input$savebtn2, {
    req(isTRUE(input$save_outputs2), length(input$save_select2) > 0)
    
    u <- conduct_predict()
    cdi <- conduct_inputs()
    req(!is.null(u), nrow(u) > 0)
    
    stamp <- format(Sys.time(), "%Y-%m-%d")
    savedStamp(stamp)
    
    saveDir <- normalizePath(file.path(base_dir, stamp), winslash = "/", mustWork = FALSE)
    dir.create(saveDir, recursive = TRUE, showWarnings = FALSE)
    lastSaveDir(saveDir)
    
    # Generate only the selected artefacts. Feature/characteristic inputs are
    # always included so downloaded outputs are traceable to the scenario.
    withProgress(message = "Generating outputs...", value = 0, {
      
      if ("Risk plot" %in% input$save_select2) {
        incProgress(0.33, detail = "Risk plot")
        rp <- risk_plot_fun(u)
        ggsave(
          file.path(saveDir, "risk_plot.png"), 
          plot = rp, 
          width = 8, height = 5, dpi = 300
        )
      }
      
      if ("Summary statistics" %in% input$save_select2) {
        incProgress(0.34, detail = "Summary statistics")
        df <- sum_fun(u)$df
        write_csv(df, file.path(saveDir, "summary_statistics.csv"))
      }
      
      df2 <- c_features(cdi)
      write_csv(df2, file.path(saveDir, "features_characteristics.csv"))
      
    })
    
    zip_path <- zip_dir(saveDir)
    lastZipPath(normalizePath(zip_path, winslash = "/", mustWork = TRUE))
    file_list <- list.files(saveDir)

    # Let the user download the prepared archive from the modal.
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
            downloadButton("download_saved2", "Download zip")
          )
        ),
        easyClose = TRUE,
        footer = tagList(modalButton("Close"))
      )
    )
  })
  
  invisible(NULL)
}
