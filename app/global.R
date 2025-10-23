
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)
library(tidyverse)
library(markdown)
library(knitr)
library(kableExtra)

# Design page markdown
rmarkdown::render(
  input        = "www/mkdown/design_overview.Rmd",
  output_format= "html_fragment",
  output_file  = "design_overview.html",
  output_dir   = "www/mkdown",
  quiet        = TRUE
)

rmarkdown::render(
  input        = "www/mkdown/design_func.Rmd",
  output_format= "html_fragment",
  output_file  = "design_func.html",
  output_dir   = "www/mkdown",
  quiet        = TRUE
)

rmarkdown::render(
  input        = "www/mkdown/design_egs.Rmd",
  output_format= "html_fragment",
  output_file  = "design_egs.html",
  output_dir   = "www/mkdown",
  quiet        = TRUE
)

# Conduct page markdown
rmarkdown::render(
  input        = "www/mkdown/conduct_overview.Rmd",
  output_format= "html_fragment",
  output_file  = "conduct_overview.html",
  output_dir   = "www/mkdown",
  quiet        = TRUE
)

rmarkdown::render(
  input        = "www/mkdown/conduct_func.Rmd",
  output_format= "html_fragment",
  output_file  = "conduct_func.html",
  output_dir   = "www/mkdown",
  quiet        = TRUE
)

rmarkdown::render(
  input        = "www/mkdown/conduct_egs.Rmd",
  output_format= "html_fragment",
  output_file  = "conduct_egs.html",
  output_dir   = "www/mkdown",
  quiet        = TRUE
)