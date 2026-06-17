
# Export app-facing model objects from the sibling attrisk analysis project.
# Run from the attrisk_shiny project root after the attrisk pipeline has
# produced the latest outputs. The script intentionally writes only the compact
# objects the Shiny app needs, not the raw Stan fit.

args <- commandArgs(trailingOnly = TRUE)
attrisk_dir <- if (length(args) >= 1) args[[1]] else "../attrisk"
app_dir <- if (length(args) >= 2) args[[2]] else "app"

must_exist <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", normalizePath(path, mustWork = FALSE), call. = FALSE)
  }
  path
}

copy_required <- function(from, to) {
  must_exist(from)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(from, to, overwrite = TRUE)
  if (!ok) stop("Could not copy ", from, " to ", to, call. = FALSE)
  invisible(to)
}

app_data_dir <- file.path(app_dir, "www", "data")
dir.create(app_data_dir, recursive = TRUE, showWarnings = FALSE)

design_model_path <- must_exist(file.path(attrisk_dir, "outputs", "design", "design_model.rds"))
design_draws_path <- file.path(app_data_dir, "design_draws.rds")

if (!requireNamespace("posterior", quietly = TRUE)) {
  stop("Package 'posterior' is required to export design_draws.rds.", call. = FALSE)
}
if (!requireNamespace("rstan", quietly = TRUE)) {
  stop(
    "Package 'rstan' is required to convert the upstream stanfit. ",
    "Run this script in the same R environment used for the attrisk model fit.",
    call. = FALSE
  )
}

message("Reading design model: ", design_model_path)
design_model <- readRDS(design_model_path)

message("Writing app-facing posterior draws: ", design_draws_path)
design_draws <- posterior::as_draws_matrix(design_model)
saveRDS(design_draws, design_draws_path)

copy_required(
  file.path(attrisk_dir, "processed_data", "stan_data.rds"),
  file.path(app_data_dir, "design_df.rds")
)

agd <- readRDS(must_exist(file.path(attrisk_dir, "processed_data", "agd_steps_out.rds")))
trials <- agd$format_covars[[1]]
saveRDS(trials, file.path(app_data_dir, "trials.rds"))

copy_required(
  file.path(attrisk_dir, "processed_data", "shape_rate.rds"),
  file.path(app_data_dir, "shape_rate.rds")
)

copy_required(
  file.path(attrisk_dir, "outputs", "conduct", "citl", "delta_draws.rds"),
  file.path(app_data_dir, "delta_draws.rds")
)

copy_required(
  file.path(attrisk_dir, "processed_data", "sobol_lst.rds"),
  file.path(app_data_dir, "sobol_lst.rds")
)

copy_required(
  file.path(attrisk_dir, "vivli", "v2", "Summaries", "covariate_mu_d.csv"),
  file.path(app_data_dir, "conduct_scale.csv")
)

exported <- c(
  design_draws = design_draws_path,
  design_df = file.path(app_data_dir, "design_df.rds"),
  trials = file.path(app_data_dir, "trials.rds"),
  shape_rate = file.path(app_data_dir, "shape_rate.rds"),
  delta_draws = file.path(app_data_dir, "delta_draws.rds"),
  sobol_lst = file.path(app_data_dir, "sobol_lst.rds"),
  conduct_scale = file.path(app_data_dir, "conduct_scale.csv")
)

manifest <- data.frame(
  object = names(exported),
  path = normalizePath(unname(exported), winslash = "/", mustWork = TRUE),
  md5 = unname(tools::md5sum(exported)),
  modified = format(file.info(exported)$mtime, "%Y-%m-%d %H:%M:%S %z"),
  stringsAsFactors = FALSE
)

write.csv(
  manifest,
  file.path(app_data_dir, "model_manifest.csv"),
  row.names = FALSE
)

message("Export complete. Review app code mappings before deploying this data bundle.")
