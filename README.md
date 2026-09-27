# AttRisk: An interactive application for predicting attrition risk during trial design and conduct

This repository contains the R Shiny application, supporting prediction code, and user documentation for *AttRisk*, described in Chapter 7 of the thesis.

AttRisk provides an interactive interface to the framework for predicting control arm attrition risk in phase III/IV randomised controlled trials of newer glucose-lowering drugs for type 2 diabetes mellitus. The application allows users to:

- specify trial design characteristics and follow-up duration to estimate aggregate attrition risk;
- visualise cumulative attrition risk and instantaneous attrition hazard over time;
- compare a proposed design with observed attrition in five similar trials from the model-development dataset;
- combine trial design features with participant baseline characteristics to explore participant-level attrition risk; and
- download selected plots, tabular summaries, and the inputs used to generate them.

The underlying model development and internal validation are maintained separately in [`attrisk`](https://github.com/RMcCPhD/attrisk). That analysis includes 342 trials with 72,692 control arm participants, of which 76 trials contributed individual participant data (IPD) for 19,023 participants. The design-stage model underwent ten-fold cross-validation; the conduct-stage model was not internally validated, and the complete framework has not been externally validated. AttRisk is a prototype application, and its predictive capabilities have not been clinically validated.

## Reproducibility and data access

The public materials contain the application source code, static interface assets, a PDF user guide, and a script for exporting model objects from the analysis repository. The app uses previously fitted models; model fitting and internal validation are performed in `attrisk`, rather than within the Shiny interface.

The model bundle used by the current app is stored locally in `app/www/data/`. This directory is excluded by `.gitignore`, although two legacy files remain tracked. The required current model objects are not supplied as a complete bundle in the public checkout. A fresh clone therefore requires the model exports and setup adjustments described below before the app can run.

Raw IPD are not included. They were accessed through the Vivli secure research environment for the underlying analysis. The app uses model draws, aggregate trial information, exported covariate scaling information, and derived conduct-stage objects. Generated participant profiles in the upstream workflow are numerical integration points, not records of actual participants. The `attrisk` README describes the original data sources, access restrictions, and requirements for regenerating the fitted objects.

## Relationship to the model-development analysis

The application is intended to use outputs from a sibling checkout of `attrisk`:

``` text
parent_directory/
|-- attrisk/
`-- attrisk_shiny/
```

`tools/export_attrisk_app_data.R` converts the fitted design-stage model to a posterior-draw matrix, extracts aggregate trial information, and copies supporting objects into the app. It also writes `model_manifest.csv`, recording exported paths, MD5 checksums, and modification times. The script accepts alternative source and application directories as command-line arguments.

The current prediction helpers read the following files from `app/www/data/`:

| File | Purpose |
|----|----|
| `design_draws.rds` | Posterior draws from the fitted design-stage model. |
| `design_df.rds` | Stan input data, including the design matrix, arm count, and shape-log-rate constants. |
| `trials.rds` | Aggregate trial characteristics, control arm sample sizes, attrition counts, and follow-up durations for the similar-trial comparison. |
| `conduct_scale.csv` | Covariate scaling information for participant-level inputs. |
| `delta_draws.rds` | Draw-specific conduct-stage log-rate intercept adjustments. |
| `sobol_lst.rds` | A conduct-stage object containing the named participant-level log-hazard-ratio vector in its `loghr` element. |

The exporter additionally copies `shape_rate_design.rds`; the current helpers obtain the shape-log-rate constants from `design_df.rds`. All model objects must come from a consistent analysis run, with matching coefficient names, trial ordering, and posterior-draw ordering.

## Repository structure

### `app/`

The active Shiny application. `global.R` loads shared packages, `ui.R` defines the dashboard and inputs, and `server.R` connects input validation, reactive predictions, plots, tables, and downloads. Launch this directory with `shiny::runApp()` as shown below.

### `app/R/`

Prediction and output helpers. `aaa_model_helpers.R` loads the model bundle and defines shared input mappings and Gompertz prediction functions. The `design_*` files implement design-stage input preparation, predictions, similar-trial comparisons, plots, summaries, and downloads. The `conduct_*` files implement the corresponding participant-level workflow, including the conduct-stage intercept adjustment. `app/R/old/` retains earlier implementations.

### `app/www/`

Static assets and locally staged model data. `style/` contains CSS and JavaScript for the interface, `text/` contains explanatory text, author information, and the disclaimer, and `docs/AttRisk.pdf` is the user guide displayed in the app. `data/` holds the model bundle described above. The home page also references `mkdown/home.Rmd`, which is absent from the current working tree and must be restored or its reference updated before launch.

### `app/saved_outputs/`

The runtime destination for selected plots and CSV summaries before they are packaged as ZIP downloads. The save handlers create dated subdirectories and attempt to remove the generated files after a download. This directory is generated when outputs are saved.

### `tools/`

`export_attrisk_app_data.R` prepares the app-facing model bundle from the sibling analysis repository. Its retained paths and output names require reconciliation with the current analysis and app, as described below.

### `old/`

Archived versions of the home, design-stage, and conduct-stage interfaces, together with earlier prediction code and supporting assets. These record application development and are separate from the active app in `app/`.

## Application workflow

| Stage | Interface | Purpose |
|----|----|----|
| Introduction | Home and User guide | Introduces AttRisk and displays the PDF user guide. |
| Design-stage inputs | Trial design | Specifies duration, number of arms, sample size, phase, eligibility criteria, sites, outcomes, comparators, blinding, and geographical regions. |
| Design-stage predictions | Trial design | Displays cumulative attrition risk, instantaneous hazard, and tabulated risk over follow-up. |
| Similar-trial comparison | Trial design sidebar | Selects five similar trials using weighted Gower distance and displays observed final attrition proportions, with links to ClinicalTrials.gov. |
| Conduct-stage inputs | Trial conduct | Combines trial design features with participant characteristics, including age, sex, race, diabetes duration, laboratory measurements, comorbidities, and concomitant medications. |
| Conduct-stage predictions | Trial conduct | Displays participant-level risk trajectories and tabulated risk after applying participant-level associations and an intercept adjustment. |
| Export | Design and conduct sidebars | Packages selected PNG plots and CSV summaries, together with the entered features, as a ZIP download. |

Risk plots display posterior medians and 95% intervals; summary tables display median risk as a percentage at each follow-up time. The similar-trial overlay shows observed end-of-follow-up proportions rather than fitted trajectories for those trials.

The conduct-stage implementation uses the exported participant-level log-hazard-ratio estimates and averages the exported intercept adjustments across arms within each posterior draw. It does not rerun the upstream numerical integration and arm-specific alignment procedure for each user-entered design.

### Preparing a fresh checkout

Install the packages listed below and obtain a compatible model bundle, or regenerate the necessary outputs in `attrisk`. Before using the supplied exporter, reconcile these retained working-environment references:

- Change the design-model source from `outputs/design/design_model.rds` to `outputs/design_stage/model/design_model.rds` in the current analysis layout.
- Change the intercept-adjustment source from `outputs/conduct/citl/delta_draws.rds` to `outputs/conduct_stage/citl/delta_draws.rds`.
- Change the scaling-summary source from `vivli/v2/Summaries/covariate_mu_d.csv` to `vivli/Summaries/covariate_mu_d.csv`.
- Reconcile the conduct-stage object: the exporter copies `processed_data/sobol_lst_mi.rds` to `sobol_lst_mi.rds`, whereas the app reads `sobol_lst.rds` and expects a named `loghr` element. The current upstream profile workflow writes objects under `outputs/conduct_stage/`. Prepare a compatible app object and update the export source and destination; renaming an imputation collection alone does not establish that it has the required structure.

The exporter also requires `processed_data/stan_data.rds`, `processed_data/agd_steps_out.rds`, and `processed_data/shape_rate_design.rds` from the analysis. These are generated intermediates, so the sibling repository alone does not provide all required inputs.

After reconciling the paths and object format, run the exporter from the `attrisk_shiny` repository root:

``` sh
Rscript tools/export_attrisk_app_data.R ../attrisk app
```

Restore `app/www/mkdown/home.Rmd`, or update the `includeMarkdown()` call in `app/ui.R` to an available home-page resource. Confirm that the six model files listed above are present and compatible before launching. These are setup requirements for the current source, rather than an automated end-to-end installation workflow.

### Running the application locally

From an R session with the repository root as the working directory:

``` r
shiny::runApp("app")
```

Alternatively, open `attrisk_shiny.Rproj` in RStudio and run the same command. Shiny runs the application from `app/`, allowing its relative paths to `www/` to resolve correctly. The application needs write access to `app/saved_outputs/` and the R temporary directory to prepare downloads.

Enter the required numeric inputs and select the trial design features, then use **Predict risk**. The conduct page additionally requires participant age. Use the sidebar controls to show similar trials or select outputs for download. Reset controls clear the relevant inputs.

## Software requirements

The application requires R and the following packages. Exact R and package versions are not locked in this repository.

- `shiny`
- `shinydashboard`
- `shinycssloaders`
- `shinyjs`
- `shinyalert`
- `shinyvalidate`
- `tidyverse`
- `markdown`
- `posterior`
- `DT`
- `flexsurv`
- `cluster`
- `zip` for creating download archives without relying on a system ZIP utility

These can be installed from an R session:

``` r
install.packages(c(
  "shiny", "shinydashboard", "shinycssloaders", "shinyjs",
  "shinyalert", "shinyvalidate", "tidyverse", "markdown",
  "posterior", "DT", "flexsurv", "cluster", "zip"
))
```

The export script additionally requires `rstan` to convert the upstream `stanfit` object and should be run in the R environment used for model fitting. Running the app with an already prepared bundle does not require refitting the Stan model. Regenerating the underlying analysis has additional dependencies documented in `attrisk`.

## Statistical analysis plan

The pre-specified study protocol is maintained separately in [`attrisk_protocol`](https://github.com/RMcCPhD/attrisk_protocol). Chapter 6 of the thesis describes the implemented prediction framework, and Chapter 7 describes the AttRisk application. The model-development code and reproducibility instructions are available in [`attrisk`](https://github.com/RMcCPhD/attrisk).

## Citation

TBC
