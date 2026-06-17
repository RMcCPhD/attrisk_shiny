# attrisk output handoff review

Checked on 2026-06-17 from `attrisk_shiny` against the sibling `../attrisk`
project.

## Main finding

The app is currently wired to an October 2025 app data bundle in
`app/www/data`. The current `attrisk` pipeline produced refreshed design and
conduct outputs on 2026-06-14 to 2026-06-15, and the model schema changed.
Refreshing files alone is not enough; the app's UI/input mapping and prediction
logic must be updated to the new schema.

## Output changes to reflect

- `design_df.rds` should come from `../attrisk/processed_data/stan_data.rds`.
  The old app design matrix is `344 x 21`; the new matrix is `342 x 31`.
- `design_draws.rds` should be an app-facing `posterior::draws_matrix` exported
  from `../attrisk/outputs/design/design_model.rds`. Do not make the app load
  the raw `stanfit` object.
- `trials.rds` should come from
  `../attrisk/processed_data/agd_steps_out.rds` element `format_covars[[1]]`.
- `shape_rate.rds` replaces the old `lm_res.csv` plus
  `lm_res_shrinkage.rds` shape bundle. It contains `shape_alpha`, `rate_beta`,
  and `uncertainty`.
- `delta_draws.rds` should come from
  `../attrisk/outputs/conduct/citl/delta_draws.rds`.
- `sobol_lst.rds` should come from `../attrisk/processed_data/sobol_lst.rds`.
- A new `conduct_scale.csv` should be exported from
  `../attrisk/vivli/v2/Summaries/covariate_mu_d.csv` and used to centre-scale
  participant-level conduct inputs.

`tools/export_attrisk_app_data.R` demonstrates this handoff.

## Required app mapping updates

Design-stage inputs need to emit the new model matrix columns:

- Treatment type: `trttypeactive`, `trttypeplacebo_active`.
- Trial size: `n_cat250-499`, `n_cat500-749`, `n_cat750-999`, `n_cat>=1000`.
- Arm size derived from `n / n_arms`: `arm_n_cat100-199`,
  `arm_n_cat200-299`, `arm_n_cat>=300`.
- Arms/sites/outcomes: `n_arms_cat`, `n_sites_cat100-199`,
  `n_sites_cat>=200`, `n_poms_cat`.
- Masking and regions: `maskingdouble`, `maskingtriple`,
  `maskingquadruple`, `region_of_the_americas`, `european_region`,
  `western_pacific_region`, `african_region`, `south_east_asia_region`,
  `eastern_mediterranean_region`.
- Eligibility: `upper_age80`, `upper_age>80`, `lower_age>18`,
  `upper_bmi>30`, `upper_bmi>40`, `lower_bmi<27`, `lower_egfr<30`,
  `lower_egfr<60`.

Conduct-stage participant inputs also need renaming and scaling:

- `t2dur` becomes `durn`.
- `hdl` becomes `chol_hdl`.
- `total` becomes `chol_total`.
- `current` becomes `cursmoke`.
- `conmed` becomes `conmeds`.
- Numeric conduct covariates should be transformed as `(value - mu) / d` using
  `conduct_scale.csv`; the current hard-coded divisors are stale.

## Framework review

- The app duplicates model loading and Gompertz helpers in `design_predict.R`
  and `conduct_design.R`; these should be centralised.
- `conduct_citl.R` was loading large RDS files inside the prediction reactive;
  these should be loaded once at startup.
- The live app should validate that `colnames(design_df$X)` match the input
  mapper output before prediction. Otherwise schema drift fails silently or
  with hard-to-read matrix errors.
- The save-output helpers duplicate `zip_dir()` and write under
  `saved_outputs/<date>`, which can collide between users/sessions. Prefer a
  per-session temporary directory and only expose the downloaded zip.
- Several user-facing strings show mojibake where an en dash, greater-than-or-
  equal sign, or ellipsis was intended; clean these before release.
- The app data directory is gitignored, so the refresh process needs a manifest
  such as `model_manifest.csv` to make deployments auditable.
