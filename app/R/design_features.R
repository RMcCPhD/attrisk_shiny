# Save a table of trial design features that generated outputs.

d_features <- function(design_inputs) {
  # Keep only user-facing display rows; internal model covariates are omitted.
  attrisk_feature_table(design_inputs, "Design features")
}
