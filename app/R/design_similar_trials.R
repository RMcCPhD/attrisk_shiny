
# Find 5 trials most similar in design features to the user trial
# Used to plot overall attrition risk for these trials against user trial
# Significant predictors (age_upper, Americas region) and core design features
# (trttype, phase, blinding, duration) are targets
# Uses Gower distance to gauge dissimilarity between each feature in trial set
# and user trial

predict_top5 <- function(design_inputs, input_flag) {
  reactive({
    shiny::req(isTRUE(input_flag()))
    
    # Define columns to check
    core_prefixes <- c("^reg_am$", "^blinding", "^trttype", "^phase", "age_upper")
    core_fixed    <- c("duration", "n_arms")
    X <- imp_df$X
    avail <- colnames(X)
    
    # Get target columns from model matrix
    from_prefix <- unique(unlist(lapply(core_prefixes, function(p) avail[grepl(p, avail)])))
    targets <- unique(c(intersect(core_fixed, avail), from_prefix))
    targets <- intersect(targets, map_nms$param)
    stopifnot(length(targets) > 0)
    
    # Matrix for target variables
    X_sel <- as_tibble(X[, targets, drop = FALSE]) %>%
      mutate(trial_id = imp_trials$trial_id) %>%
      relocate(trial_id)
    
    # Build user input row
    input_param <- design_inputs()
    
    user_row <- input_param %>%
      filter(param %in% targets) %>%
      select(param, value_prep) %>%
      mutate(value_prep = as.numeric(value_prep)) %>%
      pivot_wider(names_from = param, values_from = value_prep) %>%
      mutate(trial_id = "user") %>%
      relocate(trial_id)
    
    # Ensure target variables exist
    missing_cols <- setdiff(colnames(X_sel), colnames(user_row))
    if (length(missing_cols)) {
      for (cc in missing_cols) user_row[[cc]] <- 0
    }
    user_row <- user_row[, colnames(X_sel)]
    
    # Compute Gower distance (i.e. dissimilarity between data points)
    X_all <- dplyr::bind_rows(X_sel, user_row)
    
    # Weights for each design feature
    wts <- rep(1, ncol(X_all) - 1)
    names(wts) <- colnames(X_all)[-1]
    
    # Upweight core design features
    bump_list <- c(duration = 1.5, n_arms = 1.2, age_upper = 1.5)
    for (nm in names(bump_list)) if (nm %in% names(wts)) wts[nm] <- bump_list[[nm]]
    wts[grepl("^phase", names(wts))]    <- wts[grepl("^phase", names(wts))] * 1.5
    wts[grepl("^blinding", names(wts))] <- wts[grepl("^blinding", names(wts))] * 1.5
    
    # Treat reg_am asymmetrically (i.e. presence matters)
    type_list <- list(asymm = which(grepl("^reg_am$", names(wts))))
    
    gower_mat <- cluster::daisy(
      X_all[, -1],
      metric  = "gower",
      weights = unname(wts),
      type    = type_list
    ) %>% as.matrix()
    
    # Extract 5 most similar trials (last row is user row)
    d_user <- gower_mat[nrow(gower_mat), -nrow(gower_mat)]
    top_idx <- order(d_user)[1:5]
    top_ids <- X_sel$trial_id[top_idx]
    
    # Tibble of risk for 5 most similar trials
    top5_df <- imp_trials %>%
      filter(trial_id %in% top_ids) %>%
      group_by(trial_id) %>%
      summarise(
        n = sum(arm_n),
        attr = sum(attr),
        duration = first(duration),
        .groups = "drop"
      ) %>%
      mutate(
        risk_q50 = round(attr / n * 100, 2),
        t_wks = round(duration * 52)
      ) %>%
      arrange(match(trial_id, top_ids))
    
    top5_df
  })
}
