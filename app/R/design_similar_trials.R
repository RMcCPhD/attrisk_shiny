
# Find the five trials most similar to the user-entered design
predict_top5 <- function(design_inputs, input_flag) {
  reactive({
    shiny::req(isTRUE(input_flag()))
    
    X <- attrisk_design_df$X
    targets <- colnames(X)
    
    X_sel <- as_tibble(X[, targets, drop = FALSE]) %>%
      mutate(
        trial_id = attrisk_trials$trial_id,
        duration = attrisk_trials$duration
      ) %>%
      relocate(trial_id)
    
    input_param <- design_inputs()
    user_x <- attrisk_model_values(input_param, targets)
    user_duration <- attrisk_input_value(input_param, "duration")
    
    user_row <- as_tibble_row(as.list(user_x)) %>%
      mutate(
        trial_id = "user",
        duration = user_duration
      ) %>%
      relocate(trial_id)
    
    user_row <- user_row[, colnames(X_sel)]
    X_all <- bind_rows(X_sel, user_row)
    
    # Give slightly more influence to clinically important design dimensions
    wts <- rep(1, ncol(X_all) - 1)
    names(wts) <- colnames(X_all)[-1]
    wts["duration"] <- 1.5
    wts[grepl("^trttype", names(wts))] <- wts[grepl("^trttype", names(wts))] * 1.5
    wts[grepl("^masking", names(wts))] <- wts[grepl("^masking", names(wts))] * 1.5
    wts[grepl("^n_cat|^arm_n_cat|^n_sites_cat|^n_arms_cat", names(wts))] <-
      wts[grepl("^n_cat|^arm_n_cat|^n_sites_cat|^n_arms_cat", names(wts))] * 1.2
    
    type_list <- list(
      asymm = which(grepl("region|trttype|masking", names(wts)))
    )
    
    # Gower distance handles the mix of binary indicators and duration
    gower_mat <- cluster::daisy(
      X_all[, -1],
      metric = "gower",
      weights = unname(wts),
      type = type_list
    ) %>%
      as.matrix()
    
    d_user <- gower_mat[nrow(gower_mat), -nrow(gower_mat)]
    top_idx <- order(d_user)[seq_len(min(5, length(d_user)))]
    top_ids <- X_sel$trial_id[top_idx]
    
    # Return one observed final-risk point per matched trial
    attrisk_trials %>%
      filter(.data$trial_id %in% top_ids) %>%
      group_by(.data$trial_id) %>%
      summarise(
        n = sum(.data$arm_n, na.rm = TRUE),
        attr = sum(.data$arm_attr, na.rm = TRUE),
        duration = first(.data$duration),
        .groups = "drop"
      ) %>%
      mutate(
        risk_q50 = round(.data$attr / .data$n * 100, 2),
        t_wks = round(.data$duration * 52)
      ) %>%
      arrange(match(.data$trial_id, top_ids))
  })
}
