
# Generate risk and instantaneous hazard plots
conduct_risk_plot <- function(conduct_df, top5_df = NULL, show_top5 = FALSE) {
  p <- conduct_df %>%
    mutate(across(contains("risk_"), ~ round(. * 100, 2))) %>% 
    ggplot(aes(x = t_wks, y = risk_q50)) +
    geom_line() +
    geom_ribbon(
      aes(ymin = risk_q025, ymax = risk_q975),
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
  
  if (isTRUE(show_top5) && !is.null(top5_df) && nrow(top5_df) > 0) {
    p <- p + 
      geom_point(
        data = top5_df, 
        aes(x = t_wks, y = risk_q50, group = trial_id),
        size = 5,
        shape = 4
      )
  }
  p
}
