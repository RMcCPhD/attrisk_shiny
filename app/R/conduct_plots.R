
# Generate risk and instantaneous hazard plots
conduct_risk_plot <- function(conduct_df) {
  
  # Compute x and y limits
  y_min <- min(round(conduct_df$risk_q025 * 100, 2), na.rm = TRUE)
  y_max <- max(round(conduct_df$risk_q975 * 100, 2), na.rm = TRUE)
  
  x_min <- min(conduct_df$t_wks, na.rm = TRUE)
  x_max <- max(conduct_df$t_wks, na.rm = TRUE)
  
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
    scale_x_continuous(n.breaks = 8, limits = c(x_min, x_max)) +
    scale_y_continuous(n.breaks = 8, limits = c(y_min, y_max)) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20, face = "bold")
    )
  p
}
