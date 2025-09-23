
# Plots from Gompertz predictions
# Hazard, cumulative hazard, cumulative incidence

design_plot <- function(design_predict) {
  reactive({
    est_gomp <- design_predict()
    
    plot_haz <- est_gomp %>% 
      select(est, t, haz) %>% 
      pivot_wider(names_from = "est", values_from = "haz") %>% 
      ggplot(aes(x = t * 52, y = mean, ymin = `q2.5`, ymax = `q97.5`)) +
      geom_line() +
      geom_ribbon(alpha = 0.3) +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Hazard rate") +
      theme_classic() +
      theme(
        axis.text = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold")
      )
    
    plot_chaz <- est_gomp %>% 
      select(est, t, chaz) %>% 
      pivot_wider(names_from = "est", values_from = "chaz") %>% 
      ggplot(aes(x = t * 52, y = mean)) +
      geom_line() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Accumulated hazard rate") +
      theme_classic() +
      theme(
        axis.text = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold")
      )
    
    plot_incid <- est_gomp %>% 
      select(est, t, incid) %>% 
      pivot_wider(names_from = "est", values_from = "incid") %>% 
      ggplot(aes(x = t * 52, y = mean)) +
      geom_step() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Accumulated incidence") +
      theme_classic() +
      theme(
        axis.text = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold")
      )
  })
}