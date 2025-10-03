
# Plots from Gompertz predictions
# Hazard, cumulative hazard, cumulative incidence

design_plot <- function(design_predict) {
  reactive({
    est_gomp <- design_predict()
    
    plot_haz <- est_gomp %>% 
      select(est, t, haz) %>% 
      pivot_wider(names_from = "est", values_from = "haz") %>% 
      ggplot(aes(x = t*52, y = mean)) +
      geom_line() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Hazard rate") +
      theme_bw()
    
    plot_chaz <- est_gomp %>% 
      select(est, t, chaz) %>% 
      pivot_wider(names_from = "est", values_from = "chaz") %>% 
      ggplot(aes(x = t*52, y = mean)) +
      geom_line() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Accumulated hazard rate") +
      theme_bw()
    
    plot_incid <- est_gomp %>% 
      select(est, t, incid) %>% 
      pivot_wider(names_from = "est", values_from = "incid") %>% 
      ggplot(aes(x = t*52, y = mean)) +
      geom_step() +
      scale_x_continuous(n.breaks = 8) +
      labs(x = "Duration in weeks", y = "Accumulated incidence") +
      theme_bw()
  })
}