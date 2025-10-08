
# Generate summary table (DT)
conduct_sum <- function(conduct_df) {
  df <- conduct_df %>%
    select(t_wks, compl_q50, drp_q50, contains(c("surv", "risk"))) %>% 
    mutate(
      across(c(t_wks, compl_q50, drp_q50, starts_with("at_risk")), ~ round(.)),
      across(starts_with("risk"), ~ round(. * 100, 2)),
      across(c(t_wks, compl_q50, drp_q50), ~ as.integer(.))
    ) %>% 
    select(t_wks, compl_q50, drp_q50, risk_q50) %>%
    rename(
      "Follow-up time (weeks)" = t_wks,
      "Participants at risk" = compl_q50,
      "Non-completers" = drp_q50,
      "Attrition risk (%)" = risk_q50,
    )
  
  tbl <- DT::datatable(
    df,
    rownames = FALSE,
    class = "compact stripe hover cell-border",
    lazyRender = TRUE,
    options = list(
      scrollY = 320,
      scrollX = FALSE,
      scrollCollapse = TRUE,
      paging = FALSE,
      searching = FALSE,
      fixedHeader = TRUE,
      autoWidth = TRUE,
      columnDefs = list(
        list(className = "dt-head-center dt-body-center", targets = "_all")
      )
    )
  )
  
  return(list(df = df, dt = tbl))
}