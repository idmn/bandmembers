get_plot_data <- function(timeline_code) {
  data <- parse_timeline_code(timeline_code)
  
  period <- list()
  period$from <- lubridate::dmy(data$Period$from)
  if (data$Period$till == "{{#time:d/m/Y}}") {
    period$till <- Sys.Date()
  } else {
    period$till <- dmy(data$Period$till)
  }
  
  data$PlotData %>% 
    select(role_short = color, name_short = bar, from, till) %>% 
    mutate(
      from = case_when(
        from == "start" ~ period$from,
        TRUE ~ dmy(from, quiet = TRUE)
      ),
      till = case_when(
        till == "end" ~ period$till,
        TRUE ~ dmy(till, quiet = TRUE)
      )
    ) %>% 
    left_join(select(data$BarData, name_short = bar, name = text), by = "name_short") %>% 
    left_join(select(data$Colors, role_short = id, role = legend), by = "role_short") %>% 
    # reorder vars
    select(name, name_short, role, role_short, from, till)
}

get_plot_data(timeline_code)
