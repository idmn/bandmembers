library(lubridate)
library(ggplot2)
library(forcats)

bandmembers <- readRDS("data/bandmembers.Rds")

breaks_years <- seq(
  from = year(period$from), to = year(period$till),
  by = max(round((year(period$till) - year(period$from))/10), 1)
)

breaks <- paste0(breaks_years, "-01-01") %>% as.Date()

plot_data %>% 
  arrange(role, desc(from)) %>% 
  mutate(name = fct_inorder(name)) %>% 
  ggplot(aes(x = from, xmin = from, xmax = till, y = name, fill = role)) +
  # TODO make zero duration thicker
  geom_crossbar(x = period$from, xmin = period$from, xmax = period$till,
                width = .7, fatten = .3, fill = "gray95", color = NA) +
  #geom_crossbar(position = position_dodge2(width = .1, padding = 0.2), color = NA) +
  geom_crossbar(position = position_dodge(width = .8), color = NA, width = .6) +
  #geom_errorbar(position = position_dodge(preserve = "single"), width = 0, size = 2) +
  scale_x_date(
    expand = c(.02, 0),
    breaks = breaks, 
    labels = breaks_years
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("AC/DC")
  NULL
