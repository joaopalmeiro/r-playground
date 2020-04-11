library(tidyverse)
library(here)
library(lubridate)
library(GGally)
library(gridExtra)
library(patchwork)

data <-
  readr::read_csv(
    "https://raw.githubusercontent.com/data-visualization-society/datavizsociety/master/challenge_data/dvs_challenge_3_membership_anniversary/dvs_challenge_3_membership_anniversary.csv",
    col_types = cols(date = col_date("%m/%d/%Y"))
  )

MONTHS <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

is.na(data) %>% colSums()
data <- data %>% na.omit()
data <-
  data %>% mutate(id = row_number(), month = as.factor(lubridate::month(date)))
is.na(data) %>% colSums()
glimpse(data)

long_data <- data %>% gather(category, average_value, data:society)
glimpse(long_data)

data %>% group_by(month) %>% tally()

# Source: https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
sample <- data %>%
  group_by(month) %>%
  nest() %>%
  ungroup() %>%
  mutate(n = rep(2, length(MONTHS))) %>%
  mutate(samp = map2(data, n, sample_n)) %>%
  select(-data) %>%
  unnest(samp)

shifter <- function(x, n = 1) {
  if (n == 0)
    x
  else
    c(x[length(x)], x[c(1:(length(x) - 1))])
}

generate_monthly_plots <- function(data, months_list) {
  color_list <- c(
    "#69b3a2",
    "#E8E8E8",
    "#E8E8E8",
    "#E8E8E8",
    "#E8E8E8",
    "#E8E8E8",
    "#E8E8E8",
    "#E8E8E8",
    "#E8E8E8",
    "#E8E8E8",
    "#E8E8E8",
    "#E8E8E8"
  )
  
  plots = list()
  
  for (m in months_list) {
    months_list <- c(months_list[-1], months_list[1])
    
    plot <- data %>%
      arrange(factor(month, levels = months_list), desc(month)) %>%
      ggparcoord(
        columns = 3:5,
        groupColumn = 8,
        showPoints = FALSE,
        scale = "globalminmax",
        alphaLines = 1
      ) +
      scale_color_manual(values = color_list) + list(theme_minimal() + theme(legend.position = "none"))
    
    plots[[m]] = plot
    
    color_list <- shifter(color_list)
  }
  
  return(plots)
}

plots_list <- generate_monthly_plots(data, MONTHS)

do.call("grid.arrange", c(plots_list, ncol = 2))
