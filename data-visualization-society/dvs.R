library(tidyverse)
library(here)
library(lubridate)
library(GGally)
library(gridExtra)
library(grid)
library(ggtext)
library(hrbrthemes)

options(pillar.sigfig = 5)

# hrbrthemes::import_roboto_condensed()

data <-
  readr::read_csv(
    "https://raw.githubusercontent.com/data-visualization-society/datavizsociety/master/challenge_data/dvs_challenge_3_membership_anniversary/dvs_challenge_3_membership_anniversary.csv",
    col_types = cols(date = col_date("%m/%d/%Y"))
  )

MONTHS <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

TURQUOISE <- "#2db1a4" # Data
MUSTARD <- "#dcb22a"   # Visualization
PLUM <- "#9f5f9c"      # Society

line_colors <- list(data = TURQUOISE, visualization = MUSTARD, society = PLUM)

is.na(data) %>% colSums()
data <- data %>% na.omit()
data <-
  data %>% mutate(
    id = row_number(),
    month = as.factor(lubridate::month(date)),
    year_month = as.factor(paste(
      lubridate::month(date, label = TRUE), lubridate::year(date)
    ))
  )
is.na(data) %>% colSums()
glimpse(data)

long_data <- data %>% gather(category, average_value, data:society)
glimpse(long_data)

data %>% group_by(month) %>% tally()
long_data %>% group_by(average_value) %>% tally()
available_dates <- data %>% group_by(year_month) %>% tally()
available_dates

MONTHS_YEAR <- as.character(pull(distinct(data, year_month)))
MONTHS_YEAR

# Source: https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
sample <- data %>%
  group_by(year_month) %>%
  nest() %>%
  ungroup() %>%
  mutate(n = rep(2, length(MONTHS_YEAR))) %>%
  mutate(samp = map2(data, n, sample_n)) %>%
  select(-data) %>%
  unnest(samp)

sample

shifter <- function(x, n = 1) {
  if (n == 0) {
    x
  }
  else {
    c(x[length(x)], x[c(1:(length(x) - 1))])
  }
}

color_shifter <- function(x, color, n = 1) {
  if (n == 0) {
    x
  }
  else {
    x <- c(x[length(x)], x[c(1:(length(x) - 1))])
    x[which(!(x %in% x[duplicated(x)]))] <- color
    x
  }
}

generate_monthly_plots <-
  function(data,
           months_list,
           cols,
           group_col,
           month_col,
           secondary_color = "#E8E8E8") {
    month_col <- c(month_col)
    
    color_filter <- data %>%
      group_by(index = get(month_col)) %>%
      summarise(
        data = mean(data),
        visualization = mean(visualization),
        society = mean(society)
      ) %>%
      mutate(higher_mean_value = apply(.[, 2:4], 1, function(x)
        names(x)[which.max(x)])) %>%
      arrange(factor(index, levels = months_list), desc(index))
    
    first_value <- (color_filter %>% head(n=1))$higher_mean_value
    
    color_list <-
      c(line_colors[[first_value]], rep(secondary_color, length(months_list) - 1))
    
    color_filter <- color_filter %>% 
      mutate(next_higher_mean_value=lead(higher_mean_value, default = first_value))
    
    plots = list()
    
    first_col_order <- which(colnames(data) == head(cols, n = 1))
    last_col_order <- which(colnames(data) == tail(cols, n = 1))
    
    for (m in months_list) {
      months_list <- c(months_list[-1], months_list[1])
      
      plot <- data %>%
        arrange(factor(get(month_col), levels = months_list), desc(get(month_col))) %>%
        ggparcoord(
          columns = cols,
          groupColumn = group_col,
          showPoints = FALSE,
          scale = "globalminmax",
          alphaLines = 1,
          order = first_col_order:last_col_order
        ) +
        scale_color_manual(values = color_list) + list(
          theme_ipsum_rc() + theme(
            legend.position = "none",
            panel.grid.minor = element_blank(),
            plot.margin =
              margin(10, 10, 10, 10)
          )
        )
      
      plots[[m]] = plot
      
      update_color <- line_colors[[(color_filter %>% filter(index == m))$next_higher_mean_value]]
      
      color_list <- color_shifter(color_list, update_color)
    }
    
    return(plots)
  }

cols_to_plot <- c("data", "visualization", "society")
plots_list <-
  generate_monthly_plots(sample, MONTHS_YEAR, cols_to_plot, "year_month", "year_month")

# Alternative: do.call("grid.arrange", c(plots_list, ncol = floor(sqrt(length(plots_list)))))
g <- grid.arrange(
  grobs = plots_list,
  layout_matrix = rbind(c(1, 2, 3),
                        c(4, 5, 6),
                        c(7, 8, 9),
                        c(10, 11, 12),
                        c(NA, 13, NA)),
  top = textGrob(
    "Title",
    x = 10,
    hjust = 0,
    gp = gpar(fontsize = 20, fontfamily = "Roboto Condensed"),
    default.units = "points"
  ),
  padding = unit(25, "points"),
  bottom = textGrob(
    "Source: Data Visualization Society | Chart: João Palmeiro",
    gp = gpar(fontsize = 5)
  )
)

# A4 (horizontal)
ggsave("teste.png",
       g,
       width = 297,
       height = 210,
       units = "mm")
