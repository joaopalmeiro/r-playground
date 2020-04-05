library(rethinking)
library(tidyverse)

set.seed(100)

# 1

# Grid approximation to compute the posterior
p_parameter_grid_approximation <-
  seq(from = 0,
      to = 1,
      length.out = 1e3)

# Uniform, flat prior
p_parameter_prior <- rep(1, 1e3)

# 8 water (W), 15 tosses (T)
likelihood <-
  dbinom(8, size = 15, prob = p_parameter_grid_approximation)

# Bayes' theorem
posterior_numerator <- likelihood * p_parameter_prior

posterior <- posterior_numerator / sum(posterior_numerator)
posterior

samples_1 <-
  sample(
    p_parameter_grid_approximation,
    prob = posterior,
    size = 1e4,
    replace = TRUE
  )

mean(samples_1)
PI(samples_1, prob = 0.99)

# 2

p_parameter_prior <- sort(rep(c(0, 1), 500))
p_parameter_prior

posterior_numerator <- likelihood * p_parameter_prior

posterior <- posterior_numerator / sum(posterior_numerator)
posterior

samples_2 <-
  sample(
    p_parameter_grid_approximation,
    prob = posterior,
    size = 1e4,
    replace = TRUE
  )

mean(samples_2)
PI(samples_2, prob = 0.99)

# 3

interval_width <-
  function(N_sizes,
           seed,
           p_parameter_true = 0.7,
           simulations = 100) {
    if (!missing(seed)) {
      set.seed(seed)
    }
    
    N_sizes_simulations <- sort(rep(N_sizes, simulations))
    
    final_table <- tibble(N = integer(),
                          simulation = integer(),
                          width = double())
    
    simulation <- 0
    
    for (N in N_sizes_simulations) {
      if (simulation == 100) {
        simulation <- 1
      } else {
        simulation <- simulation + 1
      }
      
      W <- rbinom(1, size = N, prob = p_parameter_true)
      
      p_parameter_grid_approximation <-
        seq(from = 0,
            to = 1,
            length.out = 1e3)
      
      p_parameter_prior <- rep(1, 1e3)
      
      likelihood <-
        dbinom(W, size = N, prob = p_parameter_grid_approximation)
      
      posterior_numerator <- likelihood * p_parameter_prior
      posterior <- posterior_numerator / sum(posterior_numerator)
      
      samples <-
        sample(
          p_parameter_grid_approximation,
          prob = posterior,
          size = 1e4,
          replace = TRUE
        )
      
      percentile_interval <- PI(samples, prob = 0.99)
      
      width <-
        as.numeric(percentile_interval[2] - percentile_interval[1])
      
      table <-
        tibble(
          N = as.integer(N),
          simulation = as.integer(simulation),
          width = width
        )
      
      final_table <- bind_rows(final_table, table)
    }
    
    final_table_summary <- final_table %>%
      group_by(N) %>%
      summarise(
        mean = mean(width),
        min = min(width),
        max = max(width),
        n = n()
      )
    
    return(final_table_summary)
  }

N_sizes <- c(20, 50, 100, 200, 500, 1000, 2000, 2500, 3000)
interval_widths <- interval_width(N_sizes, 100)
interval_widths

# Style function.
# Inspired by the BBC style (https://bbc.github.io/rcookbook/).
clean_style <- function() {
  fonttitle <- "Roboto"
  fontsubtitle <- "Roboto Thin"
  
  theme(
    plot.title = element_text(family = fonttitle,
                              color = "#2F2F2F"),
    plot.title.position = "plot",
    plot.subtitle = element_text(
      family = fontsubtitle,
      size = 8,
      color = "#2F2F2F",
      margin = margin(0, 0, 20, 0)
    ),
    axis.title = element_blank(),
    axis.text = element_text(family = fonttitle,
                             color = "#2F2F2F"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
}

k_format <- function() {
  function(x)
    ifelse(x < 1000, x, paste0(x / 1000, "k"))
}

subtitle <-
  bquote("The max, mean and min width values were obtained through" ~ .(toString(unique(interval_widths$n))) ~ "simulations")

ggplot(interval_widths, aes(x = N, y = mean)) +
  geom_errorbar(aes(ymin = min, ymax = max), width = 50, colour = "#2F2F2F") +
  geom_point(
    size = 1,
    shape = 21,
    fill = "white",
    colour = "#2F2F2F"
  ) +
  geom_hline(
    yintercept = 0.05,
    linetype = "dashed",
    color = "#EF4E63",
    size = 0.3
  ) +
  geom_hline(yintercept = 0,
             size = 0.3,
             colour = "#CBCBCB") +
  list(theme_minimal() + clean_style()) +
  scale_x_continuous(expand = c(0, 0), labels = k_format()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Percentile interval width vs sample size", subtitle = subtitle)

# ggsave("figures/percentile_interval_width_vs_sample_size.png")
