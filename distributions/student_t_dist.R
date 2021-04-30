library(here)
library(ggdist)
library(ggplot2)
library(styler)
library(tibble)
library(svglite)

n <- 100
df <- n - 1

dist_df <- tribble(
  ~dist, ~df,
  "t", df
)

# More info:
# - https://mjskay.github.io/ggdist/reference/stat_dist_slabinterval.html
dist_df %>%
  # y = "" or y = 0
  ggplot(aes(dist = dist, arg1 = df)) +
  # Probability density function (pdf)
  stat_dist_slab(
    slab_type = "pdf",
    orientation = "horizontal",
    fill = NA,
    color = "#2c3e50",
    normalize = "all", # The maximum height across all data is 1.
    # normalize = "none"
  ) +
  coord_cartesian(expand = FALSE) +
  xlim(-4, 4) +
  theme_ggdist() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank()
  )

device <- "svg"
# device <- "png"

ggsave(
  here(paste0("student_t_dist.", device)),
  dpi = 320,
  width = 4.5,
  height = 2.5,
  units = "in",
  device = device
)
