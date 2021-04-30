library(here)
library(ggdist)
library(ggplot2)
library(styler)
library(tibble)
library(svglite)

n <- 100
df <- n - 1
alpha <- 0.05

dist_df <- tribble(
  ~dist, ~df,
  "t", df
)

# More info:
# - https://mjskay.github.io/ggdist/reference/stat_dist_slabinterval.html
# - https://www.nordtheme.com/docs/colors-and-palettes
dist_df %>%
  # y = "" or y = 0
  ggplot(aes(dist = dist, arg1 = df)) +
  # Probability density function (pdf)
  stat_dist_slab(
    aes(fill = stat(x > qt(1 - alpha, df))),
    show.legend = FALSE,
    slab_type = "pdf",
    orientation = "horizontal",
    # fill = NA,
    color = "#3B4252",
    normalize = "all", # The maximum height across all data is 1.
    # normalize = "none"
  ) +
  scale_fill_manual(values = c("#ECEFF4", "#81A1C1")) +
  coord_cartesian(expand = FALSE) +
  xlim(-4, 4) +
  theme_ggdist() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_line(color = "#D8DEE9"),
    axis.ticks.x = element_line(color = "#D8DEE9"),
    axis.text.x = element_text(
      color = "#3B4252", size = 11
    ),
  )

device <- "svg"
# device <- "png"

ggsave(
  here(paste0("student_t_dist.", device)),
  dpi = 320,
  width = 5.5,
  height = 3.5,
  units = "in",
  device = device
)
