library(here)
library(ggdist)
library(ggplot2)
library(styler)
library(tibble)
library(svglite)
library(haven)

n <- 100
df <- n - 1
alpha <- 0.05
mu <- 40
quantile <- qt(1 - alpha, df)

font_size <- 11

data <- read_sav("data.sav")
mean(data$Idade)
sd(data$Idade)

t_test <- t.test(
  data$Idade,
  conf.level = 1 - alpha,
  alternative = "less",
  mu = mu
)

# ~ -1.956816
statistic <- unname(t_test[["statistic"]])
statistic

dist_df <- tribble(
  ~dist, ~df,
  "t", df
)

x_axis_labels <- c(
  # -4,
  statistic,
  0,
  quantile,
  4
)
names(x_axis_labels) <- c(
  # "-4",
  toString(round(statistic, 2)),
  "0",
  toString(round(quantile, 2)),
  paste0("t(", df, ")")
)
x_axis_labels

x_axis_face <- c(
  # "plain",
  "bold",
  "plain",
  "bold",
  "plain"
)

x_axis_hjust <- c(
  0.5,
  0.5,
  0.5,
  1 # or 0
)

# More info:
# - https://mjskay.github.io/ggdist/reference/stat_dist_slabinterval.html
# - https://www.nordtheme.com/docs/colors-and-palettes
dist_df %>%
  # y = "" or y = 0
  ggplot(aes(dist = dist, arg1 = df)) +
  # Probability density function (pdf)
  stat_dist_slab(
    aes(fill = stat(x > quantile)),
    show.legend = FALSE,
    slab_size = 1, # Stroke width
    slab_type = "pdf",
    orientation = "horizontal",
    # fill = NA,
    color = "#3B4252",
    normalize = "all", # The maximum height across all data is 1.
    # normalize = "none"
  ) +
  geom_segment(
    aes(
      x = 0,
      y = 0,
      xend = 0,
      yend = 0.9
    ),
    color = "#D8DEE9",
    # linetype = "dotdash"
    linetype = "longdash"
  ) +
  geom_segment(
    aes(
      x = statistic,
      y = 0,
      xend = statistic,
      yend = (dt(statistic, 99) * 0.9) / dt(0, 99)
    ),
    color = "#D8DEE9",
    # linetype = "dotdash"
    linetype = "longdash"
  ) +
  # Add a repeated layer, but without the filled area,
  # in order to overlap the ends of the reference lines.
  # Comment in case of post-editing in Figma.
  stat_dist_slab(
    slab_size = 1, # Stroke width
    slab_type = "pdf",
    orientation = "horizontal",
    fill = NA,
    color = "#3B4252",
    normalize = "all",
  ) +
  scale_fill_manual(values = c("#ECEFF4", "#81A1C1")) +
  scale_x_continuous(
    breaks = x_axis_labels,
    limits = c(-4, 4),
  ) +
  coord_cartesian(expand = FALSE) +
  # xlim(-4, 4) +
  theme_ggdist() +
  theme(
    plot.margin = margin(
      t = 0,
      r = font_size / 2, # or font_size * 3
      b = font_size / 2,
      l = font_size / 2, # or font_size
      unit = "pt"
    ),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_line(color = "#D8DEE9"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(
      color = "#3B4252",
      size = font_size,
      face = x_axis_face,
      hjust = x_axis_hjust
    ),
  )

# device <- "svg"
device <- "png"

ggsave(
  here(paste("student_t_dist", device, sep = ".")),
  dpi = 320,
  width = 5.5,
  height = 3.5,
  units = "in",
  device = device
)
