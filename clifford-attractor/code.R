library(tidyverse)
library(Rcpp)

sourceCpp("attractor.cpp")
sourceCpp("map.cpp")

# Clifford Attractor
a <- -1.25
b <- -1.25
c <- -1.82
d <- -1.91

# General 2D-Map
a1 <- -0.8
a2 <- 0.4
a3 <- -1.1
a4 <- 0.5
a5 <- -0.6
a6 <- -0.1
a7 <- -0.5
a8 <- 0.8
a9 <- 1.0
a10 <- -0.3
a11 <- -0.6
a12 <- -0.3
a13 <- -1.2
a14 <- -0.3

# Clifford Attractor
df <- createCliffordTrajectory(10000000, 0, 0, a, b, c, d)
head(df)

# `46`: the smallest filled point
plot <- ggplot(df, aes(x = x, y = y)) +
  geom_point(shape = 46, alpha = .01) +
  coord_equal() +
  theme_void()

ggsave(
  "attractor.png",
  plot,
  height = 5,
  width = 5,
  units = "in",
  dpi = 320
)

# General 2D-Map
df <- createMapTrajectory(10000000,
                          1,
                          1,
                          a1,
                          a2,
                          a3,
                          a4,
                          a5,
                          a6,
                          a7,
                          a8,
                          a9,
                          a10,
                          a11,
                          a12,
                          a13,
                          a14)

mx <- quantile(df$x, probs = 0.05)
Mx <- quantile(df$x, probs = 0.95)
my <- quantile(df$y, probs = 0.05)
My <- quantile(df$y, probs = 0.95)

df <- df %>% filter(x > mx, x < Mx, y > my, y < My)

plot <- ggplot(df) +
  geom_point(aes(x = x, y = y),
             shape = 46,
             alpha = .01,
             color = "black") +
  coord_fixed() +
  theme_void()

ggsave(
  "map.png",
  plot,
  height = 5,
  width = 5,
  units = "in",
  dpi = 320
)
