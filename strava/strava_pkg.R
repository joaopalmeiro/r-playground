library(strava)

data <- process_data("data/activities")
head(data)

p1 <- plot_facets(data)
ggsave(
  "plots/facets.png",
  p1,
  width = 20,
  height = 20,
  units = "cm"
)
