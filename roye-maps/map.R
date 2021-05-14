library(feedeR)
library(sf)
library(fs)
library(tidyverse)
library(lubridate)
library(classInt)
library(tmap)
library(rvest)
library(styler)
library(RColorBrewer)

url <- "http://www.catastro.minhap.es/INSPIRE/buildings/ES.SDGC.bu.atom.xml"

# Import RSS feed with provincial links
prov_enlaces <- feed.extract(url)
prov_enlaces

# Extract the table with the links
prov_enlaces_tab <- as_tibble(prov_enlaces$items) %>%
  # For Windows
  mutate(title = repair_encoding(title))
prov_enlaces_tab

# Filter the province and get the RSS link
val_atom <- prov_enlaces_tab %>%
  filter(str_detect(title, "Valencia")) %>%
  pull(link)
val_atom

# Import the RSS
val_enlaces <- feed.extract(val_atom)

# Get the table with the download links
val_enlaces_tab <- as_tibble(val_enlaces$items) %>%
  mutate(
    title = repair_encoding(title),
    link = repair_encoding(link)
  )
val_enlaces_tab

# Filter the table with the name of the city
val_link <- val_enlaces_tab %>%
  filter(str_detect(title, "VALENCIA")) %>%
  pull(link)
val_link

# Create a temporary file
temp <- tempfile()

# Download the data
download.file(URLencode(val_link), temp)

# Unzip to a folder called `buildings`
unzip(temp, exdir = "buildings")

# Get the path with the file
# GML: Geography Markup Language
file_val <- dir_ls("buildings", regexp = "building.gml")

# Import the data
buildings_val <- st_read(file_val)
buildings_val

clean_buildings_val <- buildings_val %>%
  mutate(
    beginning = str_replace(beginning, "^-", "0000") %>%
      ymd_hms() %>% as_date()
  )
clean_buildings_val

buildings_val %>%
  slice(which(is.na(clean_buildings_val$beginning), arr.ind = TRUE)) %>%
  select(beginning)

# Font
sysfonts::font_add_google("Montserrat", "Montserrat")
showtext::showtext_auto()

# Distribution
clean_buildings_val %>%
  filter(beginning >= "1750-01-01") %>%
  ggplot(aes(beginning)) +
  geom_density(fill = "#2166ac", alpha = 0.7) +
  scale_x_date(
    date_breaks = "20 year",
    date_labels = "%Y"
  ) +
  theme_minimal() +
  theme(
    title = element_text(family = "Montserrat"),
    axis.text = element_text(family = "Montserrat")
  ) +
  labs(y = "", x = "", title = "Evolution of urban development")

# Get the coordinates of Valencia
ciudad_point <- tmaptools::geocode_OSM("Valencia", as.sf = TRUE)

# Project the points
# EPSG: 25830
ciudad_point <- st_transform(ciudad_point, 25830)

# Create the buffer
# Radius of 2.5 km from the city center
point_bf <- st_buffer(ciudad_point, 2500)

# Get the intersection between the buffer and the buildings
buildings_val25 <- st_intersection(clean_buildings_val, point_bf)

# Categorize the year into 15 groups
br <- classIntervals(year(buildings_val25$beginning), 15, "quantile")
lab <- names(print(br, under = "<", over = ">", cutlabels = FALSE))

buildings_val25 <- buildings_val25 %>%
  mutate(
    yr_cl = cut(year(beginning),
      br$brks,
      labels = lab,
      include.lowest = TRUE
    )
  )
buildings_val25

# Colors
col_spec <- brewer.pal(11, "Spectral")
col_spec_fun <- colorRampPalette(col_spec)

# tmap object
m <- tm_shape(buildings_val25) +
  tm_polygons("yr_cl",
    border.col = "transparent",
    palette = col_spec_fun(15),
    textNA = "Without data",
    title = ""
  )

# Create the final map
m +
  tm_layout(
    bg.color = "black",
    outer.bg.color = "black",
    legend.outside = TRUE,
    legend.text.color = "white",
    legend.text.fontfamily = "Montserrat",
    panel.label.fontfamily = "Montserrat",
    panel.label.color = "white",
    panel.label.bg.color = "black",
    panel.label.size = 5,
    panel.label.fontface = "bold"
  )

# Interactive map
# tmap_leaflet(m)
