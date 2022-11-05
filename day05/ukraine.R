# 2022 Russian Invasion of Ukraine
# Source: https://github.com/zhukovyuri/VIINA
# Source: OpenStreetMap
# Reference: https://ggplot2tutor.com/tutorials/streetmaps

df <- readr::read_csv("day05/events_latest.csv")


library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)

city_coords <- getbb("Kiev Ukraine")
# Events table
# How to add it to geom_sf???
kiev_df <- df %>% dplyr::filter(longitude > city_coords[1,1] & longitude > city_coords[1,2],
                                latitude > city_coords[2,1] & latitude > city_coords[2,2])

# Pull the data from OpenStreetMap
limits <-  c(city_coords[1,1], city_coords[2,1],
             city_coords[1,2], city_coords[2,2])

highway <- opq(bbox = limits) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary",
                            "secondary", "tertiary")) %>%
  osmdata_sf(quiet = FALSE)

streets <- opq(bbox = limits)%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- opq(bbox = limits)%>%
  add_osm_feature(key = "waterway", value = c("canal", "river")) %>%
  osmdata_sf()


# Plot
ggplot() +
  geom_sf(data = highway$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .4,
          alpha = .8) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .2,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .2,
          alpha = .5) +
  coord_sf(xlim = city_coords[1,],
           ylim = city_coords[2,],
           expand = FALSE) +
  theme_void() +
  # labs(title = "Kiev",
  #      caption = "Data Source: OpenStreetMap") +
  theme(
    plot.background = element_rect(fill = "#282828"),
    plot.title= element_text(size=12,
                               color="White",
                               face="bold")
  )



