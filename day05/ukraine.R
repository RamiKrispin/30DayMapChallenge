# 2022 Russian Invasion of Ukraine
# Data
# Source: https://github.com/zhukovyuri/VIINA
# Source: OpenStreetMap
# Reference:
# https://ggplot2tutor.com/tutorials/streetmaps
# http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
# https://taraskaduk.com/posts/2021-01-18-print-street-maps/

df <- readr::read_csv("day05/events_latest.csv")


library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)

city_coords <- matrix(data = c(30.0682, 50.2742, 31.1641, 50.6956),
                      nrow = 2, ncol = 2)

city_coords <- getbb("Kyiv Ukraine")

kyiv_df <- df %>% dplyr::filter(longitude > city_coords[1,1] & longitude > city_coords[1,2],
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
  add_osm_feature(key = "waterway", value = c("canal", "river", "riverbank")) %>%
  osmdata_sf()

# water_osm <- opq(bbox = limits) %>%
#   add_osm_feature(key = "natural", value = "water") %>%
#   osmdata_sf() %>%
#   unname_osmdata_sf()
#
# river_osm <- opq(bbox = limits) %>%
#   add_osm_feature(key = "waterway", value = c("river", "riverbank")) %>%
#   osmdata_sf() %>%
#   unname_osmdata_sf()

# water <- c(water_osm, river_osm) %>%
#   .$osm_multipolygons %>%
#   dplyr::select(osm_id, name) %>%
#   dplyr::mutate(area = st_area(.)) %>%
#   # this filter gets rid of tiny isolated lakes et cetera
#   filter(area >= quantile(area, probs = 0.75))

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
  geom_point(data = kyiv_df,
             mapping = aes(x = longitude, y = latitude),
             colour = "red") +
  coord_sf(xlim = city_coords[1,],
           ylim = city_coords[2,],
           expand = TRUE) +
  theme_void() +
  labs(title = "Kyiv",
       subtitle = "30.616°N / 50.484°E",
       caption = "Data Source: OpenStreetMap") +
  theme(
    plot.background = element_rect(fill = "#282828"),
    plot.title= element_text(size=24, hjust=.5,
                               color="White",
                               face="bold"),
    plot.caption= element_text(size=8,
                             color="White",
                             face="plain",
                             hjust=0.01),
    plot.subtitle = element_text(size = 8, hjust=.5,
                                 color="White",
                                 margin=margin(2, 0, 5, 0))
  )

ggsave("day5.png", width = 6, height = 6)


