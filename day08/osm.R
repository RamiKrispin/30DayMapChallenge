# Topic: OpenStreetMap
# Source: https://en.wikipedia.org/wiki/London_Underground
# Source: OpenStreetMap

library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)

text_color <- "black"
fill <- "white"


city_coords <- getbb("New York City")

limits <-  c(city_coords[1,1], city_coords[2,1],
             city_coords[1,2], city_coords[2,2])

available_tags("railway")
available_tags("highway")


residential <-  opq(bbox = limits) %>%
  add_osm_feature(key = "highway",
                  value = c("residential")) %>%
  osmdata_sf(quiet = FALSE)


motorway <- opq(bbox = limits)%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary")) %>%
  osmdata_sf()

unique(streets$osm_lines$name)
ggplot() +
  geom_sf(data = residential$osm_lines,
          inherit.aes = FALSE,
          color = "orange",
          size = .3,
          alpha = .4) +
  geom_sf(data = motorway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .4) +
  theme_void() +
  labs(title = "San Francisco, CA",
       subtitle = "-0.088°N / 51.489°E",
       caption = "#30DayMapChallenge | Viz: Rami Krispin | Data: OpenStreetMap") +
  theme(
    plot.background = element_rect(fill = "white"),
    plot.title= element_text(size=24, hjust=.5,
                             color= text_color,
                             face="bold"),
    plot.caption= element_text(size=9,
                               color= text_color,
                               face="plain",
                               hjust=0.01),
    plot.subtitle = element_text(size = 9, hjust=.5,
                                 color= text_color,
                                 margin=margin(2, 0, 5, 0))
  )
