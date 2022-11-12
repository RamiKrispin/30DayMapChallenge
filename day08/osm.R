# Topic: OpenStreetMap
# Source: https://en.wikipedia.org/wiki/London_Underground
# Source: OpenStreetMap

library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)

text_color <- "white"
fill <- "black"


city_coords <- getbb("New York City")
city_coords[2,1] <- 40.49
limits <-  c(city_coords[1,1], city_coords[2,1],
             city_coords[1,2], city_coords[2,2])

available_tags("railway")
available_tags("highway")
available_tags("natural")

residential <-  opq(bbox = limits) %>%
  add_osm_feature(key = "highway",
                  value = c("residential")) %>%
  osmdata_sf(quiet = FALSE)


motorway <- opq(bbox = limits)%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary")) %>%
  osmdata_sf()

water <- opq(bbox = limits)%>%
  add_osm_feature(key = "place",
                  value = c("sea", "ocean")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = residential$osm_lines,
          inherit.aes = FALSE,
          color = "#a8dadc",
          size = .3,
          alpha = .5) +
  geom_sf(data = motorway$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .3,
          alpha = .4) +
  coord_sf(xlim = city_coords[1,],
           ylim = city_coords[2,],
           expand = TRUE) +
  theme_void() +
  labs(title = "New York",
       subtitle = "40.7128° N, 74.0060°W",
       caption = "#30DayMapChallenge | Viz: Rami Krispin | Data: OpenStreetMap") +
  theme(
    plot.background = element_rect(fill = fill),
    plot.title= element_text(size=24, hjust=.5,
                             color= text_color,
                             face="bold"),
    plot.caption= element_text(size=9,
                               color= text_color,
                               face="plain",
                               hjust=0.1),
    plot.subtitle = element_text(size = 9, hjust=.5,
                                 color= text_color,
                                 margin=margin(0, 0, 0, 0))
  )
