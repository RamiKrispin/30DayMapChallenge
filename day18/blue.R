# Day 18 - Colour Friday: Blue
# Data: https://data.ca.gov/dataset/california-electric-transmission-lines1
# Tutorials
# https://r-graph-gallery.com/325-background-map-from-geojson-format-in-r.html


# packages
library(geojsonio)
library(sp)
library(sf)
library(rnaturalearth)
library(dplyr)
library(ggplot2)

# loading the California electric transmission lines data
spdf <- geojson_read("day18/California_Electric_Transmission_Lines.geojson",
                     what = "sp")
str(spdf)
plot(spdf)

# Convert to sf object
sfdf <- st_as_sf(x = spdf)

# California geomatry
ca <- ne_states(returnclass = 'sf') %>%
  filter(iso_a2 == "US", type == "State",
         name == "California") %>%
  select(name, latitude, longitude)

plot(ca)

ggplot() +
  geom_sf(data = ca,
          inherit.aes = FALSE,
          color = "gray",
          size = .3,
          alpha = .4) +
  geom_sf(data = sfdf,
          inherit.aes = FALSE,
          color = "blue",
          size = .2,
          alpha = .7) +
  theme_void() +
  labs(title = "California Electric Transmission Lines",
       caption = "#30DayMapChallenge | Viz: Rami Krispin | Data: California Data Portal/ Natural Earth") +
  theme(
    plot.background = element_rect(fill = "#fffffc"),
    plot.title= element_text(size=24, hjust=.5,
                             color="black",
                             face="bold"),
    plot.caption= element_text(size=12,
                               color="black",
                               face="plain",
                               hjust=0.1,
                               margin=margin(2, 0, 5, 0))
  )
