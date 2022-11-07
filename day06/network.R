# Plotting the London Tube map
# Source: https://en.wikipedia.org/wiki/London_Underground
# Source: OpenStreetMap

library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)

city_coords <- getbb("London")

limits <-  c(city_coords[1,1], city_coords[2,1],
             city_coords[1,2], city_coords[2,2])

available_tags("railway")
available_tags("highway")


highway <- opq(bbox = limits) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary")) %>%
  osmdata_sf(quiet = FALSE)

residential <-  opq(bbox = limits) %>%
  add_osm_feature(key = "highway",
                  value = c("residential")) %>%
  osmdata_sf(quiet = FALSE)


streets <- opq(bbox = limits)%>%
  add_osm_feature(key = "highway",
                  value = c("living_street")) %>%
  osmdata_sf()


river <- opq(bbox = limits)%>%
  add_osm_feature(key = "waterway", value = c( "river", "riverbank")) %>%
  osmdata_sf()
# Separating between the Thames river and rest
river_thames <- river$osm_lines %>%
  dplyr::filter(name == c("River Thames"))

river_other <- river$osm_lines %>%
  dplyr::filter(name != c("River Thames"))

canal <- opq(bbox = limits)%>%
  add_osm_feature(key = "waterway", value = c("canal")) %>%
  osmdata_sf()

# London tube data
subway <- opq(bbox = limits) %>%
  add_osm_feature(key = "railway",
                  value = c("subway")) %>%
  osmdata_sf(quiet = FALSE)

lines_names <- sort(unique(subway$osm_lines$name))
# Extracting the lines objects
metropolitan <- subway$osm_lines %>%
  dplyr::filter(name %in% c("Metropolitan and Piccadilly Line (Uxbridge Branch)",
                            "Metropolitan Line",
                            "Metropolitan Line (Uxbridge Branch)"))

bakerloo <- subway$osm_lines %>%
  dplyr::filter(name %in% c("Bakerloo Line"))

central <- subway$osm_lines %>%
  dplyr::filter(name %in% c("Central Line",
                            "Central Line - Ealing branch - Eastbound",
                            "Central Line - Ealing branch - Westbound",
                            "Central Line - Eastbound",
                            "Central Line - Westbound",
                            "Central Line (Hainault towards Woodford)",
                            "Central Line (Hainault towards Woodford) - Grange Hill Tunnel",
                            "Central Line (Hainault, Newbury Park, Leytonstone)",
                            "Central Line (Leystonstone, Newbury Park, Hainault)",
                            "Central Line (Woodford Branch to Hainault) - Grange Hill Tunnel",
                            "Central Line (Woodford Branch)"))

circle <- subway$osm_lines %>%
  dplyr::filter(name %in%
                  c("Circle and District Lines",
                    "Circle and Hammersmith & City Lines",
                    "Circle Line",
                    "Circle, District and Hammersmith & City Lines",
                    "Circle, Hammersmith & City and Metropolitan Lines"))

district <-  subway$osm_lines %>%
  dplyr::filter(name %in%
                  c(
                    "District and Hammersmith & City Line",
                    "District and Hammersmith & City Lines",
                    "District and Piccadilly Lines",
                    "District Line",
                    "District Line (Ealing Broadway Branch)" ,
                    "District Line (Edgware Road Branch)",
                    "District Line (Wimbledon Branch)",
                    "District Line (Wimbledon Branch);District Line"
                  ))

hammersmith <- subway$osm_lines %>%
  dplyr::filter(name %in%
                  c(
                    "Hammersmith & City and District Lines",
                    "Hammersmith & City and Metropolitan Lines",
                    "Hammersmith & City Line"
                  ))

jubilee <- subway$osm_lines %>%
  dplyr::filter(name %in%
                  c(
                    "Jubilee line",
                    "Jubilee Line",
                    "Jubilee Line Eastbound",
                    "Jubilee Line Westbound"
                  ))

northern <- subway$osm_lines %>%
  dplyr::filter(name %in%
                  c(
                    "Northern Line",
                    "Northern Line (Bank Branch_)",
                    "Northern Line (Bank Branch)",
                    "Northern Line (Charing Cross Branch)",
                    "Northern Line (Charing Cross Branch) Northbound",
                    "Northern Line (Charing Cross Branch) Southbound",
                    "Northern Line (Edgware Branch)",
                    "Northern Line (High Barnet Branch)",
                    "Northern Line (Mill Hill East Branch)",
                    "Northern line extension"
                  ))

piccadilly <- subway$osm_lines %>%
  dplyr::filter(name %in%
                  c(
                    "Piccadilly Line (Heathrow Branch)",
                    "Piccadilly Line (Heathrow T4 Loop)",
                    "Piccadilly Line (Heathrow Terminal 5 Branch)",
                    "Piccadilly Line (Uxbridge Branch)"
                  ))

victoria <- subway$osm_lines %>%
  dplyr::filter(name %in%
                  c(
                    "Victoria Line",
                    "Victoria Line Northbound",
                    "Victoria Line Southbound"
                  ))

waterloo <- subway$osm_lines %>%
  dplyr::filter(name %in%
                  c(
                    "Waterloo & City Line"
                  ))


subway_entrance <- opq(bbox = limits) %>%
  add_osm_feature(key = "railway",
                  value = c("subway_entrance")) %>%
  osmdata_sf(quiet = FALSE)
# Tube parameters
line_size <- 0.7
line_alpha <- 1


ggplot() +
  geom_sf(data = highway$osm_lines,
          inherit.aes = FALSE,
          color = "gray",
          size = .3,
          alpha = .4) +
  geom_sf(data = residential$osm_lines,
          inherit.aes = FALSE,
          color = "gray",
          size = .2,
          alpha = .4) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "gray",
          size = .2,
          alpha = .8) +
  geom_sf(data = river_thames,
          inherit.aes = FALSE,
          color = "#1d3557",
          size = 2,
          alpha = 1) +
  geom_sf(data = river_other,
          inherit.aes = FALSE,
          color = "#1d3557",
          size = 0.8,
          alpha = 0.5) +
  geom_sf(data = canal$osm_lines,
          inherit.aes = FALSE,
          color = "#a8dadc",
          size = 0.2,
          alpha = 0.5) +
  geom_sf(data = metropolitan,
          inherit.aes = FALSE,
          color = "#FF00FF",
          size = line_size,
          alpha = line_alpha) +
  geom_sf(data = bakerloo,
          inherit.aes = FALSE,
          color = "brown",
          size = line_size,
          alpha = line_alpha) +
  geom_sf(data = central,
          inherit.aes = FALSE,
          color = "red",
          size = line_size,
          alpha = line_alpha) +
  geom_sf(data = circle,
          inherit.aes = FALSE,
          color = "yellow",
          size = line_size,
          alpha = line_alpha) +
  geom_sf(data = district,
          inherit.aes = FALSE,
          color = "green",
          size = line_size,
          alpha = line_alpha) +
  geom_sf(data = hammersmith,
          inherit.aes = FALSE,
          color = "pink",
          size = line_size,
          alpha = line_alpha) +
  geom_sf(data = jubilee,
          inherit.aes = FALSE,
          color = "grey",
          size = line_size,
          alpha = line_alpha) +
  geom_sf(data = northern,
          inherit.aes = FALSE,
          color = "black",
          size = line_size,
          alpha = line_alpha) +
  geom_sf(data = piccadilly,
          inherit.aes = FALSE,
          color = "#00008B",
          size = line_size,
          alpha = line_alpha) +
  geom_sf(data = victoria,
          inherit.aes = FALSE,
          color = "#add8e6",
          size = line_size,
          alpha = line_alpha) +
  geom_sf(data = waterloo,
          inherit.aes = FALSE,
          color = "#40e0d0",
          size = line_size,
          alpha = line_alpha) +
  coord_sf(xlim = city_coords[1,],
           ylim = city_coords[2,],
           expand = TRUE) +
  theme_void() +
  labs(title = "London Tube",
       subtitle = "-0.088°N / 51.489°E",
       caption = "#30DayMapChallenge | Viz: Rami Krispin | Data: OpenStreetMap") +
  theme(
    plot.background = element_rect(fill = "#343a40"),
    plot.title= element_text(size=24, hjust=.5,
                             color="White",
                             face="bold"),
    plot.caption= element_text(size=9,
                               color="White",
                               face="plain",
                               hjust=0.01),
    plot.subtitle = element_text(size = 9, hjust=.5,
                                 color="White",
                                 margin=margin(2, 0, 5, 0))
  )
