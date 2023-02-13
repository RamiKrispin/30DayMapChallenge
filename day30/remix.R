# Day 30 Remix
# Data source Kontur via data.humdata.org
# Download file from here: https://data.humdata.org/dataset/kontur-population-united-states-of-america

library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(camcorder)

# Load Germany dataset
us_df <- st_read("./day30/kontur_population_US_20220630.gpkg")

class(us_df)
length(unique(us_df$h3))
head(us_df)
quantile(us_df$population)
min(us_df$population)
mean(us_df$population)
str(us_df)

# Load Berlin Polygon from Natual Earth
# berlin_df <- ne_states(country = "Germany", returnclass = "sf") %>%
#   filter(name_en == "Berlin")
#
# # Normalize the projection
# st_crs(germany_df[1,])
# st_crs(berlin_df[1,])
#
# df <- st_transform(germany_df, crs = st_crs(berlin_df[1,]))
#
# berlin_hex <- st_intersection(df, berlin_df) %>%
#   select(h3, population, geom)

# Start recording
gg_record(
  dir = file.path("./day30", "gif"),
  device = "png",
  width = 8,
  height = 5,
  units = "in",   # units for width and height
  dpi = 300       # dpi to use when saving image
)


# Create the plot
text <- "white"
background <- "black"
# background <- "#fffffc"

ggplot() +
  geom_sf(data = us_df,
          aes(fill = population),
          lwd = 0.05) +
  scale_fill_viridis_c(option = "magma",begin = 0.3) +
  theme_void()  +
  labs(title = "Population Density in Berlin, Germany",
       subtitle = "Per 400m Hexagon",
       caption = "#30DayMapChallenge | Viz: Rami Krispin | Data: Kontur") +
  theme(
    plot.background = element_rect(fill = background),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title= element_text(size=16, hjust=.1,
                             color = text,
                             face="bold",
                             margin=margin(10, 0, 0, 0)),
    plot.subtitle = element_text(size=12, hjust=0.039,
                                 color = text,
                                 face = "bold"),
    plot.caption= element_text(size = 9,
                               color = text,
                               face = "plain",
                               hjust = 0.9,
                               margin = margin(2, 0, 5, 0)
    ),
    legend.position = c(0.92, 0.8),
    legend.title = element_text(color = text,
                                face = "plain"),
    legend.text = element_text(color = text,
                               face = "plain")
  ) +
  labs(fill = "Population \nDensity")



# Create a gif
gg_playback(name = file.path("./day21", "gif", "berlin_pop.gif"))

# Stop recording
gg_stop_recording()

