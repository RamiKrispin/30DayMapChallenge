# Topic: Red
# Source: https://github.com/RamiKrispin/covid19Italy
# Tutorial: https://ramikrispin.github.io/covid19Italy/articles/geospatial_visualization.html
# GIS Data: rnaturalearth
# COVID19 Data: Italy Department of Civil Protection


library(covid19italy)
library(rnaturalearth)
library(dplyr)
library(mapview)
library(tmap)

# Updating the covid19 data, need to restart after
#update_data()


# Getting Italy GIS data
italy_map <- ne_states(country = "Italy", returnclass = "sf")

italy_map <- italy_map %>%
  select(province = name, region, geometry)

italy_map_region <- italy_map %>%
  group_by(region) %>%
  summarise(n = n())


italy_map_region <- italy_map_region %>%
  left_join(italy_region %>%
              filter(date == max(date)), # subseting for the most recent day
            by = c("region" = "region_spatial"))


italy_map_province <- italy_map %>%
  left_join(italy_province %>%
              filter(date == max(date)), # subseting for the most recent day
            by = c("province" = "province_spatial"))
# Plotting with the mapview package
italy_map_province %>%
  mapview(zcol = "total_cases", col.regions = viridisLite::rocket)

# Plotting with the tmap package
tm_shape(italy_map_province) +
  tm_polygons(col = "total_cases",
              n = 10,
              title = "Total Cases",
              palette = "Reds") +
  tm_style("cobalt") +
  tm_layout(
    title= "Total COVID-19 Cases in Italy by Province",
    title.position = c('left', 'top'),
    legend.title.size = 1,
    legend.text.size = 0.6,
    legend.position = c(0.7, 0.6),
    inner.margins = c(0, .1, .1, .22)) +
  tm_credits(text = "#30DayMapChallenge | Viz: Rami Krispin | Data: Italy Department of Civil Protection",
             position = c("left", "bottom"))
