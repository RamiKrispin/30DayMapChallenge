# Mapping meteorite landings
# Source: https://data.nasa.gov/Space-Science/Meteorite-Landings/ak9y-cwf9
# API: https://data.nasa.gov/resource/gh4g-9sfh.json

library(data.table)
library(dplyr)
library(geojsonio)
library(sp)
library(leaflet)


cmd <- "curl 'https://data.nasa.gov/resource/gh4g-9sfh.json?$limit=50000' | jq  -r '.[] | [.name, .nametype, .recclass, .mass, .fall, .year, .reclat, .reclong] | @csv'"

raw <- fread(cmd = cmd,
             col.names = c("name", "nametype", "recclass",
                           "mass", "fall", "year_temp", "lat", "long"))


table(is.na(raw$lat))
table(is.na(raw$long))
table(is.na(raw$mass))
table(is.na(raw$recclass))
table(raw$recclass)
table(raw$fall)
df <- raw %>%
  dplyr::filter(!is.na(lat),
                !is.na(long),
                !is.na(mass)) %>%
  as.data.frame()
hist(df$mass, breaks = 30)
hist((df %>% dplyr::filter(mass < max(mass, na.rm = TRUE)))$mass, breaks = 30)


# taking few mins to run this loop (probably would be faster with lapply)
for (i in 1:nrow(df)) {
  coords <- c(df$long[i], df$lat[i])
  if(any(is.na(coords))) next
  point <- sp::SpatialPoints(
    matrix(
      coords,
      nrow = 1
    )
  )
}


# Set the color bins
bpal <- colorBin("Reds", log(df$mass), bins = 10)

# Plot the data
leaflet(data = df) %>% addTiles() %>%
  addCircleMarkers(lng = ~ long,
                   lat = ~ lat,
                   opacity = 0.7,
                   color = ~ bpal(log(mass)),
                   fillColor = ~ bpal(log(mass)),
                   radius = ~ log(mass) / 50 ) %>%
  addLegend(position = "bottomright",
            pal = bpal,
            values = ~ log(mass),
            title = "Meteorite Mass <br> Log Scale",
            opacity = 1)
