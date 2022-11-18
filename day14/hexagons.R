# Tutorial: https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html
# US hexagon data: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
# US population data: United States Census Bureau - https://data.census.gov/cedsci/

# library
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)

# loading the population data
pop_df <- read.csv("./day12/NST-EST2021-alldata.csv") %>%
  setNames(tolower(names(.))) %>%
  select(state, name, pop21 = popestimate2021) %>%
  filter(state != 0) %>%
  arrange(-pop21)
head(pop_df)

min(pop_df$pop21)
quantile(pop_df$pop21)

pop_df$bin <- cut(pop_df$pop21 ,
    breaks=c(0.5 * 10 ^ 6,
             2 * 10 ^ 6,
             4 * 10 ^ 6,
             7 * 10 ^ 6,
             20 * 10 ^ 6,
             30 * 10 ^ 6,
             Inf),
     labels=c("0.5-2", "2-4", "4-7", "7-20", "20-30","30+"),
    include.lowest = TRUE)
# loading US Hexogan data
spdf <- geojson_read("day14/us_states_hexgrid.geojson",  what = "sp")

head(spdf@data)

# clean the data
spdf@data <- spdf@data %>%
  mutate(google_name = trimws(gsub(" \\(United States\\)", "", google_name)))

spdf_fortified <- tidy(spdf, region = "google_name")

head(spdf_fortified)

# Calculate centroids
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

df <- spdf_fortified %>%
  left_join(pop_df, by = c("id" = "name"))
head(df)

palettes <- RColorBrewer::brewer.pal(8, "Blues")[2:8]

ggplot() +
  geom_polygon(data = df, aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="black", size=3, alpha=0.6) +
  theme_void() +
  scale_fill_manual(
    values= palettes,
    name="Population Range Millions",
    guide = guide_legend(keyheight = unit(3, units = "mm"),
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top',
                         nrow=1)
  ) +
  ggtitle("US Estimated Population by State 2021" ) +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    plot.title = element_text(size= 22,
                              hjust=0.5,
                              color = "white",
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

