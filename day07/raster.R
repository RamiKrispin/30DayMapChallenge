# Day 7 - Roster
# Great tutorial: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
library(raster)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(camcorder)


# Load Country data with states polygons
gis_df <- ne_states(country = "Peru", returnclass = "sf")

plot(gis_df)

# Load the raster data
raster_raw <- getData("alt",
                    country = unique(gis_df$adm0_a3),
                    path = "./day07") %>%
  raster::as.data.frame(xy=TRUE)

names(raster_raw)[which(names(raster_raw) == paste(unique(gis_df$adm0_a3),
                                                 "_msk_alt",
                                                 sep = ""))] <- "alt"

raster_df <- raster_raw %>%
  filter(!is.na(alt))

# Set altitude bins
quantile(raster_df$alt)


raster_df$bins <- cut(raster_df$alt ,
                      breaks=c(min(raster_df$alt),
                               0,
                               250,
                               500,
                               2500,
                               4000,
                               5000,
                               Inf),
                      labels=c("-300-0", "0-250", "250-500",
                               "500-2500", "2500-4000", "4000-5000",
                               "5000+"),
                      include.lowest = TRUE)

table(raster_df$bins)

head(raster_df)
palette <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(length(unique(raster_df$bins))))
# Start recording
gg_record(
  dir = file.path("./day07", "gif"),
  device = "png",
  width = 8,
  height = 5,
  units = "in",
  dpi = 350
)

# Plot ----
background <- "black"
text <- "white"
# Set annotations
annotation_df <- data.frame(city = c("Lima", "The Andes\nMountains", "Amazon\nRainforest"),
                            x = c(-78, -80.2, -72),
                            y = c(-14.5, -9.5, -7.5),
                            xend = c(-77.0428, -78, -74),
                            yend = c(-12.0464, -9.7, -5),
                            hjust = c(1.1, 0.5, - 0.05),
                            vjust = c(0.35, 0, 0.5),
                            size = c(0.5),
                            curvature = c(0.2, 0.2, 0.2),
                            angle = c(90, 180, 90))

ggplot() +
  geom_raster(
    data = raster_df,
    inherit.aes = FALSE,
    aes(
      x = x,
      y = y,
      fill = bins
    )) +
  scale_fill_manual(
    values= palette,
    name="Altitude Levels (Meters above Sea Level)",
    guide = guide_legend(keyheight = unit(3, units = "mm"),
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top',
                         nrow=1)) +
  theme_void() +
  labs(title = "Peru Altitude Map",
       caption = "#30DayMapChallenge | Viz: Rami Krispin | Data: www.worldclim.org") +
  theme(
    plot.title= element_text(size=24, hjust=.05,
                             color = text,
                             face="bold",
                             margin=margin(10, 0, 0, 0)),
    plot.background = element_rect(fill = background),
    plot.caption= element_text(size = 8,
                               color = text,
                               face = "plain",
                               hjust = 0.95,
                               margin = margin(0, 0, 7, 0)),
    legend.title = element_text(color = text,
                                face = "plain"),
    legend.text = element_text(color = text,
                               face = "plain"),
    legend.position = c(0.3, 0.1)
  ) +
  geom_curve(data= annotation_df,
             aes(x = x, y = y,
                 xend = xend, yend = yend),
             color = text,
             arrow = arrow(length = unit(0.02, "npc")),
             size = annotation_df$size,
             #angle = annotation_df$angle,
             # curvature = annotation_df$curvature
  ) +
  geom_text(
    data = annotation_df,
    aes(
      x = x, y = y,
      label = city,
      hjust = hjust,
      vjust = vjust
    ),
    size = 4,
    color = text
  )


# Create a gif
gg_playback(name = file.path("./day07", "gif", "raster1.gif"))

# Stop recording
gg_stop_recording()

