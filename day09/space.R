# Day 9 - Space
# Plotting raster map of the moon
# Data source:
# Moon raster - NASA, https://svs.gsfc.nasa.gov/cgi-bin/details.cgi?aid=4720
# Mars Olympus Mons volcano raster - NASA, https://www.jpl.nasa.gov/images/pia09150-olympus-mons
# Mars Olympus Mons volcano tiff - NASA, https://www.jpl.nasa.gov/images/pia00300-olympus-mons
# Mars tiles - http://www.mars.asu.edu/data/mola_color/
library(raster)
library(camcorder)
library(ggplot2)
library(sf)

# Load the data
moon <- raster("./day09/lroc_color_poles.tif")
plot(moon)

# mars <- raster("./day09/PIA09150.tif")
# mars <- raster("./day09/Olympus Mons.png")
mars <- raster("./day09/Olympus Mons 2.tif")
plot(mars)


# Converting to tabular format
# Moon data is too big
moon_spdf <- as(moon, "SpatialPixelsDataFrame")
moon_df <- as.data.frame(moon_spdf)
colnames(test_df) <- c("value", "x", "y")

mars_spdf <- as(mars, "SpatialPixelsDataFrame")
mars_df <- as.data.frame(mars_spdf)
colnames(mars_df) <- c("value", "x", "y")
rm(mars_spdf)
gc()



# Start recording ----
# gg_record(
#   dir = file.path("./day09", "gif"),
#   device = "png",
#   width = 8,
#   height = 5,
#   units = "in",
#   dpi = 350
# )

# Plot ----
olympus <- mars_df %>%
  dplyr::filter(x > 210 & x < 4500,
                y > 10 & y < 4170)

quantile(olympus$value)

olympus$bins <- cut(olympus$value,
                    breaks=c(0,
                             20,
                             50,
                             75,
                             100,
                             200,
                             225,
                             250,
                             Inf),
                    labels=c("0-20", "20-50",
                             "50-75", "75-100", "100-200",
                             "200-225","225-250", "250+"),
                    include.lowest = TRUE)

palette <- rev(colorRampPalette(RColorBrewer::brewer.pal(length(unique(olympus$bins)), "Oranges"))(length(unique(olympus$bins))))


description <- paste("Olympus Mons is a shield volcano on Mars and ",
                     "the highest known mountain in the solar system",
                     sep = "")

peak <- paste("The Olympus Mons is rising to a height of 21,229 meters above ",
              "Datum (equivalent to Earth's sea level). This is Olympus Mons is",
              "about two and a half times Mount Everest's height above sea level",
              sep = "\n")

# size <- "The mountain size is about the same\nas the size of the state of Missouri"
size <- paste("Olympus Mons covers an area of about 300,000 km\n",
              "which is approximately the size of Italy or the Philippines",
              sep = "")
summit <- paste("The summit of the mountain has six nested",
                "calderas (collapsed craters) forming",
                "an irregular depression 60 km by 80 km",
                "across and up to 3.2 km deep",
                sep = "\n")

annotation_df <- data.frame(text = c(peak, size, summit),
                            x = c(3200, 600,3500),
                            y = c(3500, 700, 950),
                            xend = c(2425, 1000, 2500),
                            yend = c(2075, 1300, 1800),
                            hjust = c(0.25, 0, 0),
                            vjust = c(0),
                            size = c(0.5, 0.5, 0.5),
                            curvature = c(0.3, - 0.3, 0.25),
                            angle = c(90, 90, 90))
# text settings
background <- "black"
text <- "#e9ecef"
annotation_size <- 3

ggplot() +
  geom_raster(data = olympus,
              inherit.aes = FALSE,
              aes(
                x = x,
                y = y,
                # fill = bins
                fill = value
              )) +
  # scale_fill_manual(values= palette) +
  scale_fill_viridis_c(option = "magma",
                       begin = 0.05,
                       end = 0.9,
                       direction = -1) +
  labs(title = "Olympus Mons Volcano, Mars",
       subtitle = description,
       caption = "Viz: Rami Krispin | #30DayMapChallenge | Data: Arizona State University, Mars Space Flight Facility | Info: Wikipedia") +
  theme_void() +
  theme(legend.position="none",
        plot.caption= element_text(size = 10,
                                   color = text,
                                   face = "plain",
                                   hjust = 0.45,
                                   margin = margin(0, 0, 5, 0)),
        plot.subtitle = element_text(size = 12, hjust = 0.5,
                                     color= text,
                                     margin=margin(5, 0, 0, 0)),
        plot.title= element_text(size=20,
                                 hjust=.5,
                                 color = text,
                                 face="bold",
                                 margin=margin(5, 0, 0, 0)),
        plot.background = element_rect(fill = background)) +
  geom_curve(data= annotation_df[1,],
             aes(x = x, y = y,
                 xend = xend, yend = yend),
             color = text,
             arrow = arrow(length = unit(0.02, "npc")),
             size = annotation_df$size[1],
             angle = annotation_df$angle[1],
             curvature = annotation_df$curvature[1]
  ) +
  geom_text(
    data = annotation_df[1,],
    aes(
      x = x - 300, y = y + 50,
      label = text,
      hjust = hjust,
      vjust = vjust
    ),
    size = annotation_size,
    color = text) +
  geom_curve(data= annotation_df[2,],
             aes(x = x, y = y,
                 xend = xend, yend = yend),
             color = text,
             arrow = arrow(length = unit(0.02, "npc")),
             size = annotation_df$size[2],
             angle = annotation_df$angle[2],
             curvature = annotation_df$curvature[2]
  ) +
  geom_text(
    data = annotation_df[2,],
    aes(
      x = x - 300, y = y,
      label = text,
      hjust = hjust,
      vjust = 1.4
    ),
    size = annotation_size,
    color = text)  +
  geom_curve(data= annotation_df[3,],
             aes(x = x, y = y,
                 xend = xend, yend = yend),
             color = text,
             arrow = arrow(length = unit(0.02, "npc")),
             size = annotation_df$size[3],
             angle = annotation_df$angle[3],
             curvature = annotation_df$curvature[3]
  ) +
  geom_text(
    data = annotation_df[3,],
    aes(
      x = x- 400, y = y -700,
      label = text,
      hjust = hjust,
      vjust = vjust
    ),
    size = annotation_size,
    color = text)

# Create a gif ----
gg_playback(name = file.path("./day09", "gif", "mars.gif"))

# Stop recording ----
gg_stop_recording()
