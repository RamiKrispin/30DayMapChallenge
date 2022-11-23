# Day 10 - Bad map
# Plotting sf object using the base plot function
# This, by default, the function will plot any objects (e.g., codes, names),
# regardless if it relevent or have context (e.g, numeric, continues, etc.)

library(rnaturalearth)

us_df <- ne_states(country = "United States of America", returnclass = "sf")


plot(us_df)
