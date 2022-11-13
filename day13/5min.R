# Day 13 - 5 minute map
# Plotting San Francisco COVID-19 cases count by geography
# Data: covid19sf package via San Francisco, Department of Public Health
# Tutorial: https://ramikrispin.github.io/covid19sf/articles/geo.html
library(covid19sf)
library(mapview)
library(dplyr)
# Can refresh the package datasets with the following function:
# covid19sf_refresh()

# loading the data, object already sf
data(covid19sf_geo)
class(covid19sf_geo)

covid19sf_geo %>%
  filter(area_type == "ZCTA") %>%
  mapview(zcol = "count")
