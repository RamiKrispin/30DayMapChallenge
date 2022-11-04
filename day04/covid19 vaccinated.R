library(rnaturalearth)
library(sf)
library(mapview)
library(coronavirus)
library(dplyr)
sf_use_s2(FALSE)

data("covid19_vaccine")

map <- ne_countries(returnclass = "sf") %>%
  dplyr::select(name, iso2 = iso_a2, iso3 = iso_a3, geometry)

head(map)

df <- map %>% left_join(
  covid19_vaccine %>%
    filter(date == max(date),
           is.na(province_state)) %>%
    mutate(perc = round(100 * people_fully_vaccinated / population, 2)) %>%
    select(country_region, iso2, iso3, people_fully_vaccinated, perc, continent_name),
  by = c("iso2", "iso3")
)

df  %>%
  mapview::mapview(zcol = "perc",
                   at = seq(0, max(df$perc, na.rm = TRUE), 10),
                   legend = TRUE,
                   layer.name = "Fully Vaccinated %")
