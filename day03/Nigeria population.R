# Day 3 - polygons
# Plotting Nigerian States population
# Data source wikipedia: https://en.wikipedia.org/wiki/List_of_Nigerian_states_by_population

library(rvest)
library(dplyr)
library(rnaturalearth)
library(mapview)
library(tmap)
# Get Nigerian states polygons
nigeria <- ne_states(country = "Nigeria", returnclass = "sf")
head(nigeria)

# Pulling the popalation data froom wiki
url <- "https://en.wikipedia.org/wiki/List_of_Nigerian_states_by_population"
page <- read_html(url)
tables <- html_node(page, ".wikitable")
pop_table <- html_table(tables, fill = TRUE) %>%
  select(state_temp = State, pop_2006_temp = `Population (2006)`, pop_2019_temp = `Population (2019)`)
pop_table$pop_2006 <- as.numeric(gsub(x = pop_table$pop_2006_temp,pattern = ",", replacement = ""))
pop_table$pop_2019 <- as.numeric(gsub(x = pop_table$pop_2019_temp,pattern = ",", replacement = ""))
pop_table$state <- gsub(x = pop_table$state_temp,pattern = " State", replacement = "")
pop_table <- pop_table %>% select(-state_temp, -pop_2006_temp, - pop_2019_temp) %>%
  select(state, pop_2006, pop_2019) %>%
  mutate(state_fix = state)

head(pop_table)

# Fix labels
pop_table$state_fix[which(pop_table$state_fix == "Nasarawa")] <- "Nassarawa"
nigeria_pop <- nigeria %>% left_join(pop_table, by = c("name" = "state_fix"))

plot(nigeria_pop["pop_2019"], key.pos = 1,
     axes = TRUE, main = "Nigeria Population by State",
     key.width = lcm(1.3), key.length = 1.0)



mapview(nigeria_pop,
        zcol = "pop_2019",
        legend = TRUE,
        layer.name = "Population")

sf_use_s2(FALSE)
tmap_mode("plot")
tm_shape(nigeria_pop) +
  tm_polygons(col = "pop_2019",
              style = "pretty",
              title = "Population",
              # breaks = c(0, 2*10^6, 4*10^6, 6*10^6, 8*10^6, 15*10^6),
              palette = "Blues") +
  tm_style("cobalt") +
  tm_text("state", size = 0.8) +
  tm_credits("Source: Wikipedia - List of Nigerian states by population",
             position = c("LEFT", "BOTTOM")) +
  tm_layout(title= "Nigeria Population by States",
            title.position = c('right', 'top') ,
            inner.margins = c(0.1, .01, .1, .05))


