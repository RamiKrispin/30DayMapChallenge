# Minimal - Califorrnia map

library(dplyr)
library(rnaturalearth)



states <- ne_states( returnclass = 'sf') %>%
  filter(iso_a2 == "US", type == "State", name == "California") %>%
  select(name)

plot(states, main = "California")

