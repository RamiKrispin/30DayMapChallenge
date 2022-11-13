# Plotting US GDP per capita by state
# Data:
# GDP by state - Bureau of Economic Analysis
# https://www.bea.gov/data/gdp/gdp-state
# US population
# United States Census Bureau - https://data.census.gov/cedsci/
# Plotly map example: https://ramikrispin.github.io/covid19sf/articles/geo.html
library(dplyr)
library(rnaturalearth)
library(plotly)
# Load the data ----
states <- ne_states( returnclass = 'sf') %>%
  filter(iso_a2 == "US", type == "State") %>%
  select(name, latitude, longitude)

states$geometry <- NULL
head(states)
# GDP data
gdp21 <- read.csv("./day12/gdp21.csv")
head(gdp21)
gdp21$state <- trimws(gdp21$state )
# Population data
pop_df <- read.csv("./day12/NST-EST2021-alldata.csv") %>%
  setNames(tolower(names(.))) %>%
  select(state, name, pop21 = popestimate2021)
head(pop_df)
pop_df$name <- trimws(pop_df$name)
df <- pop_df %>% filter(state != 0) %>%
  left_join(gdp21, by = c("name" = "state")) %>%
  left_join(states, by = "name") %>%
  mutate(gdp_per_capita = gdp * 10 ^ 6 / pop21)

head(df)

df2 <- df %>%
  filter(!is.na(latitude)) %>%
  arrange(-gdp_per_capita) %>%
  # head(20) %>%
  mutate(states_ordered = factor(name, levels = name))

df2
# Plotting the data

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)


p1 <- plot_geo(df, locationmode = 'USA-states', sizes = c(1, 200)) %>%
  add_markers(
    x = ~longitude,
    y = ~latitude,
    size = ~gdp / pop21,
    marker = list(color = "#457b9d"),
    hoverinfo = "text",
    text = ~paste("State: ", name, "<br>",
                  "Population: ", pop21, "<br>",
                  "GDP: ", "$",gdp, " M", "<br>",
                  "GDP per Capita: ", "$", round(gdp_per_capita), "<br>",
                  sep = "")
  ) %>%
  layout(title = "US GDP per Capita by State",
         margin = list(
           l = 50,
           r = 50,
           b = 50,
           t = 100
         ),
         geo = g)

p1

p2 <- plot_ly(data = df2,
              x = ~ states_ordered,
              y = ~ gdp_per_capita,
              marker = list(color = "#457b9d"),
              type = "bar",
              showlegend = FALSE) %>%
  layout(yaxis = list(title = "GDP per Capita ($)"),
         xaxis = list(title = ""))

p2

subplot(p1, p2,
        titleY = TRUE,
        heights = c(0.7, 0.3),
        nrows = 2) %>%
  layout(title = "US GDP per Capita by State",
         margin = list(
           l = 10,
           r = 10,
           b = 10,
           t = 35))
