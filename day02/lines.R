# Day 2 - lines
# Airports codes https://datahub.io/core/airport-codes#data
# Flights data: https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FGJ
`%>%` <- magrittr::`%>%`

# Airline mapping
# Source: https://www.transtats.bts.gov/Oneway.asp?Svryq_Qr5p=h0v37r%FDPn44vr4%FDP1qr.%FDjur0%FD6ur%FD5nzr%FDp1qr%FDun5%FDorr0%FD75rq%FDoB%FDz7y6v2yr%FDpn44vr45%FP%FDn%FD07zr4vp%FD57ssvA%FDv5%FD75rq%FDs14%FDrn4yvr4%FD75r45%FP%FDs14%FDrAnz2yr%FP%FDcN%FP%FDcN%FLE%FM%FP%FDcN%FLF%FM.%FDh5r%FD6uv5%FDsvryq%FDs14%FDn0nyB5v5%FDnp4155%FDn%FD4n0tr%FD1s%FDBrn45.&Svryq_gB2r=Pun4&fry_Pn6=bc_haVdhR_PNeeVRe&Y11x72_gnoyr=Y_haVdhR_PNeeVRef&fry_in4=cPg_bagVZR_Nee&fry_f6n6=a/N&Qn6n_gB2r=PNg&cr4pr06_Synt=D&Qv52ynB_Synt=D

airlines_map <- data.frame(code = c("9E", "AA", "AS", "B6", "DL",
                                    "F9", "G4", "HA", "MQ", "NK",
                                    "OH", "OO", "QX", "UA", "WN",
                                    "YV", "YX"),
                           name = c("Endeavor Air Inc.",
                                    "American Airlines Inc.",
                                    "Alaska Airlines Inc.",
                                    "JetBlue Airways",
                                    "Delta Air Lines Inc.",
                                    "Frontier Airlines Inc.",
                                    "Allegiant Air",
                                    "Hawaiian Airlines Inc.",
                                    "Envoy Air",
                                    "Spirit Air Lines",
                                    "PSA Airlines Inc.",
                                    "SkyWest Airlines Inc.",
                                    "Horizon Air",
                                    "United Air Lines Inc.",
                                    "Southwest Airlines Co.",
                                    "Mesa Airlines Inc.",
                                    "Republic Airline"))



flights_df <- readr::read_csv("day02/On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2022_8.csv") %>%
  as.data.frame() %>%
  dplyr::select(date = FlightDate,
                airline = Reporting_Airline,
                origin_code = Origin,
                origin_city = OriginCityName,
                origin_state_code = OriginState,
                origin_state_name = OriginStateName,
                dest_code = Dest,
                dest_city = DestCityName,
                dest_state_code = DestState,
                dest_state_name = DestStateName,
                dep_delay = DepDelay,
                arr_delay = ArrDelay) %>%
  dplyr::group_by(airline, origin_code, origin_city,
                  origin_state_code, origin_state_name, dest_code,
                  dest_city, dest_state_code, dest_state_name) %>%
  dplyr::summarise(total = dplyr::n(),
                   dep_delay = mean(dep_delay, na.rm = TRUE),
                   arr_delay = mean(arr_delay, na.rm = TRUE),
                   .groups = "drop") %>%
  as.data.frame() %>%
  dplyr::arrange(-total)


head(flights_df)

airport_codes <- readr::read_csv("day02/airport-codes_csv.csv") %>%
  dplyr::filter(!is.na(iata_code)) %>%
  as.data.frame()

head(airport_codes)

coordinates_df <- do.call(rbind, strsplit(airport_codes$coordinates, split = ","))
airport_codes$long <- as.numeric(trimws(coordinates_df[, 2]))
airport_codes$lat <-  as.numeric(trimws(coordinates_df[, 1]))

head(airport_codes)

df <- flights_df %>%
  dplyr::filter(origin_state_name != "U.S. Pacific Trust Territories and Possessions",
                dest_state_name != "U.S. Pacific Trust Territories and Possessions") %>%
  dplyr::left_join(airport_codes %>%
                     dplyr::select(iata = iata_code,
                                   long_start = long,
                                   lat_start = lat),
                   by = c("origin_code" = "iata")) %>%
  dplyr::left_join(airport_codes %>%
                     dplyr::select(iata = iata_code,
                                   long_end = long,
                                   lat_end = lat),
                   by = c("dest_code" = "iata")) %>%
  dplyr::left_join(airlines_map %>%
                     dplyr::select(airline = code, airline_name = name),
                   by = "airline")
head(df)
table(is.na(df$lat_start))
table(is.na(df$lat_end))

# Aggregate by airport
airport_df <- df %>%
  dplyr::filter(airline == "UA") %>%
  dplyr::group_by(origin_code, origin_city,
                  origin_state_code, origin_state_name) %>%
  dplyr::summarise(total_flights = sum(total),
                   avg_delay = mean(dep_delay, na.rm = TRUE),
                   lat = unique(lat_start),
                   long = unique(long_start),
                   .groups = "drop") %>%
  dplyr::arrange(-total_flights) %>%
  as.data.frame()
head(airport_df)

geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  countrywidth = 0.5,
  landcolor = "white",
  countrycolor = "black",
  lakecolor = "white",
  subunitcolor = "white",
  countrycolor = "white",
  bgcolor= "black",
  showlakes = TRUE,
  showocean = FALSE
)


# Set the scale legend
long <- 34
lat <- -168
scale_df <- data.frame(size = c(100, 500, 3000, 6000),
                       long = c(long - 1.5, long - 0.9, long - 0.3, long + 0.3),
                       lat = c(lat, lat + 3.5, lat + 7, lat + 10.5))

plot_geo(locationmode = 'USA-states', color = "#ffb703") %>%
  add_segments(
    data = df %>% dplyr::filter(airline == "UA"),
    y = ~long_start, yend = ~long_end,
    x = ~lat_start, xend = ~lat_end,
    line = list(color = "#ffd166"),
    alpha = 0.3, size = I(0.7),
    hoverinfo = "none"
  )  %>% add_markers(
    data = airport_df, y = ~long, x = ~lat, text = ~origin_code,
    size = ~total_flights, hoverinfo = "text", alpha = 0.9,
    marker = list(color = "#ffb703")
  ) %>% add_markers(
    data = scale_df, y = ~ long, x = ~ lat,
    size = ~size, hoverinfo = "text", alpha = 0.9,
    marker = list(color = "#ffb703")
  ) %>%
  add_annotations(
    text = "Number of Flights - Scale:",
    x = 0.15,
    y = 0.45,
    showarrow = FALSE,
    xref = "paper",
    yref = "paper",
    font = list(size = 10)
  ) %>%
  add_annotations(
    text = c("100  500  3000  6000"),
    x = 0.15,
    y = 0.39,
    showarrow = FALSE,
    xref = "paper",
    yref = "paper",
    font = list(size = 10)
  ) %>%
  add_annotations(
    text = "#30DayMapChallenge | Viz: Rami Krispin | Data: Bureau of Transportation Statistics/Datahub",
    x = 0.15,
    y = 0.02,
    showarrow = FALSE,
    xref = "paper",
    yref = "paper",
    font = list(size = 12)
  ) %>%
  layout(
    title = 'United Airlines Domestic Flight Paths During August 2022',
    font = list(size = 16, color = "white"),
    geo = geo,
    showlegend = FALSE,
    plot_bgcolor  = "black",
    paper_bgcolor = "black",
    margin = list(
      l = 0,
      r = 0,
      b = 0,
      t = 45
    )
  )

