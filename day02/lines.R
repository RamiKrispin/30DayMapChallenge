# Airports codes https://datahub.io/core/airport-codes#data

df <- readr::read_csv("day02/On_Time_Reporting_Carrier_On_Time_Performance_(1987_present)_2022_8.csv") %>%
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
                dest_state_name = DestStateName)


head(df)
airport_codes <- readr::read_csv("day02/airport-codes_csv.csv") %>%
  dplyr::filter(!is.na(iata_code))

head(airport_codes)

coordinates_df <- do.call(rbind, strsplit(airport_codes$coordinates, split = ","))
airport_codes$long <- as.numeric(trimws(coordinates_df[, 2]))
airport_codes$lat <-  as.numeric(trimws(coordinates_df[, 1]))
