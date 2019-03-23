# pre-processing the Austin dockless data

library(tidyverse)
library(RSocrata)
library(lubridate)
library(ggthemes)

austin_dockless_raw <- "https://data.austintexas.gov/resource/7d8e-dm7r.csv" %>% 
  RSocrata::read.socrata() %>% 
  as_tibble()

## checking for weirdness (comment counts as of March 15th 2019 or so) ----
# trips with start or end times from before 2018
austin_dockless_raw %>% 
  filter(start_time < ymd(20180101) | end_time < ymd(20180101)) %>% 
  select(start_time, end_time) # 341 trips

# trips where the end is before the start
austin_dockless_raw %>% 
  filter(end_time < start_time) # 375 trips

# trips where the distance travelled is negative
austin_dockless_raw %>% 
  filter(trip_distance < 0) %>% 
  select(trip_distance) # 562 trips

# trips where the distance is unreasonably high
austin_dockless_raw %>% 
  filter(trip_distance > 16e4) %>% # 160k meters is approx 100 miles.
  select(start_time, end_time,start_longitude, start_latitude,
         end_longitude, end_latitude, trip_distance) # 478 trips

# trips where the start or end is unreasonably far from the medians
austin_dockless_raw %>% 
  filter(start_latitude - median(start_latitude, na.rm = TRUE) > .5 |
           start_longitude - median(start_longitude, na.rm = TRUE) > .5 |
           end_latitude - median(end_latitude, na.rm = TRUE) > .5 |
           end_longitude - median(end_longitude, na.rm = TRUE) > .5) %>%  
  select(start_time, end_time, start_longitude, start_latitude,
         end_longitude, end_latitude, trip_distance) # 8.2k trips

# trips where the hexagon id was out of bounds
austin_dockless_raw %>% 
  filter(orig_cell_id == "OUT_OF_BOUNDS" |
           dest_cell_id == "OUT_OF_BOUNDS") # 51k trips

## cleaning the data and writing it out ----
  
austin_dockless <- austin_dockless_raw %>% 
  # removing trips with start or end times from before 2018
  filter(start_time > ymd(20180101) | end_time > ymd(20180101)) %>% 
  # removing trips where the end is before the start
  filter(end_time >= start_time) %>% 
  # removing trips where the distance travelled is negative or is unreasonably high
  filter(trip_distance > 0 |
         trip_distance < 16e4) %>% # 160k meters is approx 100 miles.
  # removing trips with missing hexagonal data
  filter(orig_cell_id != "OUT_OF_BOUNDS",  
         dest_cell_id != "OUT_OF_BOUNDS")

# adding features
austin_dockless <- austin_dockless %>% 
  # pricing model used by vast majority of dockless vechicles
  mutate(trip_cost = 1 + 0.15 * (trip_duration / 60)) %>%  
  # a grouping variable for whether the ride was during SXSW or not
  mutate(sxsw = if_else(start_time > ymd(20180307) & start_time < ymd(20180318),
                        TRUE, FALSE)) %>% 
  rename(trip_distance_meters = trip_distance) %>% 
  mutate(trip_distance_miles = trip_distance_meters / 1609.344) # converting meters to miles

write_csv(austin_dockless, "adhi/austin_dockless.csv")
