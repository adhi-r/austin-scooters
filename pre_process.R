# pre-processing the Austin dockless data

library(tidyverse)
library(RSocrata)
library(lubridate)
library(ggthemes)

austin_dockless_raw <- "https://data.austintexas.gov/resource/7d8e-dm7r.csv" %>% 
  RSocrata::read.socrata() %>% 
  as_tibble()
  
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
