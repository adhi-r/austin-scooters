# austin scooter data pull
library(tidyverse)
library(RSocrata)
library(lubridate)
library(ggthemes)

austin_dockless <- "https://data.austintexas.gov/resource/7d8e-dm7r.csv" %>% 
  RSocrata::read.socrata() %>% 
  as_tibble()

## checking for weirdness ----
# trips with start or end times from before 2018
austin_dockless %>% 
  filter(start_time < ymd(20180101) | end_time < ymd(20180101)) %>% 
  select(start_time, end_time) # 341 trips

# trips where the end is before the start
austin_dockless %>% 
  filter(end_time < start_time) # 375 trips

# trips where the distance travelled is negative
austin_dockless %>% 
  filter(trip_distance < 0) %>% 
  select(trip_distance) # 562 trips

# trips where the distance is unreasonably high
austin_dockless %>% 
  filter(trip_distance > 16e4) %>% # 160k meters is approx 100 miles.
  select(start_time, end_time,start_longitude, start_latitude,
         end_longitude, end_latitude, trip_distance) # 478 trips

# trips where the start or end is unreasonably far from the medians
austin_dockless %>% 
  filter(start_latitude - median(start_latitude, na.rm = TRUE) > .5 |
         start_longitude - median(start_longitude, na.rm = TRUE) > .5 |
         end_latitude - median(end_latitude, na.rm = TRUE) > .5 |
         end_longitude - median(end_longitude, na.rm = TRUE) > .5) %>%  
  select(start_time, end_time, start_longitude, start_latitude,
         end_longitude, end_latitude, trip_distance) # 8.2k trips

# trips where the hexagon id was out of bounds
austin_dockless %>% 
  filter(orig_cell_id == "OUT_OF_BOUNDS" |
         dest_cell_id == "OUT_OF_BOUNDS") # 51k trips

## scooter utility ----
# what is the typical usage of a single scooter - battery


# what is the typical lifetime of a scooter


## time series questions ----
# what does hour really mean
austin_dockless %>%
  select(hour, start_time, end_time) %>% 
  mutate(observed_start_hour = hour(start_time),
         observed_end_hour = hour(end_time))

# how many rides were over an hour long
austin_dockless %>% 
  filter(trip_duration > 3600) %>% # 3600 seconds = 1 hour
  count()

# when was the epoch ride
austin_dockless %>% 
  arrange(start_time)

# seasonality of day
austin_dockless %>% 
  group_by(floor_hour = floor_date(start_time, "hour")) %>% 
  count() %>% 
  group_by(hour_of_day = hour(floor_hour)) %>% 
  summarise(mean_rides = mean(n)) %>% 
  ggplot(aes(hour_of_day, mean_rides)) +
  geom_histogram(stat = "identity")

# seasonality of week
austin_dockless %>% 
  filter(start_time > ymd(20180901)) %>% # to start from a comparable period of time without a bunch of 0's
  group_by(floor_day = floor_date(start_time, "day")) %>% 
  count() %>% 
  group_by(day_of_week = wday(floor_day)) %>% 
  summarise(mean_rides = mean(n)) %>% 
  ggplot(aes(day_of_week, mean_rides)) +
  geom_histogram(stat = "identity")

# rides/day over time
austin_dockless %>% 
  filter(start_time > ymd(20180801)) %>% 
  filter(vehicle_type == "scooter") %>% 
  ggplot(aes(start_time)) +
  geom_histogram(binwidth = 86400) + # 86400 seconds = 1 day 
  labs(title = "The beginning of SXSW was peak scoot-pocalypse in Austin",
       subtitle = "Scooter rides per day, source: data.austin.gov",
       x = "",
       y = "") +
  scale_x_datetime(date_breaks = "month", 
                   date_labels = "%b") +
  theme_minimal()
ggsave("adhi/sxsw_scootpocalypse.png", height = 4, width = 7, units = c("in"))

# minutes ridden per day 
# might be a more compelling metric since sxsw riders might be doing a lot of really short rides

austin_dockless %>% 
  group_by(date = floor_date(start_time, "day")) %>%
  summarise(total_ride_time = sum(trip_duration)) %>% 
  mutate(total_ride_hours = total_ride_time/60/60) %>% 
  filter(date > ymd(20180801)) %>% 
  ggplot(aes(date, total_ride_hours)) + 
  geom_histogram(stat = "identity", binwidth=1) +
  labs(title = "Total scooter ride hours per day",
       x = "",
       y = "")
  

# is that huge spike due to SXSW?
austin_dockless %>% 
  group_by(month = month(start_time, label = TRUE),
           day = day(start_time)) %>% 
  count() %>% 
  arrange(desc(n))


## location based questions
# load in hexagon/location polygon crosswalk
austin_hexagon_grid <- RSocrata::read.socrata("https://data.austintexas.gov/resource/fpn4-eje4.csv")

## advanced stuff
# 
# weather
# 
# traffic

# cost of every ride
austin_dockless %>%
  filter(end_time > start_time) %>% 
  filter(!end_time > Sys.time()) %>% 
  mutate(trip_cost = 1 + 0.15 * (trip_duration / 60)) %>% 
  arrange(desc(trip_cost)) %>% 
  select(trip_cost, start_time, end_time, trip_distance)
  