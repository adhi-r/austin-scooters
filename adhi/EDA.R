# austin scooter data pull
library(tidyverse)
library(RSocrata)
library(lubridate)
library(ggthemes)
austin_scooters <- "https://data.austintexas.gov/resource/7d8e-dm7r.csv" %>% 
  RSocrata::read.socrata() %>% 
  as_tibble()

## scooter utility
# what is the typical usage of a single scooter - battery
# what is the typical lifetime of a scooter


## time series questions
# what does hour really mean
austin_scooters %>%
  select(hour, start_time, end_time) %>% 
  mutate(observed_start_hour = hour(start_time),
         observed_end_hour = hour(end_time))

# how many rides were over an hour long
austin_scooters %>% 
  select(start_time, end_time) %>% 
  mutate(duration = end_time - start_time,
         duration = as.numeric(duration)) %>%
  mutate(duration_minutes = as.numeric(duration/60)) %>%
  filter(duration_minutes > 30) %>% 
  count()

# when was the epoch ride
austin_scooters %>% 
  arrange(start_time)

# seasonality of day
austin_scooters %>% 
  group_by(floor_hour = floor_date(start_time, "hour")) %>% 
  count() %>% 
  group_by(hour_of_day = hour(floor_hour)) %>% 
  summarise(mean_rides = mean(n)) %>% 
  ggplot(aes(hour_of_day, mean_rides)) +
  geom_histogram(stat = "identity")

# seasonality of week
austin_scooters %>% 
  filter(start_time > ymd(20180901)) %>% # to start from a comparable period of time without a bunch of 0's
  group_by(floor_day = floor_date(start_time, "day")) %>% 
  count() %>% 
  group_by(day_of_week = wday(floor_day)) %>% 
  summarise(mean_rides = mean(n)) %>% 
  ggplot(aes(day_of_week, mean_rides)) +
  geom_histogram(stat = "identity")



# rides/day increasing over time?
austin_scooters %>% 
  filter(start_time > ymd(20181001)) %>% 
  ggplot(aes(start_time)) +
  geom_freqpoly(binwidth = 86400) + # 86400 seconds = 1 day 
  ggtitle("The beginning of SXSW was peak scootpocalypse in Austin",
          "Scooter rides per day, data.austin.gov")
 
# is that huge spike due to SXSW?
austin_scooters %>% 
  group_by(month = month(start_time, label = TRUE), day = day(start_time)) %>% 
  count() %>% 
  arrange(desc(n))


## location based questions
# load in hexagon/location polygon crosswalk
austin_hexagon_grid <- RSocrata::read.socrata("https://data.austintexas.gov/resource/fpn4-eje4.csv")

# advanced
# 
# weather
# 
# traffic
