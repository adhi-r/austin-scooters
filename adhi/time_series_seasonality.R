# time series and seasonality questions
library(tidyverse)
library(RSocrata)
library(lubridate)
library(ggthemes)

austin_dockless <- read_csv("adhi/austin_dockless.csv")

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
  filter(start_time > ymd(20180901)) %>% # to start from a comparable period of time without a bunch of 0's
  group_by(floor_hour = floor_date(start_time, "hour")) %>% 
  count() %>% 
  group_by(hour_of_day = hour(floor_hour)) %>% 
  summarise(mean_rides = mean(n)) %>% 
  ggplot(aes(hour_of_day, mean_rides)) +
  geom_histogram(stat = "identity") +
  labs(title = "distribution of rides throughout the day",
       subtitle = "mean of every hour of the day using data from 2018-09-01 on")

# seasonality of week
austin_dockless %>% 
  filter(start_time > ymd(20180901)) %>% # to start from a comparable period of time without a bunch of 0's
  group_by(floor_day = floor_date(start_time, "day")) %>% 
  count() %>% 
  group_by(day_of_week = wday(floor_day)) %>% 
  summarise(mean_rides = mean(n)) %>% 
  ggplot(aes(day_of_week, mean_rides)) +
  geom_histogram(stat = "identity") +
  labs(title = "distribution of rides throughout the week",
       subtitle = "mean of every day of the week using data from 2018-09-01 on")

# rides/day over time
austin_dockless %>% 
  #filter(start_time > ymd(20180801)) %>% 
  filter(vehicle_type == "scooter") %>% 
  ggplot(aes(start_time)) +
  geom_histogram(binwidth = 86400) + # 86400 seconds = 1 day 
  labs(title = "SXSW was peak scoot-pocalypse in Austin",
       subtitle = "Scooter rides per day, source: data.austin.gov",
       x = "",
       y = "") +
  scale_x_datetime(date_breaks = "month", 
                   date_labels = "%b") +
  theme_minimal()
ggsave("adhi/sxsw_scootpocalypse.png", height = 4, width = 7, units = c("in"))

# minutes ridden per day 
# might be a more compelling metric since sxsw riders 
# might be doing a lot of really short rides

austin_dockless %>% 
  group_by(date = floor_date(start_time, "day")) %>%
  summarise(total_ride_time = sum(trip_duration)) %>% 
  mutate(total_ride_hours = total_ride_time/60/60) %>% 
  #filter(date > ymd(20180801)) %>% 
  ggplot(aes(date, total_ride_hours)) + 
  geom_histogram(stat = "identity") +
  labs(title = "Total scooter ride hours per day",
       x = "",
       y = "")


# is that huge spike due to SXSW?
austin_dockless %>% 
  group_by(month = month(start_time, label = TRUE),
           day = day(start_time)) %>% 
  count() %>% 
  arrange(desc(n))
