setwd("Daniel")

library(ggplot2)
library(tidyverse)
library(RSocrata)
library(lubridate)
library(ggthemes)

austin_dockless <- read_csv(file = "austin_dockless.csv")

device_time <- austin_dockless_raw %>% 
 select(device_id, start_time, end_time) 

time <- austin_dockless_raw %>%
  select(device_id, start_time, end_time) 
  group_by(device_id) 
  
duration <- austin_dockless %>%
  group_by(device_id) %>%
  summarize(start_min=min(start_time), start_max=max(end_time)) %>% 
  mutate(start_date = as.Date(start_min, format ="%Y/%m/%d"), 
         end_date = as.Date(start_max, format="%Y/%m/%d"),
         date_diff = as.integer(end_date - start_date), 
         dslr = as.integer(Sys.Date() - end_date))

ggplot(duration, aes(x=dslr)) + 
  geom_histogram(binwidth = 10) 

duration %>%
  filter(start_date > ymd(20190101)) %>%
  group_by(start_date) %>% 
  count() %>% 
  arrange(n) %>% 
  ggplot(aes(x=start_date, y=n)) + 
  geom_histogram(stat = 'identity') + 
  scale_x_date(date_labels = "%b %d", date_breaks='7 days')


duration_summary <- summary(duration)


