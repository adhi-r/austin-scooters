# scooter location/hexagon mapping
library(tidyverse)
library(lubridate)
library(sp)
# read in dockless data
austin_dockless <- read_csv("adhi/austin_dockless.csv")

# download hexagon id dataframe
austin_hexagon_grid <- RSocrata::read.socrata("https://data.austintexas.gov/resource/fpn4-eje4.csv")

# pdf example
Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
Srs1 = Polygons(list(Sr1), "s1")

Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
plot(SpP, col = 1:3, pbg="white")


attr = data.frame(a=1:3, b=3:1, row.names=c("s3/4", "s2", "s1"))
SrDf = SpatialPolygonsDataFrame(SpP, attr)
as(SrDf, "data.frame")
spplot(SrDf)

# 
# attempt to plot geographical distribution of rides

hexagon_ride_counts <- austin_dockless %>% 
  filter(month == 2 & year == 2019, # restrict to feburary 19
         orig_cell_id != "") %>%  #filtering out blank hexagon id's
  mutate(orig_cell_id = as.integer(orig_cell_id)) %>% 
  group_by(orig_cell_id) %>% 
  count() %>% 
  arrange(desc(n))

hexagon_ride_counts %>%
  left_join(austin_hexagon_grid, by = c("orig_cell_id" = "id")) %>% 
  write_csv("austin_hexagon_SO.csv")
