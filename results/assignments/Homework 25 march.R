library(data.table)
library(ggplot2)

#1
runoff_stations <- fread('./data/raw/runoff_stations.csv')
runoff_stations[, sname := factor(abbreviate(station))]
runoff_stations[, altitude := round(altitude, 0)]

runoff_station_area_altitude <- runoff_stations[, .(sname, area, altitude)]

#2
ggplot(runoff_station_area_altitude) +
    geom_point(aes(x = area, y = altitude))

#3
runoff_stations2 <- readRDS('data/runoff_stations.rds')

ggplot(data = runoff_stations2, aes(x = area, y = altitude, col = size)) +
  geom_point() +
  geom_text(label = runoff_stations2$sname) +
  theme_bw()

ggplot(data = runoff_stations2, aes(x = lon, y = lat, col = altitude)) +
  geom_point() +
  geom_text(label = runoff_stations2$sname) +
  scale_color_gradient(low = 'dark green', high = 'brown') +
  theme_bw()

#4
runoff_station_years <- runoff_stations2[, .(sname, start, end)]
ggplot(data = runoff_station_years, aes(x = start, y = sname)) +
  geom_segment(aes(xend = end, yend = sname))