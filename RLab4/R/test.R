data(weather)
data(flights)
require(dplyr)

flights<-select(flights, year, month, day, hour, origin, dep_delay, arr_delay, distance)
weather<-select(weather, -dewp, -humid, -precip, -pressure)
wfComb<-flights %>% inner_join(weather, by=c("origin", "year", "month", "day", "hour")) 
