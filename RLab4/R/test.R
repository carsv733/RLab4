data(weather)
data(flights)
require(dplyr)
install.packages("caret")
library(caret)
install.packages("mlbench")
library(mlbench)
data(BostonHousing)
data(oil)

flights<-select(flights, year, month, day, hour, origin, dep_delay, arr_delay, distance)
weather<-select(weather, -dewp, -humid, -pressure)
wfComb<-flights %>% inner_join(weather, by=c("origin", "year", "month", "day", "hour")) %>% mutate("prec*wind"=precip*wind_speed, "dir*wind"=wind_dir*wind_speed) 


