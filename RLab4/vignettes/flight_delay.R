## ------------------------------------------------------------------------
library(nycflights13)
data(flights)
data(weather)
require(dplyr)
library(caret)
library(RLab4)

## ------------------------------------------------------------------------
flights<- flights %>% select(year, month, day, hour, origin, dep_delay, arr_delay, distance) %>% na.omit()
weather<-weather %>% select(-dewp, -humid, -pressure) %>% na.omit()
wfComb<-flights %>% inner_join(weather, by=c("origin", "year", "month", "day", "hour")) %>% mutate("prec_wind"=precip*wind_speed, "dir_wind"=wind_dir*wind_speed) 


## ------------------------------------------------------------------------
trainTestIndex <- createDataPartition(wfComb$dep_delay, p = .85, list = FALSE,times = 1)

Valid  <- wfComb[-trainTestIndex,]
Left <- wfComb[trainTestIndex,]

trainIndex <- createDataPartition(Left$dep_delay, p = 80/85, list = FALSE,times = 1)

Test <- Left[-trainIndex,]
Train <- Left[ trainIndex,] 

## ------------------------------------------------------------------------

model7 <- ridgereg(dep_delay ~ distance + temp + wind_dir + wind_speed
                   + precip + visib + prec_wind + dir_wind,
                   data=Train,0.0001)

## ------------------------------------------------------------------------
v3<-model7$predict(Valid)
v3Error<-v3-Valid$dep_delay
RMSE3<-sqrt(mean(v3Error^2))

RMSE3

## ------------------------------------------------------------------------
vTest<-model7$predict(Test)
vTestError<-vTest-Test$dep_delay
RMSETest<-sqrt(mean(vTestError^2))

RMSETest

