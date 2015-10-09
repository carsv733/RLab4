install.packages("nycflights13")
library(nycflights13)
data(airports)
data(flights)
data(weather)

require(dplyr)
install.packages("caret")
library(caret)
install.packages("mlbench")
library(mlbench)

flights<- flights %>% select(year, month, day, hour, origin, dep_delay, arr_delay, distance) %>% na.omit()
weather<-weather %>% select(-dewp, -humid, -pressure) %>% na.omit()
wfComb<-flights %>% inner_join(weather, by=c("origin", "year", "month", "day", "hour")) %>% mutate("prec_wind"=precip*wind_speed, "dir_wind"=wind_dir*wind_speed) 

trainTestIndex <- createDataPartition(wfComb$dep_delay, p = .85, list = FALSE,times = 1)

Valid  <- wfComb[-trainTestIndex,]
Left <- wfComb[trainTestIndex,]

trainIndex <- createDataPartition(Left$dep_delay, p = 80/85, list = FALSE,times = 1)

Test <- Left[-trainIndex,]
Train <- Left[ trainIndex,]

model5 <- ridgereg(dep_delay ~ distance + temp + wind_dir + wind_speed
                    + precip + visib + prec_wind + dir_wind,
                   data=Train[1:10000,],0.001)

fitControl <- trainControl(method = "repeatedcv",
                        ## 10-fold CV...
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

model4 <- train(dep_delay ~ distance + temp + wind_dir + wind_speed
                + wind_gust + precip + visib + prec_wind + dir_wind,
                data=Train[1:40000,],
                method = modelInfo,
                trControl = fitControl)

#loop för validering
Validation<-select(Validation, distance, temp, wind_dir, wind_speed, wind_gust, precip, visib, prec_wind, dir_wind)
y<-numeric()

X<-model.matrix(dep_delay ~ distance + temp + wind_dir + wind_speed
                + wind_gust + precip + visib + prec_wind + dir_wind,
                data=Validation[1:10000,])

norm <- function(X){
  for (i in 2:dim(X)[2]){
    mean <- mean(X[,i])
    sd <- sd(X[,i])
    for (j in 1:dim(X)[1]) {
      X[j,i] <- (X[j,i]-mean)/sd 
    }
  }
  return(X)
}

Validation<-norm(X)
    for (i in 1:dim(Validation)[1]) {
      y[i] <- (model5$coef()[1]+model5$coef()[2]*Validation[[i,1]]+model5$coef()[3]*Validation[[i,2]]+model5$coef()[4]*Validation[[i,3]]+model5$coef()[5]*Validation[[i,4]]+ model5$coef()[6]*Validation[[i,5]]+model5$coef()[7]*Validation[[i,6]]+model5$coef()[8]*Validation[[i,7]]+model5$coef()[9]*Validation[[i,8]])
    }

plot(Valid$dep_delay, type="l", col="purple", xlim=c(0,10000))
points(y, col="red", type="l")
