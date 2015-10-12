library(ggplot2)
library(plyr)
library(dplyr)
require(dplyr)
library(nycflights13)
data(airports)
data(flights)

visualize_airport_delays<- function(){
  
  combDest<-airports %>% mutate(dest = faa) %>% right_join(flights) %>% arrange(lon, lat) %>% group_by(name) %>% mutate(avg=mean(arr_delay, na.rm=TRUE)) 
  combOrig<-airports %>% mutate(origin = faa) %>% right_join(flights) %>% arrange(lon, lat) %>% group_by(name)  %>% mutate(avg=mean(dep_delay, na.rm=TRUE)) 
  
  delayDest<-select(combDest, lon, lat, name, avg)
  delayOrig<-select(combOrig, lon, lat, name, avg)
  
totDest<-distinct(delayDest)
totOrig<-distinct(delayOrig)
  
par(mfrow=c(2,1))
  p1<-qplot(y=avg, data=totDest,col=-avg, size=2, xlab="Airport", ylab="Mean delay (minutes)", main="Mean arrival delay by airport")
  p2<-qplot(y=avg, data=totOrig,col=-avg, size=2, xlab="Airport", ylab="Mean delay (minutes)", main="Mean departure delay by airport")
return(list(p1,p2))

}
  


