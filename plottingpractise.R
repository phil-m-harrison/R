data(uspop)
uspop
popvstime<-as.data.frame(uspop)
popvstime$Year<-c(seq(from=1790, to=1970, by=10))
ggplot(popvstime, aes(x=Year, y=x)) + 
  geom_point()+
  geom_smooth(method = "loess",colour = "blue", size = 1) +
  xlab("Year")+
  ylab("Population (millions)")+
  ggtitle("Population growth of USA from 1790 to 1970")+
  scale_x_continuous(breaks=c(seq(from=1790, to=1970, by=20)))


data(AirPassengers)
AirPassengers
airpassengersDF<-as.data.frame(AirPassengers)
airpassengersDF$time<-c(seq(from=1949,to= 1960.92, by=1/12))
ggplot(airpassengersDF, aes(x=time, y=x))+
  geom_point()+
  geom_smooth(method="loess",colour="red",size=1)+
  xlab("Year")+
  ylab("Air Passengers")+
  ggtitle("Air passengers from 1949 to 1960")+
scale_x_continuous(breaks=c(seq(from=1949, to=1961, by = 2)))  
  
  quakes
quakeslocations<-as.data.frame(quakes$lat)
quakeslocations$long<-quakes$long
colnames(quakeslocations)[1]<-"lat"

ggplot(quakes, aes(x=long,y=lat,colour=depth))+geom_point()
