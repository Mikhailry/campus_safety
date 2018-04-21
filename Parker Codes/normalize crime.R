load("~/Documents/Math 571 project/Data/iitAreaWithWeather.Rda")
load("~/Documents/Math 571 project/Data/iitCrimeWithWeather.Rda")
load("~/Documents/Math 571 project/Data/ucAreaWithWeather.Rda")
load("~/Documents/Math 571 project/Data/ucCrimeWithWeather.Rda")

HydePark_pop<-26893
HydePark_mile<-1.649
WashingtonPark_pop<-12081
WashingtonPark_mile<-1.479
Woodlawn_pop<-24150
Woodlawn_mile<-2.07
Kenwood_pop<-17601
Kenwood_mile<-1.089
Bridgeport_pop<-33878
Bridgeport_mile<-2.1
Douglas_pop<-20323
Douglas_mile<-1.672

#Get population per 2 miles by summing area populations and dividing by total miles
IITAreaPop1<-2*(Bridgeport_pop+Douglas_pop)/(Bridgeport_mile+Douglas_mile)
UCAreaPop1<-2*(HydePark_pop+WashingtonPark_pop+Woodlawn_pop+Kenwood_pop)/(HydePark_mile+WashingtonPark_mile+Kenwood_mile+Woodlawn_mile)

#Get population by the mile counts individually
IITAreaPop2<-(Bridgeport_pop/Bridgeport_mile)+(Douglas_pop/Douglas_mile)
UCAreaPop2<-2*((HydePark_pop/HydePark_mile)+(WashingtonPark_pop/WashingtonPark_mile)+(Woodlawn_pop/Woodlawn_mile)+(Kenwood_pop/Kenwood_mile))/4
  
#Crime percentage
  
  