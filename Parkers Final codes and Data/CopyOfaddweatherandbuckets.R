library(lubridate)
library(readr)
#read data
path<-"~/Documents/Math 571 project"
setwd(path)
IIT_FINAL_AGG <- read_csv("IIT_FINAL_AGG.csv")

#Change timezone and add so it is correct time
attr(IIT_FINAL_AGG$OCCURED,"tzone")<-"America/Chicago"
IIT_FINAL_AGG$OCCURED<-IIT_FINAL_AGG$OCCURED+hours(5)

#time buckets, day of week, and month
IIT_FINAL_AGG$MONTH<-NA
IIT_FINAL_AGG$DAY<-NA
IIT_FINAL_AGG$TIME_BUCKET<-NA

IIT_FINAL_AGG$MONTH<-as.numeric(strftime(IIT_FINAL_AGG$OCCURED, format= '%m', tz='America/Chicago'))
IIT_FINAL_AGG$DAY<-weekdays(IIT_FINAL_AGG$OCCURED)
#0:0-3   1:3-6    2:6-9   3:9-12   4:12-15    5:15-18   6:18-21   7:21-24
IIT_FINAL_AGG$TIME_BUCKET<-floor((as.numeric(strftime(IIT_FINAL_AGG$OCCURED, format= '%H', tz='America/Chicago')))/3)

#Add weather based on the times for IIT and UC
load("~/Documents/Math 571 project/Data/cleanweatherFull.Rda")
IIT_FINAL_AGG$COND<-NA
IIT_FINAL_AGG$STAND_COND<-NA
IIT_FINAL_AGG$SEVERITY<-NA
IIT_FINAL_AGG$TEMP<-NA
IIT_FINAL_AGG$HUM<-NA
IIT_FINAL_AGG$WIND<-NA
IIT_FINAL_AGG$PRECIP<-NA

for (i in 1:length(IIT_FINAL_AGG$OCCURED)){
  min_diff<-min(abs(IIT_FINAL_AGG$OCCURED[i]-weatherFull$date))
  row<-as.numeric(row.names(weatherFull[abs(IIT_FINAL_AGG$OCCURED[i]-weatherFull$date)==min_diff,]))
  IIT_FINAL_AGG$STAND_COND[i]<-weatherFull$condition[row][1]
  IIT_FINAL_AGG$SEVERITY[i]<-weatherFull$severity[row][1]
  IIT_FINAL_AGG$TEMP[i]<-weatherFull$temp[row][1]
  IIT_FINAL_AGG$COND[i]<-weatherFull$cond[row][1]
  IIT_FINAL_AGG$WIND[i]<-weatherFull$wind_spd[row][1]
  IIT_FINAL_AGG$HUM[i]<-weatherFull$hum[row][1]
  IIT_FINAL_AGG$PRECIP[i]<-weatherFull$precip[row][1]
}

#Spots where precip is NA should be 0.00
IIT_FINAL_AGG$PRECIP[is.na(IIT_FINAL_AGG$PRECIP)]<-0.00

#missed a couple of weather descriptions
for (i in 1:length(IIT_FINAL_AGG$OCCURED)){
  if(is.na(IIT_FINAL_AGG$STAND_COND[i])){
    node<-IIT_FINAL_AGG$COND[i]
    if (!is.na(node)){
      if(node == 'Thunderstorms and Rain'){
        IIT_FINAL_AGG$STAND_COND[i]<-('Thunderstorms')
        IIT_FINAL_AGG$SEVERITY[i]<-('Medium')
      }
      else if (node == 'Patches of Fog'){
        IIT_FINAL_AGG$STAND_COND[i]<-('Fog')
        IIT_FINAL_AGG$SEVERITY[i]<-('Low')
      }
    }
  }
}

#Save data
setwd("~/Documents/Math 571 project")
save(IIT_FINAL_AGG, file = 'IIT_FINAL_AGG.Rda')
