library(lubridate)
library(readr)
#read data
IIT_FINAL_AGG <- read_csv("~/Documents/Math 571 project/IIT_FINAL_AGG.csv")
UC_FINAL_AGG <- read_csv("~/Documents/Math 571 project/UC_FINAL_AGG.csv")

#Change timezone and add so it is correct time
attr(IIT_FINAL_AGG$OCCURED,"tzone")<-"America/Chicago"
attr(UC_FINAL_AGG$OCCURED,"tzone")<-"America/Chicago"
IIT_FINAL_AGG$OCCURED<-IIT_FINAL_AGG$OCCURED+hours(5)
UC_FINAL_AGG$OCCURED<-UC_FINAL_AGG$OCCURED+hours(5)

#time buckets, day of week, and month
IIT_FINAL_AGG$MONTH<-NA
UC_FINAL_AGG$MONTH<-NA
IIT_FINAL_AGG$DAY<-NA
UC_FINAL_AGG$DAY<-NA
IIT_FINAL_AGG$TIME_BUCKET<-NA
UC_FINAL_AGG$TIME_BUCKET<-NA

IIT_FINAL_AGG$MONTH<-as.numeric(strftime(IIT_FINAL_AGG$OCCURED, format= '%m', tz='America/Chicago'))
UC_FINAL_AGG$MONTH<-as.numeric(strftime(UC_FINAL_AGG$OCCURED, format= '%m', tz='America/Chicago'))
IIT_FINAL_AGG$DAY<-weekdays(IIT_FINAL_AGG$OCCURED)
UC_FINAL_AGG$DAY<-weekdays(UC_FINAL_AGG$OCCURED)
#0:0-3   1:3-6    2:6-9   3:9-12   4:12-15    5:15-18   6:18-21   7:21-24
IIT_FINAL_AGG$TIME_BUCKET<-floor((as.numeric(strftime(IIT_FINAL_AGG$OCCURED, format= '%H', tz='America/Chicago')))/3)
UC_FINAL_AGG$TIME_BUCKET<-floor((as.numeric(strftime(UC_FINAL_AGG$OCCURED, format= '%H', tz='America/Chicago')))/3)

#Add weather based on the times for IIT and UC
load("~/Documents/Math 571 project/Data/cleanweatherFull.Rda")
IIT_FINAL_AGG$COND<-NA
IIT_FINAL_AGG$STAND_COND<-NA
IIT_FINAL_AGG$SEVERITY<-NA
IIT_FINAL_AGG$TEMP<-NA
IIT_FINAL_AGG$HUM<-NA
IIT_FINAL_AGG$WIND<-NA
IIT_FINAL_AGG$PRECIP<-NA

UC_FINAL_AGG$COND<-NA
UC_FINAL_AGG$STAND_COND<-NA
UC_FINAL_AGG$SEVERITY<-NA
UC_FINAL_AGG$TEMP<-NA
UC_FINAL_AGG$HUM<-NA
UC_FINAL_AGG$WIND<-NA
UC_FINAL_AGG$PRECIP<-NA

start<-Sys.time()
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

for (i in 1:length(UC_FINAL_AGG$OCCURED)){
  min_diff<-min(abs(UC_FINAL_AGG$OCCURED[i]-weatherFull$date))
  row<-as.numeric(row.names(weatherFull[abs(UC_FINAL_AGG$OCCURED[i]-weatherFull$date)==min_diff,]))
  UC_FINAL_AGG$STAND_COND[i]<-weatherFull$condition[row][1]
  UC_FINAL_AGG$SEVERITY[i]<-weatherFull$severity[row][1]
  UC_FINAL_AGG$TEMP[i]<-weatherFull$temp[row][1]
  UC_FINAL_AGG$COND[i]<-weatherFull$cond[row][1]
  UC_FINAL_AGG$WIND[i]<-weatherFull$wind_spd[row][1]
  UC_FINAL_AGG$HUM[i]<-weatherFull$hum[row][1]
  UC_FINAL_AGG$PRECIP[i]<-weatherFull$precip[row][1]
}
end<-Sys.time()

#Spots where precip is NA shoul dbe 0.00
IIT_FINAL_AGG$PRECIP[is.na(IIT_FINAL_AGG$PRECIP)]<-0.00
UC_FINAL_AGG$PRECIP[is.na(IIT_FINAL_AGG$PRECIP)]<-0.00

#Save data
setwd("~/Documents/Math 571 project")
save(IIT_FINAL_AGG, file = 'IIT_FINAL_AGG.Rda')
save(UC_FINAL_AGG, file = 'UC_FINAL_AGG.Rda')
