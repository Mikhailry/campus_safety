library(readxl)
library(readr)
library(lubridate)

#Change path, load data
Crime_2014toPres_IIT_area <- read_excel("~/Documents/Math 571 project/Mathew_pub_saf/Crime_2014toPres_IIT area.xlsx")
Crime_2014toPres_UC_area <- read_excel("~/Documents/Math 571 project/Mathew_pub_saf/Crime_2014toPres_UC area.xlsx")
load("~/Documents/Math 571 project/Data/cleanweatherFull.Rda")
load("~/Documents/Math 571 project/Data/iitCrime.Rda")
Uchicago_campus_crimes_cleaned <- read_csv("~/Documents/Math 571 project/Uchicago_campus_crimes_cleaned.csv")


#*******************Crime around the Schools**************************

#Convert date/time into CDT time for area crime
attr(Crime_2014toPres_IIT_area$Date,"tzone")<-"America/Chicago"
attr(Crime_2014toPres_UC_area$Date,"tzone")<-"America/Chicago"
Crime_2014toPres_IIT_area$Date<-Crime_2014toPres_IIT_area+hours(5)
Crime_2014toPres_UC_area$Date<-Crime_2014toPres_UC_area+hours(5)

#Add new columns for weather condition
Crime_2014toPres_IIT_area[,'weather_cond']<-NA
Crime_2014toPres_IIT_area[,'weather_sev']<-NA
Crime_2014toPres_IIT_area[,'temperature']<-NA
Crime_2014toPres_IIT_area[,'weather_cond_exact']<-NA

#loop to add weather condition to the Crime_2014toPred_IIT_area data set
#Takes a while
for (i in 1:length(Crime_2014toPres_IIT_area$Date)){
  min_diff<-min(abs(Crime_2014toPres_IIT_area$Date[i]-weatherFull$date))
  row<-as.numeric(row.names(weatherFull[abs(Crime_2014toPres_IIT_area$Date[i]-weatherFull$date)==min_diff,]))
  Crime_2014toPres_IIT_area$weather_cond[i]<-weatherFull$condition[row][1]
  Crime_2014toPres_IIT_area$weather_sev[i]<-weatherFull$severity[row][1]
  Crime_2014toPres_IIT_area$temperature[i]<-weatherFull$temp[row][1]
  Crime_2014toPres_IIT_area$weather_cond_exact[i]<-weatherFull$cond[row][1]
}

Crime_2014toPres_UC_area[,'weather_cond']<-NA
Crime_2014toPres_UC_area[,'weather_sev']<-NA
Crime_2014toPres_UC_area[,'temperature']<-NA
Crime_2014toPres_UC_area[,'weather_cond_exact']<-NA

for (i in 1:length(Crime_2014toPres_UC_area$Date)){
  min_diff<-min(abs(Crime_2014toPres_UC_area$Date[i]-weatherFull$date))
  row<-as.numeric(row.names(weatherFull[abs(Crime_2014toPres_UC_area$Date[i]-weatherFull$date)==min_diff,]))
  Crime_2014toPres_UC_area[i,"weather_cond"]<-weatherFull$condition[row][1]
  Crime_2014toPres_UC_area[i,"weather_sev"]<-weatherFull$severity[row][1]
  Crime_2014toPres_UC_area[i,"temperature"]<-weatherFull$temp[row][1]
  Crime_2014toPres_UC_area[i,"weather_cond_exact"]<-weatherFull$cond[row][1]
}

#*******************Public Safety Blogs**************************
#IIT
#Change into Posix time
#iitCrime$Occured<-as.POSIXct(as.character(iitCrime$Occured),"%m/%d/%Y %I:%M %p", tz='America/Chicago')
#Add new columns
iitCrime[,'weather_cond']<-NA
iitCrime[,'weather_sev']<-NA
iitCrime[,'temperature']<-NA
iitCrime[,'weather_cond_exact']<-NA
#Enter weather
for (i in 1:length(iitCrime$Occured)){
  min_diff<-min(abs(iitCrime$Occured[i]-weatherFull$date))
  row<-as.numeric(row.names(weatherFull[abs(iitCrime$Occured[i]-weatherFull$date)==min_diff,]))
  iitCrime[i,"weather_cond"]<-weatherFull$condition[row][1]
  iitCrime[i,"weather_sev"]<-weatherFull$severity[row][1]
  iitCrime[i,"temperature"]<-weatherFull$temp[row][1]
  iitCrime[i,"weather_cond_exact"]<-weatherFull$cond[row][1]
}

#UChicago
#Get posix time
Uchicago_campus_crimes_cleaned$temp<-paste(Uchicago_campus_crimes_cleaned$Date, as.character(Uchicago_campus_crimes_cleaned$Time), sep=' ')
Uchicago_campus_crimes_cleaned$posixTime<-as.POSIXct(Uchicago_campus_crimes_cleaned$temp, '%m/%d/%y %H:%M:%S', tz= 'America/Chicago')
Uchicago_campus_crimes_cleaned$temp<-NULL
#Add new columns
Uchicago_campus_crimes_cleaned[,'weather_cond']<-NA
Uchicago_campus_crimes_cleaned[,'weather_sev']<-NA
Uchicago_campus_crimes_cleaned[,'temperature']<-NA
Uchicago_campus_crimes_cleaned[,'weather_cond_exact']<-NA
#Enter weather
for (i in 1:length(Uchicago_campus_crimes_cleaned$posixTime)){
  min_diff<-min(abs(Uchicago_campus_crimes_cleaned$posixTime[i]-weatherFull$date))
  row<-as.numeric(row.names(weatherFull[abs(Uchicago_campus_crimes_cleaned$posixTime[i]-weatherFull$date)==min_diff,]))
  Uchicago_campus_crimes_cleaned[i,"weather_cond"]<-weatherFull$condition[row][1]
  Uchicago_campus_crimes_cleaned[i,"weather_sev"]<-weatherFull$severity[row][1]
  Uchicago_campus_crimes_cleaned[i,"temperature"]<-weatherFull$temp[row][1]
  Uchicago_campus_crimes_cleaned[i,"weather_cond_exact"]<-weatherFull$cond[row][1]
}

#save
setwd("~/Documents/Math 571 project/Data")
save(Crime_2014toPres_IIT_area,file = 'iitAreaWithWeather.Rda')
save(Crime_2014toPres_UC_area,file = 'ucAreaWithWeather.Rda')
save(iitCrime,file = 'iitCrimeWithWeather.Rda')
save(Uchicago_campus_crimes_cleaned,file = 'ucCrimeWithWeather.Rda')