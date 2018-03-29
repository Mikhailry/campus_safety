library(readxl)

#Change path, load data
Crime_2014toPres_IIT_area <- read_excel("~/Documents/Math 571 project/Mathew_pub_saf/Crime_2014toPres_IIT area.xlsx")
Crime_2014toPres_UC_area <- read_excel("~/Documents/Math 571 project/Mathew_pub_saf/Crime_2014toPres_UC area.xlsx")
#load("~/Documents/Math 571 project/Data/weatherFull.Rda")
load("~/Documents/Math 571 project/Data/iitCrime.Rda")

#*******************Crime around the Schools**************************

#Convert date/time into CDT time for area crime
attr(Crime_2014toPres_IIT_area$Date,"tzone")<-"America/Chicago"
attr(Crime_2014toPres_UC_area$Date,"tzone")<-"America/Chicago"

#Add new columns for weather condition
Crime_2014toPres_IIT_area[,'weather_cond']<-NA
Crime_2014toPres_IIT_area[,'weather_sev']<-NA
Crime_2014toPres_IIT_area[,'temperature']<-NA

#loop to add weather condition to the Crime_2014toPred_IIT_area data set
#Takes a while
for (i in 1:length(Crime_2014toPres_IIT_area$Date)){
  min_diff<-min(abs(Crime_2014toPres_IIT_area$Date[i]-weatherFull$date))
  row<-as.numeric(row.names(weatherFull[abs(Crime_2014toPres_IIT_area$Date[i]-weatherFull$date)==min_diff,]))
  Crime_2014toPres_IIT_area$weather_cond[i]<-weatherFull$condition[row][1]
  Crime_2014toPres_IIT_area$weather_sev[i]<-weatherFull$severity[row][1]
  Crime_2014toPres_IIT_area$temperature[i]<-weatherFull$temp[row][1]
}

Crime_2014toPres_UC_area[,'weather_cond']<-NA
Crime_2014toPres_UC_area[,'weather_sev']<-NA
Crime_2014toPres_UC_area[,'temperature']<-NA

for (i in 1:length(Crime_2014toPres_UC_area$Date)){
  min_diff<-min(abs(Crime_2014toPres_UC_area$Date[i]-weatherFull$date))
  row<-as.numeric(row.names(weatherFull[abs(Crime_2014toPres_UC_area$Date[i]-weatherFull$date)==min_diff,]))
  Crime_2014toPres_UC_area[i,"weather_cond"]<-weatherFull$condition[row][1]
  Crime_2014toPres_UC_area[i,"weather_sev"]<-weatherFull$severity[row][1]
  Crime_2014toPres_UC_area[i,"temperature"]<-weatherFull$temp[row][1]
}

#*******************Public Safety Blogs**************************
#IIT
#Change into Posix time
iitCrime$Occured<-as.POSIXct(as.character(iitCrime$Occured),"%m/%d/%Y %I:%M %p", tz='America/Chicago')
#Add new columns
iitCrime[,'weather_cond']<-NA
iitCrime[,'weather_sev']<-NA
iitCrime[,'temperature']<-NA
#Enter weather
for (i in 1:length(iitCrime$Occured)){
  min_diff<-min(abs(iitCrime$Occured[i]-weatherFull$date))
  row<-as.numeric(row.names(weatherFull[abs(iitCrime$Occured[i]-weatherFull$date)==min_diff,]))
  iitCrime[i,"weather_cond"]<-weatherFull$condition[row][1]
  iitCrime[i,"weather_sev"]<-weatherFull$severity[row][1]
  iitCrime[i,"temperature"]<-weatherFull$temp[row][1]
}




