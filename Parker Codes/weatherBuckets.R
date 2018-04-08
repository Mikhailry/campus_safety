load("~/Documents/Math 571 project/Data/iitAreaWithWeather.Rda")
load("~/Documents/Math 571 project/Data/iitCrimeWithWeather.Rda")
load("~/Documents/Math 571 project/Data/ucAreaWithWeather.Rda")
load("~/Documents/Math 571 project/Data/ucCrimeWithWeather.Rda")

Crime_2014toPres_IIT_area$bucket<-NA
Crime_2014toPres_UC_area$bucket<-NA
iitCrime$bucket<-NA
Uchicago_campus_crimes_cleaned$bucket<-NA
Crime_2014toPres_IIT_area$Month<-NA
Crime_2014toPres_UC_area$Month<-NA
iitCrime$Month<-NA
Uchicago_campus_crimes_cleaned$Month<-NA

#IIT crime
#6 hour buckets, 0-6, 6-12, 12-18, 18-24
for (i in 1:length(iitCrime$Incident)){
  iitCrime$Month[i]<-as.numeric(strftime(iitCrime$Occured[i], format= '%m', tz='America/Chicago'))
  if (is.na(iitCrime$Occured[i])){
    iitCrime$bucket[i]<-NA
  }
  else if (as.numeric(strftime(iitCrime$Occured[i], format= '%H', tz='America/Chicago'))<6){
    iitCrime$bucket[i]<-1
  }
  else if (as.numeric(strftime(iitCrime$Occured[i], format= '%H', tz='America/Chicago'))<12){
    iitCrime$bucket[i]<-2
  }
  else if (as.numeric(strftime(iitCrime$Occured[i], format= '%H', tz='America/Chicago'))<18){
    iitCrime$bucket[i]<-3
  }
  else if (as.numeric(strftime(iitCrime$Occured[i], format= '%H', tz='America/Chicago'))<24){
    iitCrime$bucket[i]<-4
  }
}

#UC Crime
for (i in 1:length(Uchicago_campus_crimes_cleaned$Incident)){
  Uchicago_campus_crimes_cleaned$Month[i]<-as.numeric(strftime(Uchicago_campus_crimes_cleaned$posixTime[i], format= '%m', tz='America/Chicago'))
  if (is.na(Uchicago_campus_crimes_cleaned$posixTime[i])){
    Uchicago_campus_crimes_cleaned$bucket[i]<-NA
  }
  else if (as.numeric(strftime(Uchicago_campus_crimes_cleaned$posixTime[i], format= '%H', tz='America/Chicago'))<6){
    Uchicago_campus_crimes_cleaned$bucket[i]<-1
  }
  else if (as.numeric(strftime(Uchicago_campus_crimes_cleaned$posixTime[i], format= '%H', tz='America/Chicago'))<12){
    Uchicago_campus_crimes_cleaned$bucket[i]<-2
  }
  else if (as.numeric(strftime(Uchicago_campus_crimes_cleaned$posixTime[i], format= '%H', tz='America/Chicago'))<18){
    Uchicago_campus_crimes_cleaned$bucket[i]<-3
  }
  else if (as.numeric(strftime(Uchicago_campus_crimes_cleaned$posixTime[i], format= '%H', tz='America/Chicago'))<24){
    Uchicago_campus_crimes_cleaned$bucket[i]<-4
  }
}
#IIT area
for (i in 1:length(Crime_2014toPres_IIT_area$Date)){
  Crime_2014toPres_IIT_area$Month[i]<-as.numeric(strftime(Crime_2014toPres_IIT_area$Date[i], format= '%m', tz='America/Chicago'))
  if (is.na(Crime_2014toPres_IIT_area$Date[i])){
    Crime_2014toPres_IIT_area$bucket[i]<-NA
  }
  else if (as.numeric(strftime(Crime_2014toPres_IIT_area$Date[i], format= '%H', tz='America/Chicago'))<6){
    Crime_2014toPres_IIT_area$bucket[i]<-1
  }
  else if (as.numeric(strftime(Crime_2014toPres_IIT_area$Date[i], format= '%H', tz='America/Chicago'))<12){
    Crime_2014toPres_IIT_area$bucket[i]<-2
  }
  else if (as.numeric(strftime(Crime_2014toPres_IIT_area$Date[i], format= '%H', tz='America/Chicago'))<18){
    Crime_2014toPres_IIT_area$bucket[i]<-3
  }
  else if (as.numeric(strftime(Crime_2014toPres_IIT_area$Date[i], format= '%H', tz='America/Chicago'))<24){
    Crime_2014toPres_IIT_area$bucket[i]<-4
  }
}
#UC Area
for (i in 1:length(Crime_2014toPres_UC_area$Date)){
  Crime_2014toPres_UC_area$Month[i]<-as.numeric(strftime(Crime_2014toPres_UC_area$Date[i], format= '%m', tz='America/Chicago'))
  if (is.na(Crime_2014toPres_UC_area$Date[i])){
    Crime_2014toPres_UC_area$bucket[i]<-NA
  }
  else if (as.numeric(strftime(Crime_2014toPres_UC_area$Date[i], format= '%H', tz='America/Chicago'))<6){
    Crime_2014toPres_UC_area$bucket[i]<-1
  }
  else if (as.numeric(strftime(Crime_2014toPres_UC_area$Date[i], format= '%H', tz='America/Chicago'))<12){
    Crime_2014toPres_UC_area$bucket[i]<-2
  }
  else if (as.numeric(strftime(Crime_2014toPres_UC_area$Date[i], format= '%H', tz='America/Chicago'))<18){
    Crime_2014toPres_UC_area$bucket[i]<-3
  }
  else if (as.numeric(strftime(Crime_2014toPres_UC_area$Date[i], format= '%H', tz='America/Chicago'))<24){
    Crime_2014toPres_UC_area$bucket[i]<-4
  }
}
summary(iitCrime)
summary(Uchicago_campus_crimes_cleaned)
summary(Crime_2014toPres_IIT_area)
summary(Crime_2014toPres_UC_area)
#histograms to view crime during a time
hist(iitCrime$bucket, xaxt='n', xlab = 'Time Frame', ylab = '# of Crimes', main = 'IIT Public Safety')
axis(1, at=c(1.0, 2.0, 3.0, 4.0), labels=c('0:00-6:00','6:00-12:00','12:00-18:00','18:00-24:00'))
hist(Uchicago_campus_crimes_cleaned$bucket, xaxt='n',xlab = 'Time Frame', ylab = '# of Crimes', main = 'UC Public Safety')
axis(1, at=c(1.0, 2.0, 3.0, 4.0), labels=c('0:00-6:00','6:00-12:00','12:00-18:00','18:00-24:00'))
hist(Crime_2014toPres_IIT_area$bucket, xaxt='n', xlab = 'Time Frame', ylab = '# of Crimes', main = 'IIT Area')
axis(1, at=c(1.0, 2.0, 3.0, 4.0), labels=c('0:00-6:00','6:00-12:00','12:00-18:00','18:00-24:00'))
hist(Crime_2014toPres_UC_area$bucket, xaxt='n', xlab = 'Time Frame', ylab = '# of Crimes', main = 'UC Area')
axis(1, at=c(1.0, 2.0, 3.0, 4.0), labels=c('0:00-6:00','6:00-12:00','12:00-18:00','18:00-24:00'))
#histogram for time 2
plot(as.factor(strftime(iitCrime$Occured, format= '%H', tz='America/Chicago')), xlab = 'Hour of Day', ylab = '# of Crimes', main = 'IIT Public Safety')
plot(as.factor(strftime(Uchicago_campus_crimes_cleaned$posixTime, format= '%H', tz='America/Chicago')), xlab = 'Hour of Day', ylab = '# of Crimes', main = 'UC Public Safety')
plot(as.factor(strftime(Crime_2014toPres_IIT_area$Date, format= '%H', tz='America/Chicago')), xlab = 'Hour of Day', ylab = '# of Crimes', main = 'IIT Area')
plot(as.factor(strftime(Crime_2014toPres_UC_area$Date, format= '%H', tz='America/Chicago')), xlab = 'Hour of Day', ylab = '# of Crimes', main = 'UC Area')
#histograms for type of weather
plot(as.factor(iitCrime$weather_cond), xlab='Weather Condition', ylab='# of Crimes', main = 'IIT Public Safety')
plot(as.factor(Uchicago_campus_crimes_cleaned$weather_cond), xlab='Weather Condition', ylab='# of Crimes', main = 'UC Public Safety')
plot(as.factor(Crime_2014toPres_IIT_area$weather_cond), xlab='Weather Condition', ylab='# of Crimes', main = 'IIT Area')
plot(as.factor(Crime_2014toPres_UC_area$weather_cond), xlab='Weather Condition', ylab='# of Crimes', main = 'UC Area')
#weather severity
plot(factor(iitCrime$weather_sev, levels = c('Low', 'Medium', 'High')), xlab='Weather Severity', ylab='# of Crimes', main = 'IIT Public Safety')
plot(factor(Uchicago_campus_crimes_cleaned$weather_sev, levels = c('Low', 'Medium', 'High')), xlab='Weather Severity', ylab='# of Crimes', main = 'UC Public Safety')
plot(factor(Crime_2014toPres_IIT_area$weather_sev, levels = c('Low', 'Medium', 'High')), xlab='Weather Severity', ylab='# of Crimes', main = 'IIT Area')
plot(factor(Crime_2014toPres_UC_area$weather_sev, levels = c('Low', 'Medium', 'High')), xlab='Weather Severity', ylab='# of Crimes', main = 'UC Area')
#histograms for temperature
hist(iitCrime$temperature, xlab = 'Temperature in Degress F', ylab = '# of Crimes', main = 'IIT Public Safety')
hist(Uchicago_campus_crimes_cleaned$temperature, xlab = 'Temperature in Degress F', ylab = '# of Crimes', main = 'UC Public Safety')
hist(Crime_2014toPres_IIT_area$temperature, xlab = 'Temperature in Degress F', ylab = '# of Crimes', main = 'IIT Area')
hist(Crime_2014toPres_UC_area$temperature, xlab = 'Temperature in Degress F', ylab = '# of Crimes', main = 'UC Area')
#histogram for month
hist(iitCrime$Month, xlab = 'Month', ylab = '# of Crimes', main = 'IIT Public Safety',xaxt='n')
axis(1, at=seq(1,12,1), labels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
hist(Uchicago_campus_crimes_cleaned$Month, xlab = 'Month', ylab = '# of Crimes', main = 'UC Public Safety', xaxt='n')
axis(1, at=seq(1,12,1), labels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
hist(Crime_2014toPres_IIT_area$Month, xlab = 'Month', ylab = '# of Crimes', main = 'IIT Area', xaxt='n')
axis(1, at=seq(1,12,1), labels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
hist(Crime_2014toPres_UC_area$Month, xlab = 'Month', ylab = '# of Crimes', main = 'UC Area', xaxt='n')
axis(1, at=seq(1,12,1), labels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

#TODO
#histogram of weekday and weekends
#histogram of day of the week


