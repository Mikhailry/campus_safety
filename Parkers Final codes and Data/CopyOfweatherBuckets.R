load("~/Documents/Math 571 project/IIT_new.rda")

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


#Histogram of weekday and weekends
iitCrime$weekday<-weekdays(iitCrime$Occured)
Uchicago_campus_crimes_cleaned$weekday<-weekdays(Uchicago_campus_crimes_cleaned$posixTime)
Crime_2014toPres_IIT_area$weekday<-weekdays(Crime_2014toPres_IIT_area$Date)
Crime_2014toPres_UC_area$weekday<-weekdays(Crime_2014toPres_UC_area$Date)
plot(factor(iitCrime$weekday), xlab='Weekday',ylab='# of Crimes', main='IIT Public Safety')
plot(factor(Uchicago_campus_crimes_cleaned$weekday), xlab='Weekday',ylab='# of Crimes', main='UC Public Safety')
plot(factor(Crime_2014toPres_IIT_area$weekday),xlab='Weekday',ylab='# of Crimes', main='IIT Area')
plot(factor(Crime_2014toPres_UC_area$weekday), xlab='Weekday',ylab='# of Crimes', main='UC Area')

#Histogram of weather data from 
load("~/Documents/Math 571 project/Data/cleanWeatherFull.Rda")
plot(factor(weatherFull$cond))
plot(factor(weatherFull$condition))
sort(table(factor(weatherFull$cond)), decreasing = TRUE)
plot(sort(table(factor(weatherFull$cond)), decreasing = TRUE)[1:8])

#TODO
#Normalize weather conditions for crime.
weather_freq<-data.frame(table(weatherFull$condition))
weather_freq$Freq<-data.frame(table(iitCrime$weather_cond))[,2]/weather_freq[,2]
plot(weather_freq, type='h')

#temperature ans crime frequency as a scatter plot
plot(table(round(iitCrime$temperature)), type = 'p')
plot(table(round(Uchicago_campus_crimes_cleaned$temperature)), type = 'p')
plot(table(round(Crime_2014toPres_IIT_area$temperature)), type = 'p')
plot(table(round(Crime_2014toPres_UC_area$temperature)), type = 'p')
