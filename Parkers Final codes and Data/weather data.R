#https://cran.r-project.org/web/packages/rwunderground/README.html
#install rwunderground package
library(devtools)
devtools::install_github('ALShum/rwunderground')
library(rwunderground)

#set api key and location
rwunderground::set_api_key('651e75c69a9131c3')
key=get_api_key()
location=set_location(territory = 'Illinois', city = 'Chicago')

#Warning, only 10 calls per minute which the function has a default parameter to slow down. Also only 500 calls per day (1 day of weather = 1 call)
#weather20140101_20150426=data.frame(history_range(location = location, date_start = '20140101', date_end = '20150426'))
#weather20150427_20151113=data.frame(history_range(location = location, date_start = '20150427', date_end = '20151113'))
#weather20151114_20170328=data.frame(history_range(location = location, date_start = '20151114', date_end = '20170328'))
weather20170329_20180319=data.frame(history_range(location = location, date_start = '20170329', date_end = '20180319'))

#Dates needed to be inputed manually so that we did not go over the call limit in a day.
#first date range used: 1/1/2014-4/26/2015
#second date range used: 4/27/2015-11/13/2015
#third date range used: 11/14/2015-3/28/2017
#fourth data range used: 3/29/2017-3/19/2018

#set path for saving and loading
path<-"~/Documents/Math 571 project/Parkers Final codes and Data"
setwd(path)
  
#save(weather20140101_20150426, file = 'weather1.Rda')
#save(weather20150427_20151113, file = 'weather2.Rda')
#save(weather20151114_20170328, file = 'weather3.Rda')
#save(weather20170329_20180319, file = 'weather4.Rda')

load('weather1.Rda')
load('weather2.Rda')
load('weather3.Rda')
load('weather4.Rda')

#Merge weather data to get full data set and save.
weatherFull<-rbind(weather20140101_20150426, weather20150427_20151113, weather20151114_20170328, eather20170329_20180319)
save(weatherFull, file = 'weatherFull.Rda')
