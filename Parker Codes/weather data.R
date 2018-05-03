#https://cran.r-project.org/web/packages/rwunderground/README.html
vcxvc#https://cran.r-project.org/web/packages/rwunderground/README.html
library(devtools)
devtools::install_github('ALShum/rwunderground')
library(rwunderground)

rwunderground::set_api_key('651e75c69a9131c3')
key=get_api_key()
location=set_location(territory = 'Illinois', city = 'Chicago')
#Warning, only 10 calls per minute which the function has a default parameter to slow down. Also only 500 calls per day (1 day of weather = 1 call)
weather20170329_20180319=data.frame(history_range(location = location, date_start = '20170329', date_end = '20180319'))

#Dates needed to be inputed manually so that we did not go over the call limit in a day.
#first date range used: 1/1/2014-4/26/2015
#second date range used: 4/27/2015-11/13/2015
#third date range used: 11/14/2015-3/28/2017
#fourth data range used: 3/29/2017-3/19/2018