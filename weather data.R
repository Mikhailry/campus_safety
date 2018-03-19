#https://cran.r-project.org/web/packages/rwunderground/README.html
library(devtools)
devtools::install_github('ALShum/rwunderground')
library(rwunderground)

rwunderground::set_api_key('651e75c69a9131c3')
key=get_api_key()
location=set_location(territory = 'Illinois', city = 'Chicago')
#Warning, only 10 calls per minute which the function has a default parameter to slow down. Also only 500 calls per day (1 day of weather = 1 call)
weather20150427_20151113=data.frame(history_range(location = location, date_start = '20150427', date_end = '20151113'))
