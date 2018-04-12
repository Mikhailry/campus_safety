library(corrplot)
library(caret)
#load in data
#CHANGE PATH
load("~/Documents/Math 571 project/Data/iitCrimeWithWeather.Rda")
load("~/Documents/Math 571 project/Data/ucCrimeWithWeather.Rda")
load("~/Documents/Math 571 project/Data/iitAreaWithWeather.Rda")
load("~/Documents/Math 571 project/Data/ucAreaWithWeather.Rda")

sort(table(as.factor(iitCrime$weather_cond_exact)), decreasing = TRUE)
sort(table(as.factor(Uchicago_campus_crimes_cleaned$weather_cond_exact)), decreasing = TRUE)
sort(table(as.factor(Crime_2014toPres_IIT_area$weather_cond_exact)), decreasing = TRUE)
sort(table(as.factor(Crime_2014toPres_UC_area$weather_cond_exact)), decreasing = TRUE)
plot(sort(table(as.factor(iitCrime$weather_cond_exact)), decreasing = TRUE)[1:8])
plot(sort(table(as.factor(Uchicago_campus_crimes_cleaned$weather_cond_exact)), decreasing = TRUE)[1:8])
plot(sort(table(as.factor(Crime_2014toPres_IIT_area$weather_cond_exact)), decreasing = TRUE)[1:8])
plot(sort(table(as.factor(Crime_2014toPres_UC_area$weather_cond_exact)), decreasing = TRUE)[1:8])

#Most common crimes 
sort(table(factor(Crime_2014toPres_IIT_area$`Primary Type`)), decreasing = TRUE)[1:8]
sort(table(factor(Crime_2014toPres_UC_area$`Primary Type`)), decreasing = TRUE)[1:8]
plot(sort(table(factor(Crime_2014toPres_IIT_area$`Primary Type`)), decreasing = TRUE)[1:8])
plot(sort(table(factor(Crime_2014toPres_UC_area$`Primary Type`)), decreasing = TRUE)[1:8])

#contingency table
t<-table(factor(Crime_2014toPres_IIT_area$weather_cond_exact),factor(Crime_2014toPres_IIT_area$`Primary Type`))
table(factor(Crime_2014toPres_UC_area$weather_cond_exact),factor(Crime_2014toPres_UC_area$`Primary Type`))

#algstat package

