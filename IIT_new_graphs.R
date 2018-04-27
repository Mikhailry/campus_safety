#-----------------------------------------------------------
#This script contains exploratory analysis of IIT and Uchicago
#data using visualization techniques
#-----------------------------------------------------------

#installing libraries
#install.packages("chron")
#install.packages("ggplot2")
#install.packages('ggmap')
#install.packages('dplyr')
#install.packages('scales')
#install.packages('lubridate')

#loading libraries
library("ggplot2")
library("ggmap")
library('dplyr')
library('scales')
library("chron")
library('lubridate')

#loading iit and uc crime data
load("D:/Github/CSP571/Project/campus_safety/IIT_new.rda")

#renaming df's
iit <- iit3
#uc <- UC_FINAL_AGG

#minor changes for graphing
#drop na's for occured variable
iit <- iit[which(!is.na(iit$OCCURED)),]
# uc <- uc[which(!is.na(uc$OCCURED)),]
# 
# #converting month from numbers to month(dt)
# iit$MONTH <- month(iit$MONTH, label=T, abbr = T)
# uc$MONTH <- month(uc$MONTH, label=T, abbr = T)
# 
# #extracting day of the week from occured date
# iit$DAY <- wday(iit$OCCURED, label=T, abbr = T, week_start = getOption("lubridate.week.start", 1))
# uc$DAY <- wday(uc$OCCURED, label=T, abbr = T, week_start = getOption("lubridate.week.start", 1))
# 
# #IIT labeling time bucket factors
# iit$TIME_BUCKET <- factor(iit$TIME_BUCKET,
#                           levels = c(0,1,2,3,4,5,6,7),
#                           labels = c("00-03", "03-06", "06-09", "09-12", "12-15", "15-18", "18-21", "21-00"))
# 
# #UC labeling time bucket factors
# uc$TIME_BUCKET <- factor(uc$TIME_BUCKET,
#                          levels = c(0,1,2,3,4,5,6,7),
#                          labels = c("00-03", "03-06", "06-09", "09-12", "12-15", "15-18", "18-21", "21-00"))
# 
# #removing dates occured earlier than 2014-01-01 00:00:01
# iit <- iit[iit$OCCURED>"2014-01-01 00:00:01",]
# uc <- uc[uc$OCCURED>"2014-01-01 00:00:01",]
# 
# 
# #separating iit campus area and iit surrounding area
# iitCamp <- iit[which(iit$TYPE_OF_DATA=="IIT-CAMPUS"),]
# iitArea <- iit[which(iit$TYPE_OF_DATA=="IIT-AREA"),]
# stopifnot(nrow(iitCamp)+nrow(iitArea)==nrow(iit))
# 
# #separating uc campus area and uc surrounding area
# ucCamp <- uc[which(uc$TYPE_OF_DATA=="UC-CAMPUS"),]
# ucArea <- uc[which(uc$TYPE_OF_DATA=="UC-AREA"),]
# stopifnot(nrow(ucCamp)+nrow(ucArea)==nrow(uc))
# 
# # #creating single dataframe to be used in some graphs
# # total <- rbind(iit, uc)
# 
# #saving iit data
# save(iit, file="iit.rda")
# save(uc, file="uc.rda")

#IIT - By crime type
#-----------------------------------------------------------
#plot incident type frequency for IIT camp + area
ggplot(iit, aes(x=INCIDENT_TYPE2)) + geom_bar(fill="steelblue", alpha = .8)+ 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
  labs(title="IIT Campus and Surrounding area Incidents by Frequency") + labs(x="Incident type", y="Frequency")

#plot incident type frequency for IIT camp
# ggplot(iitCamp, aes(x=INCIDENT_TYPE2)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="IIT campus area crime incidents by type frequency") + labs(x="Incident type", y="Frequency")
# 
# #plot incident type frequency for IIT area
# ggplot(iitArea, aes(x=INCIDENT_TYPE2)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="IIT surrounding area crime incidents by type frequency") + labs(x="Incident type", y="Frequency")
#-----------------------------------------------------------

#UC - By crime type
#-----------------------------------------------------------
#plot incident type frequency for UC camp + area
# ggplot(uc, aes(x=INCIDENT_TYPE2)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC Campus and surrounding area crime incidents by type frequency") + labs(x="Incident type", y="Frequency")
# 
# #plot incident type frequency for UC camp
# ggplot(ucCamp, aes(x=INCIDENT_TYPE2)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC campus area crime incidents by type frequency") + labs(x="Incident type", y="Frequency")
# 
# #plot incident type frequency for UC area
# ggplot(ucArea, aes(x=INCIDENT_TYPE2)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC surrounding area crime incidents by type frequency") + labs(x="Incident type", y="Frequency")
# #-----------------------------------------------------------

#IIT - separating crime and non-crime data
#-----------------------------------------------------------
# crimInd <- which(iit$INCIDENT_TYPE2!="NON CRIMINAL")
# #let's split crime and non-crime data
# iitCrime <- iit[crimInd,]
# iitNonCrime <- iit[-crimInd,]
# stopifnot(nrow(iitCrime)+nrow(iitNonCrime)==nrow(iit))
# 
# #separating iit campus area and iit surrounding area
# iitCampCrime <- iitCrime[which(iitCrime$TYPE_OF_DATA=="IIT-CAMPUS"),]
# iitAreaCrime <- iitCrime[which(iitCrime$TYPE_OF_DATA=="IIT-AREA"),]
# stopifnot(nrow(iitCampCrime)+nrow(iitAreaCrime)==nrow(iitCrime))
#-----------------------------------------------------------

#UC - separating crime and non-crime data
#-----------------------------------------------------------
# crimIndUc <- which(uc$INCIDENT_TYPE2!="NON CRIMINAL")
# #let's split crime and non-crime data
# ucCrime <- uc[crimIndUc,]
# ucNonCrime <- uc[-crimIndUc,]
# stopifnot(nrow(ucCrime)+nrow(ucNonCrime)==nrow(uc))
# 
# #separating uc campus area and uc surrounding area
# ucCampCrime <- ucCrime[which(ucCrime$TYPE_OF_DATA=="UC-CAMPUS"),]
# ucAreaCrime <- ucCrime[which(ucCrime$TYPE_OF_DATA=="UC-AREA"),]
# stopifnot(nrow(ucCampCrime)+nrow(ucAreaCrime)==nrow(ucCrime))
#-----------------------------------------------------------


#IIT - By month
#-----------------------------------------------------------
#plot incidents by month for IIT camp + area
ggplot(iit, aes(x=MONTH)) + geom_bar(fill="steelblue", alpha = .8)+ 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
  labs(title="IIT Campus and Surrounding area Incidents by Month") + labs(x="Month", y="Frequency")

#plot incidents by month for IIT camp
# ggplot(iit, aes(x=MONTH)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="IIT Campus crime incidents by month") + labs(x="Month", y="Frequency")
# 
# #plot incidents by month for IIT area
# ggplot(iit, aes(x=MONTH)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="IIT Area crime incidents by month") + labs(x="Month", y="Frequency")
#-----------------------------------------------------------

#UC - By month
#-----------------------------------------------------------
#plot incidents by month for UC camp + area
# ggplot(ucCrime, aes(x=MONTH)) + geom_bar(fill="steelblue", alpha = .8)+
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC Campus and surrounding area crime incidents by month") + labs(x="Month", y="Frequency")
# 
# #plot incidents by month for UC camp
# ggplot(ucCampCrime, aes(x=MONTH)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC Campus crime incidents by month") + labs(x="Month", y="Frequency")
# 
# #plot incidents by month for UC area
# ggplot(ucAreaCrime, aes(x=MONTH)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC Area crime incidents by month") + labs(x="Month", y="Frequency")
# #-----------------------------------------------------------


#IIT - By weekdays
#-----------------------------------------------------------
#plot incidents by weekday for IIT camp + area
ggplot(iit, aes(x=DAY)) + geom_bar(fill="steelblue", alpha = .8)+ 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
  labs(title="IIT campus and Surrounding area Incidents by Days") + labs(x="Weekday", y="Frequency")

#plot incidents by weekday for IIT camp
# ggplot(iitCampCrime, aes(x=DAY)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="IIT campus and Surrounding area Incidents by Weekdays") + labs(x="Weekday", y="Frequency")
# 
# #plot incidents by weekday for IIT area
# ggplot(iitAreaCrime, aes(x=DAY)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="IIT area crime incidents by weekdays") + labs(x="Weekday", y="Frequency")
# #-----------------------------------------------------------

#UC - By weekdays
#-----------------------------------------------------------
#plot incidents by weekday for UC camp + area
# ggplot(ucCrime, aes(x=DAY)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC campus and surrounding area crime incidents by weekdays") + labs(x="Weekday", y="Frequency")
# 
# #plot incidents by weekday for UC camp
# ggplot(ucCampCrime, aes(x=DAY)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC campus crime incidents by weekdays") + labs(x="Weekday", y="Frequency")
# 
# #plot incidents by weekday for UC area
# ggplot(ucAreaCrime, aes(x=DAY)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC area crime incidents by weekdays") + labs(x="Weekday", y="Frequency")
# #-----------------------------------------------------------


#IIT - By time
#-----------------------------------------------------------
#plot incidents by time of the day for IIT camp + area
ggplot(iit, aes(x=TIME_BUCKET)) + geom_bar(fill="steelblue", alpha = .8)+ 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
  labs(title="IIT campus and Surrounding area Incidents by Time of Day") + labs(x="Time of day", y="Frequency")

# #plot incidents by time of the day for IIT camp
# ggplot(iitCampCrime, aes(x=TIME_BUCKET)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="IIT campus crime incidents by time of day") + labs(x="Time of day", y="Frequency")
# 
# #plot incidents by time of the day for IIT area
# ggplot(iitAreaCrime, aes(x=TIME_BUCKET)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="IIT area crime incidents by time of day") + labs(x="Time of day", y="Frequency")
# #-----------------------------------------------------------


#UC - By time
#-----------------------------------------------------------
#plot incidents by time of the day for UC camp + area
# ggplot(ucCrime, aes(x=TIME_BUCKET)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC campus and surrounding area crime incidents by time of day") + labs(x="Time of day", y="Frequency")
# 
# #plot incidents by time of the day for UC camp
# ggplot(ucCampCrime, aes(x=TIME_BUCKET)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC campus crime incidents by time of day") + labs(x="Time of day", y="Frequency")
# 
# #plot incidents by time of the day for UC area
# ggplot(ucAreaCrime, aes(x=TIME_BUCKET)) + geom_bar(fill="steelblue", alpha = .8)+ 
#   geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=1.5, position=position_dodge(.9), size=3) +
#   labs(title="UC area crime incidents by time of day") + labs(x="Time of day", y="Frequency")
# #-----------------------------------------------------------



#Aggregated by type of crime by group time of the day
#each crime type frequency scaled separately, to understand the frequency throughout the day,
#however different crime type frequences can't be compared with each other
#-----------------------------------------------------------
#aggregating crime types by groups of time of the day
crimeTime <- aggregate(iit$INCIDENT_TYPE2, by=list(iit$INCIDENT_TYPE2, iit$TIME_BUCKET), FUN=length)
names(crimeTime) <- c("Crime_type", "Time_interval", "Frequency")

crimeTime$Frequency[which(crimeTime$Crime_type=='MILD INCIDENTS')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='MILD INCIDENTS')], to = c(0, 1))
crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS INCIDENTS')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS INCIDENTS')], to = c(0, 1))
# crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS CRIME')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS CRIME')], to = c(0, 1))
# crimeTime$Frequency[which(crimeTime$Crime_type=='SUBSTANCE CRIME')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='SUBSTANCE CRIME')], to = c(0, 1))

#plotting incident types frequency in respect to time interval 
ggplot(crimeTime, aes(x=Crime_type, y=Time_interval)) +
  geom_tile(aes(fill=crimeTime$Frequency)) + scale_x_discrete("Type of Incident", expand = c(0,0)) +
  scale_y_discrete("Time interval", expand = c(0,-2)) +
  scale_fill_gradient("Crime frequency", low = "white", high = "tomato1", guide = 'none') +
  theme_bw() + ggtitle("Frequency of Incident type by Time intervals") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA)) 
#-----------------------------------------------------------
  
  
  #Aggregated by type of crime by group by day of the week
  #each crime type frequency scaled separately, to understand the frequency throughout the week,
  #however different crime type frequences can't be compared with each other
  #-----------------------------------------------------------
#aggregating crime types by groups of day of the week

crimeTime <- aggregate(iit$INCIDENT_TYPE2, by=list(iit$INCIDENT_TYPE2, iit$DAY), FUN=length)
names(crimeTime) <- c("Crime_type", "Day", "Frequency")

crimeTime$Frequency[which(crimeTime$Crime_type=='MILD INCIDENTS')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='MILD INCIDENTS')], to = c(0, 1))
crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS INCIDENTS')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS INCIDENTS')], to = c(0, 1))
# crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS CRIME')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS CRIME')], to = c(0, 1))
# crimeTime$Frequency[which(crimeTime$Crime_type=='SUBSTANCE CRIME')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='SUBSTANCE CRIME')], to = c(0, 1))

#plotting incident types frequency in respect to time interval 
ggplot(crimeTime, aes(x=Crime_type, y=Day)) +
  geom_tile(aes(fill=crimeTime$Frequency)) + scale_x_discrete("Type of Incident", expand = c(0,0)) +
  scale_y_discrete("Day", expand = c(0,-2)) +
  scale_fill_gradient("Crime frequency", low = "white", high = "tomato", guide = 'none') +
  theme_bw() + ggtitle("Frequency of Incident type by Day of the Week") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA)) 
#-----------------------------------------------------------
  
  
  #Aggregated by type of crime by group by month
  #each crime type frequency scaled separately, to understand the frequency throughout the week,
  #however different crime type frequences can't be compared with each other
  #-----------------------------------------------------------
#aggregating crime types by groups of month
crimeTime <- aggregate(iit$INCIDENT_TYPE2, by=list(iit$INCIDENT_TYPE2, iit$MONTH), FUN=length)
names(crimeTime) <- c("Crime_type", "Month", "Frequency")

crimeTime$Frequency[which(crimeTime$Crime_type=='MILD INCIDENTS')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='MILD INCIDENTS')], to = c(0, 1))
crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS INCIDENTS')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS INCIDENTS')], to = c(0, 1))
# crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS CRIME')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='SERIOUS CRIME')], to = c(0, 1))
# crimeTime$Frequency[which(crimeTime$Crime_type=='SUBSTANCE CRIME')] <- rescale(crimeTime$Frequency[which(crimeTime$Crime_type=='SUBSTANCE CRIME')], to = c(0, 1))

#plotting incident types frequency in respect to month
ggplot(crimeTime, aes(x=Crime_type, y=Month)) +
  geom_tile(aes(fill=crimeTime$Frequency)) + scale_x_discrete("Type of Incident", expand = c(0,0)) +
  scale_y_discrete("Month", expand = c(0,-2)) +
  scale_fill_gradient("Crime frequency", low = "white", high = "tomato", guide = 'none') +
  theme_bw() + ggtitle("Frequency of Incident type by Month") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA)) 
#-----------------------------------------------------------
  
  
  #loading maps and saving them to rda file
  #uncomment to reload and resave maps
  # miesMap <- qmap(location = "Illinois Institute of Technology", zoom=16)
  # miesAreaMap <- qmap(location = "Illinois Institute of Technology", zoom=15)
  # kentMap <- qmap(location = "Chicago-Kent College of Law at Illinois Institute of Technology", zoom=16)
  # kentAreaMap <- qmap(location = "Chicago-Kent College of Law at Illinois Institute of Technology", zoom=15)
  # ucMap <- qmap(location = "The University of Chicago", zoom=16) 
  # ucAreaMap <- qmap(location = "The University of Chicago", zoom=15) 
  # save(miesMap, miesAreaMap, kentMap, kentAreaMap, ucMap, ucAreaMap, file="maps.rda")

#loading campus maps
load("maps.rda")



#IIT binned plot of crimes
#on terrain map
miesMap + stat_bin2d(aes(x = LONGITUDE, y = LATITUDE,  colour = INCIDENT_TYPE2, fill = INCIDENT_TYPE2), size=.5, 
                     bins = 20, alpha=.5, data=iitCrime)

#UC binned plot of crimes
#on terrain map
ucMap + stat_bin2d(aes(x = LONGITUDE, y = LATITUDE,  colour = INCIDENT_TYPE2, fill = INCIDENT_TYPE2), size=.5, 
                   bins = 20, alpha=.5, data=ucCrime)

#iit density of crimes
#-----------------------------------------------------------
#density of crimes for IIT camp + area
miesAreaMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, data = iitCrime, geom = "polygon") + scale_alpha(guide = 'none') + scale_fill_gradient(guide = 'none')

#density of crimes for IIT camp
miesMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, data = iitCampCrime, geom = "polygon") + scale_alpha(guide = 'none') + scale_fill_gradient(guide = 'none')

#density of crimes for IIT area
miesAreaMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, data = iitAreaCrime, geom = "polygon") + scale_alpha(guide = 'none') + scale_fill_gradient(guide = 'none')
#-----------------------------------------------------------

#iit density of crimes by day of the week
#-----------------------------------------------------------
#density of crimes for IIT camp + area
miesAreaMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, geom = "polygon", data=iitCrime) +
  scale_fill_gradient(low = "grey", high = "red", guide = 'none') + facet_wrap(~ DAY) + scale_alpha(guide = 'none') 

#density of crimes for IIT camp
miesMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, geom = "polygon", data=iitCampCrime) +
  scale_fill_gradient(low = "grey", high = "red", guide = 'none') + facet_wrap(~ DAY) + scale_alpha(guide = 'none')

#density of crimes for IIT area
miesAreaMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, geom = "polygon", data=iitAreaCrime) +
  scale_fill_gradient(low = "grey", high = "red", guide = 'none') + facet_wrap(~ DAY) + scale_alpha(guide = 'none')
#-----------------------------------------------------------

#iit density of crimes by time
#-----------------------------------------------------------
#density of crimes for IIT camp + area
miesAreaMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, geom = "polygon", data=iitCrime) +
  scale_fill_gradient(low = "grey", high = "red", guide = 'none') + facet_wrap(~ TIME_BUCKET) + scale_alpha(guide = 'none')

#density of crimes for IIT camp
miesMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, geom = "polygon", data=iitCampCrime) +
  scale_fill_gradient(low = "grey", high = "red", guide = 'none') + facet_wrap(~ TIME_BUCKET) + scale_alpha(guide = 'none')

#density of crimes for IIT area
miesAreaMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, geom = "polygon", data=iitAreaCrime) +
  scale_fill_gradient(low = "grey", high = "red", guide = 'none') + facet_wrap(~ TIME_BUCKET) + scale_alpha(guide = 'none')
#-----------------------------------------------------------


#iit campus crimes by month
#-----------------------------------------------------------
#density of crimes for IIT camp + area
miesAreaMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, geom = "polygon", data=iitCrime) +
  scale_fill_gradient(low = "grey", high = "red",  guide = 'none') + facet_wrap(~ MONTH) + scale_alpha(guide = 'none')

#density of crimes for IIT camp
miesMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, geom = "polygon", data=iitCampCrime) +
  scale_fill_gradient(low = "grey", high = "red",  guide = 'none') + facet_wrap(~ MONTH) + scale_alpha(guide = 'none')

#density of crimes for IIT area
miesAreaMap + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, geom = "polygon", data=iitAreaCrime) +
  scale_fill_gradient(low = "grey", high = "red",  guide = 'none') + facet_wrap(~ MONTH) + scale_alpha(guide = 'none')

#-----------------------------------------------------------


# #-----------------------------------------------------------
# #iit campus density of crimes - sector11
# iitCampus + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), bins = 10, data = iit[which(iit$SECTOR==11),], geom = "polygon") + scale_alpha(guide = 'none')
# 
# #plot sectors
# #-----------------------------------------------------------
# pointX <- c(41.8603, 41.8458, 41.8314, 41.81695, 41.8025, 41.8603, 41.8458, 41.8314, 41.81695, 41.8025, 41.8603, 41.8458, 41.8314, 41.81695, 41.8025, 41.8603, 41.8458, 41.8314, 41.81695, 41.8025, 41.8603, 41.8458, 41.8314, 41.81695, 41.8025)
# pointY <- c(-87.66687, -87.66687, -87.66687, -87.66687, -87.66687, -87.64705, -87.64705, -87.64705, -87.64705, -87.64705, -87.62723, -87.62723, -87.62723, -87.62723, -87.62723, -87.60741, -87.60741, -87.60741, -87.60741, -87.60741, -87.58759, -87.58759, -87.58759, -87.58759, -87.58759)
# pointN <- 1:25
# points <- data.frame(pointN,pointX, pointY)
# names(points) <- c("N", "lat", "lon")
# points$N <- factor(points$N)
# iitCampus14 + geom_point(aes(colour = N, size=3), data = points)
# iitCampusHyZ13 + geom_point(aes(size=3), data = points)
# #-----------------------------------------------------------
# 
