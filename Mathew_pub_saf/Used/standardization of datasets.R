## STANDARDIZING IIT DATA TO SAME FORMAT FOR PREDICTION

IIT_camp <- read.csv("D:/Github/CSP571/Project/campus_safety/IIT_Crime_to_finstand.csv")
IIT_area <- read.csv("D:/Github/CSP571/Project/campus_safety/Mathew_pub_saf/IITarea_ALL_STAND.csv")


#Adding new column for identification from campus data or crime portal data
IIT_camp$TYPE_OF_DATA <- "IIT-CAMPUS"
IIT_area$TYPE_OF_DATA <- "IIT-AREA"

#retaining necessary columns
IIT_camp <- IIT_camp[,c(3,4,7,10,12,13,14)]

#removing missing location data and other data which analysis is not being performed on.
IIT_camp <- IIT_camp[-1*c(which(is.na(IIT_camp$Latitude))),]
IIT_camp <- IIT_camp[-1*c(grep("201 e loop rd, wheaton, il",IIT_camp$Address)),]
IIT_camp <- IIT_camp[-1*c(grep("6502 south archer rd. bedford park, il",IIT_camp$Address)),]

#renaming columns
colnames(IIT_camp) <- c("LOCATION", "ADDRESS", "OCCURED","INCIDENT_TYPE1","LATITUDE","LONGITUDE","TYPE_OF_DATA")

#IIT_camp$OCCURED <- as.POSIXct(IIT_camp$OCCURED, format = "%Y-%m-%d %H:%M", tz = "America/Chicago")

#retaining columns in IIT data from crime data portal
IIT_area <- IIT_area[,c(3,4,6,10,11,13,14)]
colnames(IIT_area) <- c("OCCURED","ADDRESS","LOCATION","LATITUDE","LONGITUDE","INCIDENT_TYPE1","TYPE_OF_DATA")

#reordering columns so as to bind both data
IIT_area <- IIT_area[,c(3,2,1,6,4,5,7)]

#IIT_area$OCCURED <- as.POSIXct(IIT_camp$OCCURED, format = "%Y-%m-%d %H:%M", tz = "America/Chicago")
#binding both data frames to single
IIT_FINAL <- rbind(IIT_area,IIT_camp)

IIT_FINAL$OCCURED <- as.POSIXct(IIT_FINAL$OCCURED, format = "%Y-%m-%d %H:%M", tz = "America/Chicago")
#creating new column for reclassification of incidents into buckets of similar crimes
IIT_FINAL$INCIDENT_TYPE2 <- NA 

#grouping liqour and narcotics to single bucket
substance <- c("LIQUOR LAW VIOLATION", "NARCOTICS")
match_sub_ind <- which(IIT_FINAL$INCIDENT_TYPE1 == "LIQUOR LAW VIOLATION"|IIT_FINAL$INCIDENT_TYPE1 == "NARCOTICS")

IIT_FINAL$INCIDENT_TYPE2[match_sub_ind] <- "SUBSTANCE CRIME" 

#grouping non criminal incidents(very little severe incident)
noncrim <- c("OTHER","UTILITY INCIDENT","MEDICAL INCIDENT","ALARM","WELL BEING CHECK","SUSPICION")
match_noncrim_ind <- which(IIT_FINAL$INCIDENT_TYPE1 == "OTHER"|IIT_FINAL$INCIDENT_TYPE1 == "UTILITY INCIDENT"|IIT_FINAL$INCIDENT_TYPE1 == "MEDICAL INCIDENT"|IIT_FINAL$INCIDENT_TYPE1 == "ALARM"|IIT_FINAL$INCIDENT_TYPE1 == "WELL BEING CHECK"|IIT_FINAL$INCIDENT_TYPE1 == "SUSPICION")

IIT_FINAL$INCIDENT_TYPE2[match_noncrim_ind] <- "NON CRIMINAL" 

#grouping serious crimes together
sercrim <- c("OFFENSE INVOLVING CHILDREN","SEXUAL CRIME","KIDNAPPING","HOMICIDE","WEAPON","MISSING PERSON")
match_sercrim_ind <- which(IIT_FINAL$INCIDENT_TYPE1 == "OFFENSE INVOLVING CHILDREN"|IIT_FINAL$INCIDENT_TYPE1 == "SEXUAL CRIME"|IIT_FINAL$INCIDENT_TYPE1 == "KIDNAPPING"|IIT_FINAL$INCIDENT_TYPE1 == "HOMICIDE"|IIT_FINAL$INCIDENT_TYPE1 == "WEAPON"|IIT_FINAL$INCIDENT_TYPE1 == "MISSING PERSON")

IIT_FINAL$INCIDENT_TYPE2[match_sercrim_ind] <- "SERIOUS CRIME" 

#group theft, robbery,etc, as person crimes
perscrim <- c("ASSAULT","BATTERY", "THEFT","STALKING","ROBBERY","ACCIDENT")
match_percrim_ind <- which(IIT_FINAL$INCIDENT_TYPE1 == "ASSAULT"|IIT_FINAL$INCIDENT_TYPE1 == "BATTERY"|IIT_FINAL$INCIDENT_TYPE1 == "THEFT"|IIT_FINAL$INCIDENT_TYPE1 == "STALKING"|IIT_FINAL$INCIDENT_TYPE1 == "ROBBERY"|IIT_FINAL$INCIDENT_TYPE1 == "ACCIDENT")

IIT_FINAL$INCIDENT_TYPE2[match_percrim_ind] <- "PERSON CRIME" 

#grouping property damage, disturbance, etc as property crime
propcrim <- c("ARSON","DAMAGE TO PROPERTY","DISTURBANCE","TRESPASS")
match_propcrim_ind <- which(IIT_FINAL$INCIDENT_TYPE1 == "ARSON"|IIT_FINAL$INCIDENT_TYPE1 == "DAMAGE TO PROPERTY"|IIT_FINAL$INCIDENT_TYPE1 == "DISTURBANCE"|IIT_FINAL$INCIDENT_TYPE1 == "TRESPASS")

IIT_FINAL$INCIDENT_TYPE2[match_propcrim_ind] <- "PROPERTY CRIME" 




####     UC 
library(lubridate)
# getting in data 
UC_temp <- read.csv("UC_stand.csv")
UC_LATLONG_merge <- read.csv("Address_dic_UC_with_lat_long.csv")

# combining the lat_long data by merging on location.address
UC_temp <- merge(UC_temp,UC_LATLONG_merge,by.x = "Location.Address",by.y = "Original.address")

# converting time to posixct time
trial <- UC_temp$Occured.Start.Date
trial <- as.data.frame(trial)
trial$time <- UC_temp$Occured.Start.Time

colnames(trial)[1] <- "date"

trial$date_time <- paste(trial$date," ",trial$time)

trial$pos <- strptime(trial$date_time, "%m/%d/%Y %H:%M")

UC_temp$OCCURED <- trial$pos

# retaining only required columns

UC_temp <- UC_temp[,c(1,4,14,16,17,18)]

#labelling the data 

UC_temp$TYPE_OF_DATA <- "UC-CAMPUS"

#renaming the columns for standardization
colnames(UC_temp) <- c("ADDRESS","LOCATION","INCIDENT_TYPE1","LATITUDE","LONGITUDE","OCCURED","TYPE_OF_DATA")

#rearranging columns for standardization
UC_camp <- UC_temp[,c(2,1,6,3,4,5,7)]

#identifying columns with missing values
colnames(UC_camp)[colSums(is.na(UC_camp))>0]

#identifying rows with missing values 
rem <- unique(c(which(is.na(UC_camp$LATITUDE)),which(is.na(UC_camp$OCCURED))))

#removing missing value rows
UC_camp <- UC_camp[-1*c(rem),]

UC_camp$OCCURED <- as.POSIXct(UC_camp$OCCURED)

#reading in UC area data
UC_area <- read.csv("D:/Github/CSP571/Project/campus_safety/Mathew_pub_saf/UCarea_STAND.csv")


#retaining required columns
UC_area <- UC_area[,c(3,4,6,10,11,13)]

#categorizing from where the data was got(crime portal)
UC_area$TYPE_OF_DATA <- "UC-AREA"

#renaming columns
colnames(UC_area) <- c("OCCURED","ADDRESS","LOCATION","LATITUDE","LONGITUDE","INCIDENT_TYPE1","TYPE_OF_DATA")

#rearranging columns 
UC_area <- UC_area[,c(3,2,1,6,4,5,7)]

UC_area$OCCURED <- as.POSIXct(UC_area$OCCURED, format = "%Y-%m-%d %H:%M", tz = "America/Chicago")

#converting factor columns to character for binding of dataframaes
conv <- sapply(UC_area,is.factor)
conv1 <- sapply(UC_camp,is.factor)

UC_area[conv] <- lapply(UC_area[conv],as.character)

UC_camp[conv1] <- lapply(UC_camp[conv1],as.character)

UC_FINAL <- rbind(UC_area,UC_camp)

UC_FINAL[which(UC_FINAL$INCIDENT_TYPE1 == "SEXUAL ASSAULT"),"INCIDENT_TYPE1"] <- "SEXUAL CRIME"


UC_FINAL$INCIDENT_TYPE2 <- NA 

#grouping liqour and narcotics to single bucket
substance1 <- c("LIQUOR LAW VIOLATION", "NARCOTICS")
match_sub_ind1 <- which(UC_FINAL$INCIDENT_TYPE1 == "LIQUOR LAW VIOLATION"|UC_FINAL$INCIDENT_TYPE1 == "NARCOTICS")

UC_FINAL$INCIDENT_TYPE2[match_sub_ind1] <- "SUBSTANCE CRIME" 

#grouping non criminal incidents(very little severe incident)
noncrim1 <- c("OTHER","UTILITY INCIDENT","MEDICAL INCIDENT","ALARM","WELL BEING CHECK","SUSPICION")
match_noncrim_ind1 <- which(UC_FINAL$INCIDENT_TYPE1 == "OTHER"|UC_FINAL$INCIDENT_TYPE1 == "UTILITY INCIDENT"|UC_FINAL$INCIDENT_TYPE1 == "MEDICAL INCIDENT"|UC_FINAL$INCIDENT_TYPE1 == "ALARM"|UC_FINAL$INCIDENT_TYPE1 == "WELL BEING CHECK"|UC_FINAL$INCIDENT_TYPE1 == "SUSPICION")

UC_FINAL$INCIDENT_TYPE2[match_noncrim_ind1] <- "NON CRIMINAL" 

#grouping serious crimes together
sercrim1 <- c("OFFENSE INVOLVING CHILDREN","SEXUAL CRIME","KIDNAPPING","HOMICIDE","WEAPON")
match_sercrim_ind1 <- which(UC_FINAL$INCIDENT_TYPE1 == "OFFENSE INVOLVING CHILDREN"|UC_FINAL$INCIDENT_TYPE1 == "SEXUAL CRIME"|UC_FINAL$INCIDENT_TYPE1 == "KIDNAPPING"|UC_FINAL$INCIDENT_TYPE1 == "HOMICIDE"|UC_FINAL$INCIDENT_TYPE1 == "WEAPON"|UC_FINAL$INCIDENT_TYPE1 == "MISSING PERSON")

UC_FINAL$INCIDENT_TYPE2[match_sercrim_ind1] <- "SERIOUS CRIME" 

#group theft, robbery,etc, as person crimes
perscrim1 <- c("ASSAULT","BATTERY", "THEFT","STALKING","ROBBERY","ACCIDENT")
match_percrim_ind1 <- which(UC_FINAL$INCIDENT_TYPE1 == "ASSAULT"|UC_FINAL$INCIDENT_TYPE1 == "BATTERY"|UC_FINAL$INCIDENT_TYPE1 == "THEFT"|UC_FINAL$INCIDENT_TYPE1 == "STALKING"|UC_FINAL$INCIDENT_TYPE1 == "ROBBERY"|UC_FINAL$INCIDENT_TYPE1 == "ACCIDENT")

UC_FINAL$INCIDENT_TYPE2[match_percrim_ind1] <- "PERSON CRIME" 

#grouping property damage, disturbance, etc as property crime
propcrim1 <- c("ARSON","DAMAGE TO PROPERTY","DISTURBANCE","TRESPASS")
match_propcrim_ind1 <- which(UC_FINAL$INCIDENT_TYPE1 == "ARSON"|UC_FINAL$INCIDENT_TYPE1 == "DAMAGE TO PROPERTY"|UC_FINAL$INCIDENT_TYPE1 == "DISTURBANCE"|UC_FINAL$INCIDENT_TYPE1 == "TRESPASS")

UC_FINAL$INCIDENT_TYPE2[match_propcrim_ind1] <- "PROPERTY CRIME" 



#splitting of data into quadrants

#   UC

UC_FINAL$SECTOR <- NA

grid1 <- which((UC_FINAL$LATITUDE >= 41.7606) & (UC_FINAL$LATITUDE < 41.77509) & (UC_FINAL$LONGITUDE >= -87.64057) & (UC_FINAL$LONGITUDE < -87.62075))
UC_FINAL[grid1,"SECTOR"] <- 1

grid2 <- which((UC_FINAL$LATITUDE >= 41.7606) & (UC_FINAL$LATITUDE < 41.77509) & (UC_FINAL$LONGITUDE >= -87.62075) & (UC_FINAL$LONGITUDE < -87.60093))
UC_FINAL[grid2,"SECTOR"] <- 2

grid3 <- which((UC_FINAL$LATITUDE >= 41.7606) & (UC_FINAL$LATITUDE < 41.77509) & (UC_FINAL$LONGITUDE >= -87.60093) & (UC_FINAL$LONGITUDE < -87.58111))
UC_FINAL[grid3,"SECTOR"] <- 3

grid4 <- which((UC_FINAL$LATITUDE >= 41.7606) & (UC_FINAL$LATITUDE < 41.77509) & (UC_FINAL$LONGITUDE >= -87.58111) & (UC_FINAL$LONGITUDE <= -87.56129))
UC_FINAL[grid4,"SECTOR"] <- 4

grid5 <- which((UC_FINAL$LATITUDE >= 41.77509) & (UC_FINAL$LATITUDE < 41.78958) & (UC_FINAL$LONGITUDE >= -87.64057) & (UC_FINAL$LONGITUDE < -87.62075))
UC_FINAL[grid5,"SECTOR"] <- 5

grid6 <- which((UC_FINAL$LATITUDE >= 41.77509) & (UC_FINAL$LATITUDE < 41.78958) & (UC_FINAL$LONGITUDE >= -87.62075) & (UC_FINAL$LONGITUDE < -87.60093))
UC_FINAL[grid6,"SECTOR"] <- 6

grid7 <- which((UC_FINAL$LATITUDE >= 41.77509) & (UC_FINAL$LATITUDE < 41.78958) & (UC_FINAL$LONGITUDE >= -87.60093) & (UC_FINAL$LONGITUDE < -87.58111))
UC_FINAL[grid7,"SECTOR"] <- 7

grid8 <- which((UC_FINAL$LATITUDE >= 41.77509) & (UC_FINAL$LATITUDE < 41.78958) & (UC_FINAL$LONGITUDE >= -87.58111) & (UC_FINAL$LONGITUDE <= -87.56129))
UC_FINAL[grid8,"SECTOR"] <- 8

grid9 <- which((UC_FINAL$LATITUDE >= 41.78958) & (UC_FINAL$LATITUDE < 41.80407) & (UC_FINAL$LONGITUDE >= -87.64057) & (UC_FINAL$LONGITUDE < -87.62075))
UC_FINAL[grid9,"SECTOR"] <- 9

grid10 <- which((UC_FINAL$LATITUDE >= 41.78958) & (UC_FINAL$LATITUDE < 41.80407) & (UC_FINAL$LONGITUDE >= -87.62075) & (UC_FINAL$LONGITUDE < -87.60093))
UC_FINAL[grid10,"SECTOR"] <- 10

grid11 <- which((UC_FINAL$LATITUDE >= 41.78958) & (UC_FINAL$LATITUDE < 41.80407) & (UC_FINAL$LONGITUDE >= -87.60093) & (UC_FINAL$LONGITUDE < -87.58111))
UC_FINAL[grid11,"SECTOR"] <- 11

grid12 <- which((UC_FINAL$LATITUDE >= 41.78958) & (UC_FINAL$LATITUDE < 41.80407) & (UC_FINAL$LONGITUDE >= -87.58111) & (UC_FINAL$LONGITUDE <= -87.56129))
UC_FINAL[grid12,"SECTOR"] <- 12

grid13 <- which((UC_FINAL$LATITUDE >= 41.80407) & (UC_FINAL$LATITUDE <= 41.81856) & (UC_FINAL$LONGITUDE >= -87.64057) & (UC_FINAL$LONGITUDE < -87.62075))
UC_FINAL[grid13,"SECTOR"] <- 13

grid14 <- which((UC_FINAL$LATITUDE >= 41.80407) & (UC_FINAL$LATITUDE <= 41.81856) & (UC_FINAL$LONGITUDE >= -87.62075) & (UC_FINAL$LONGITUDE < -87.60093))
UC_FINAL[grid14,"SECTOR"] <- 14

grid15 <- which((UC_FINAL$LATITUDE >= 41.80407) & (UC_FINAL$LATITUDE <= 41.81856) & (UC_FINAL$LONGITUDE >= -87.60093) & (UC_FINAL$LONGITUDE < -87.58111))
UC_FINAL[grid15,"SECTOR"] <- 15

grid16 <- which((UC_FINAL$LATITUDE >= 41.80407) & (UC_FINAL$LATITUDE <= 41.81856) & (UC_FINAL$LONGITUDE >= -87.58111) & (UC_FINAL$LONGITUDE <= -87.56129))
UC_FINAL[grid16,"SECTOR"] <- 16


## IIT

IIT_FINAL$SECTOR <- NA

grid_1 <- which((IIT_FINAL$LATITUDE >= 41.8025) & (IIT_FINAL$LATITUDE < 41.81695) & (IIT_FINAL$LONGITUDE >= -87.66687) & (IIT_FINAL$LONGITUDE < -87.64705))
IIT_FINAL[grid_1,"SECTOR"] <- 1

grid_2 <- which((IIT_FINAL$LATITUDE >= 41.8025) & (IIT_FINAL$LATITUDE < 41.81695) & (IIT_FINAL$LONGITUDE >= -87.64705) & (IIT_FINAL$LONGITUDE < -87.62723))
IIT_FINAL[grid_2,"SECTOR"] <- 2

grid_3 <- which((IIT_FINAL$LATITUDE >= 41.8025) & (IIT_FINAL$LATITUDE < 41.81695) & (IIT_FINAL$LONGITUDE >= -87.62723) & (IIT_FINAL$LONGITUDE < -87.60741))
IIT_FINAL[grid_3,"SECTOR"] <- 3

grid_4 <- which((IIT_FINAL$LATITUDE >= 41.8025) & (IIT_FINAL$LATITUDE < 41.81695) & (IIT_FINAL$LONGITUDE >= -87.60741) & (IIT_FINAL$LONGITUDE <= -87.58759))
IIT_FINAL[grid_4,"SECTOR"] <- 4

grid_5 <- which((IIT_FINAL$LATITUDE >= 41.81695) & (IIT_FINAL$LATITUDE < 41.8314) & (IIT_FINAL$LONGITUDE >= -87.66687) & (IIT_FINAL$LONGITUDE < -87.64705))
IIT_FINAL[grid_5,"SECTOR"] <- 5

grid_6 <- which((IIT_FINAL$LATITUDE >= 41.81695) & (IIT_FINAL$LATITUDE < 41.8314) & (IIT_FINAL$LONGITUDE >= -87.64705) & (IIT_FINAL$LONGITUDE < -87.62723))
IIT_FINAL[grid_6,"SECTOR"] <- 6

grid_7 <- which((IIT_FINAL$LATITUDE >= 41.81695) & (IIT_FINAL$LATITUDE < 41.8314) & (IIT_FINAL$LONGITUDE >= -87.62723) & (IIT_FINAL$LONGITUDE < -87.60741))
IIT_FINAL[grid_7,"SECTOR"] <- 7

grid_8 <- which((IIT_FINAL$LATITUDE >= 41.81695) & (IIT_FINAL$LATITUDE < 41.8314) & (IIT_FINAL$LONGITUDE >= -87.60741) & (IIT_FINAL$LONGITUDE <= -87.58759))
IIT_FINAL[grid_8,"SECTOR"] <- 8

grid_9 <- which((IIT_FINAL$LATITUDE >= 41.8314) & (IIT_FINAL$LATITUDE < 41.8458) & (IIT_FINAL$LONGITUDE >= -87.66687) & (IIT_FINAL$LONGITUDE < -87.64705))
IIT_FINAL[grid_9,"SECTOR"] <- 9

grid_10 <- which((IIT_FINAL$LATITUDE >= 41.8314) & (IIT_FINAL$LATITUDE < 41.8458) & (IIT_FINAL$LONGITUDE >= -87.64705) & (IIT_FINAL$LONGITUDE < -87.62723))
IIT_FINAL[grid_10,"SECTOR"] <- 10

grid_11 <- which((IIT_FINAL$LATITUDE >= 41.8314) & (IIT_FINAL$LATITUDE < 41.8458) & (IIT_FINAL$LONGITUDE >= -87.62723) & (IIT_FINAL$LONGITUDE < -87.60741))
IIT_FINAL[grid_11,"SECTOR"] <- 11

grid_12 <- which((IIT_FINAL$LATITUDE >= 41.8314) & (IIT_FINAL$LATITUDE < 41.8458) & (IIT_FINAL$LONGITUDE >= -87.60741) & (IIT_FINAL$LONGITUDE <= -87.58759))
IIT_FINAL[grid_12,"SECTOR"] <- 12

grid_13 <- which((IIT_FINAL$LATITUDE >= 41.8458) & (IIT_FINAL$LATITUDE <= 41.8603) & (IIT_FINAL$LONGITUDE >= -87.66687) & (IIT_FINAL$LONGITUDE < -87.64705))
IIT_FINAL[grid_13,"SECTOR"] <- 13

grid_14 <- which((IIT_FINAL$LATITUDE >= 41.8458) & (IIT_FINAL$LATITUDE <= 41.8603) & (IIT_FINAL$LONGITUDE >= -87.64705) & (IIT_FINAL$LONGITUDE < -87.62723))
IIT_FINAL[grid_14,"SECTOR"] <- 14

grid_15 <- which((IIT_FINAL$LATITUDE >= 41.8458) & (IIT_FINAL$LATITUDE <= 41.8603) & (IIT_FINAL$LONGITUDE >= -87.62723) & (IIT_FINAL$LONGITUDE < -87.60741))
IIT_FINAL[grid_15,"SECTOR"] <- 15

grid_16 <- which((IIT_FINAL$LATITUDE >= 41.8458) & (IIT_FINAL$LATITUDE <= 41.8603) & (IIT_FINAL$LONGITUDE >= -87.60741) & (IIT_FINAL$LONGITUDE <= -87.58759))
IIT_FINAL[grid_16,"SECTOR"] <- 16

grid_17 <- which((IIT_FINAL$LATITUDE >= 41.86465) & (IIT_FINAL$LATITUDE < 41.87914) & (IIT_FINAL$LONGITUDE >= -87.66203) & (IIT_FINAL$LONGITUDE < -87.6422))
IIT_FINAL[grid_17,"SECTOR"] <- 17

grid_18 <- which((IIT_FINAL$LATITUDE >= 41.86465) & (IIT_FINAL$LATITUDE < 41.87914) & (IIT_FINAL$LONGITUDE >= -87.6422) & (IIT_FINAL$LONGITUDE <= -87.62239))
IIT_FINAL[grid_18,"SECTOR"] <- 18

grid_19 <- which((IIT_FINAL$LATITUDE >= 41.87914) & (IIT_FINAL$LATITUDE <= 41.89363) & (IIT_FINAL$LONGITUDE >= -87.66203) & (IIT_FINAL$LONGITUDE < -87.6422))
IIT_FINAL[grid_19,"SECTOR"] <- 19

grid_20 <- which((IIT_FINAL$LATITUDE >= 41.87914) & (IIT_FINAL$LATITUDE <= 41.89363) & (IIT_FINAL$LONGITUDE >= -87.6422) & (IIT_FINAL$LONGITUDE <= -87.62239))
IIT_FINAL[grid_20,"SECTOR"] <- 20

head(IIT_FINAL)
head(UC_FINAL)


write.csv(UC_FINAL, "UC_FINAL_AGG.csv")
write.csv(IIT_FINAL,"IIT_FINAL_AGG.csv")






