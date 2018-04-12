#write.csv(iitCleaned2,file = "iit_new.csv")
unique(iitCleaned2$Incident)
load(iitCleaned2) #use file iitCrime_0402 (1) in campus_safety folder
#write.xlsx(iitCleaned2$Incident, "iit_new.xlsx", append = T)


library(xlsx)

#creating a new column for new incident type
iitCleaned2$temp_inc <- NA

#using regex to match keywords to type of incident

#Medical incident

toMatch_med <- c("medical", "injury", "slip", "emergency", "transport") #add injured, fainted
matches_med <- grep(paste(toMatch_med, collapse = "|"), iitCleaned2$Notes, value = T)
match_med_ind <- match(matches_med,iitCleaned2$Notes)
match_med_ind <- unique(match_med_ind)
#Assigning incident type
iitCleaned2$temp_inc[match_med_ind] <- "MEDICAL INCIDENT"

#Narcotics incident

toMatch_narc <- c("marijuana", "narcotic", "drug")
matches_narc <- grep(paste(toMatch_narc,collapse = "|"),iitCleaned2$Notes, value = T)
match_narc_ind <- match(matches_narc,iitCleaned2$Notes)
match_narc_ind <- unique(match_narc_ind)
# to check if there is overlappling
sum(match_narc_ind %in% match_med_ind)
#Assigning incident type
iitCleaned2$temp_inc[match_narc_ind] <- "NARCOTICS"


#Robbery incidents

toMatch_rob <- c("robb")
matches_rob <- grep(paste(toMatch_rob, collapse = "|"), iitCleaned2$Notes, value = T)
match_rob_ind <- match(matches_rob,iitCleaned2$Notes)
match_rob_ind <- unique(match_rob_ind)
# to check if there is overlappling
sum(match_rob_ind %in% c(match_narc_ind,match_med_ind))
#Assigning incident type
iitCleaned2$temp_inc[match_rob_ind] <- "ROBBERY"

#Alarms - fire , smoke, trouble
toMatch_alarm <- c("alarm", "smoke", "fire")
matches_alarm <- grep(paste(toMatch_alarm, collapse = "|"), iitCleaned2$Notes, value = T)
match_alarm_ind <- match(matches_alarm,iitCleaned2$Notes)
match_alarm_ind <- unique(match_alarm_ind)
# to check if there is overlappling
which(match_alarm_ind %in% c(match_narc_ind,match_med_ind,match_rob_ind))

#to be removed
iitCleaned2[match_alarm_ind[c(41,58,184,205)],]
iitCleaned2[match_alarm_ind[c(81,182,99)],] #weapons violation
iitCleaned2[match_alarm_ind[c(11,51)],]
iitCleaned2[match_alarm_ind[c(76,80)],]
iitCleaned2[match_alarm_ind[c(143,198)],]
#removing
match_alarm_ind <- match_alarm_ind[-1*c(41,58,184,205,81,182,11,51,76,80,99,143,198)]

iitCleaned2$temp_inc[match_alarm_ind] <- "ALARM"


#Assault 

toMatch_assault <- c("assault")
matches_assault <- grep(paste(toMatch_assault, collapse = "|"), iitCleaned2$Notes, value = T)
match_assault_ind <- match(matches_assault,iitCleaned2$Notes)
match_assault_ind <- unique(match_assault_ind)


which(match_assault_ind %in% c(match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind))
#unique(iitCleaned2$Incident[match_assault_ind])
#iitCleaned2[match_assault_ind[c(which(iitCleaned2$Incident[match_assault_ind] == "battery-aggravated : hand/fists/feet w/aggravated injury"))],]
#to remove
iitCleaned2[match_assault_ind[c(5)],]
#removing 
match_assault_ind <- match_assault_ind[-1*c(5)]
iitCleaned2$temp_inc[match_assault_ind] <- "ASSAULT"

# Battery

toMatch_battery <- c("battery")
matches_battery <- grep(paste(toMatch_battery, collapse = "|"), iitCleaned2$Notes, value = T)
match_battery_ind <- match(matches_battery,iitCleaned2$Notes)
match_battery_ind <- unique(match_battery_ind)

which(match_battery_ind %in% c(match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_assault_ind))

match_battery_ind <- match_battery_ind[-1*c(2)]


iitCleaned2$temp_inc[match_battery_ind] <- "BATTERY"

# Theft, burglary

toMatch_theft <- c("theft","stolen","burglary","larceny")
matches_theft <- grep(paste(toMatch_theft, collapse = "|"), iitCleaned2$Notes, value = T)
match_theft_ind <- match(matches_theft,iitCleaned2$Notes)
match_theft_ind <- unique(match_theft_ind)


which(match_theft_ind %in% c(match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_assault_ind, match_battery_ind))

#to remove
iitCleaned2[match_theft_ind[c(66,155)],]

match_theft_ind <- match_theft_ind[-1*c(66,155)]

iitCleaned2$temp_inc[match_theft_ind] <- "THEFT"

#water,power,utility,elevator,entrapment,surge, electricity, gas

toMatch_util <- c("water","power","utility","elevator","entrapment","surge","electricity","gas")
matches_util <- grep(paste(toMatch_util, collapse = "|"), iitCleaned2$Notes, value = T)
match_util_ind <- match(matches_util,iitCleaned2$Notes)
match_util_ind <- unique(match_util_ind)


which(match_util_ind %in% c(match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_theft_ind,match_assault_ind, match_battery_ind))
#to Remove

iitCleaned2[match_util_ind[c(15,19,28,50,54)],]
iitCleaned2[match_util_ind[c(102)],]

match_util_ind <- match_util_ind[-1*c(15,19,28,50,54,102)]

iitCleaned2$temp_inc[match_util_ind] <- "UTILITY INCIDENT"


# Well being
toMatch_wellbeing <- c("well", "check", "friend", "worried")

matches_wellbeing <- grep(paste(toMatch_wellbeing, collapse = "|"), iitCleaned2$Notes, value = T)
match_wellbeing_ind <- match(matches_wellbeing,iitCleaned2$Notes)
match_wellbeing_ind <- unique(match_wellbeing_ind)

which(match_wellbeing_ind %in% c(match_util_ind,match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_theft_ind,match_assault_ind, match_battery_ind))


iitCleaned2[match_wellbeing_ind[c(4,6,9,11,15,16,18,19,23,27,28,34,35,36)],]

match_wellbeing_ind <- match_wellbeing_ind[-1*c(4,6,9,11,15,16,18,19,23,27,28,34,35,36)]


iitCleaned2$temp_inc[match_wellbeing_ind] <- "WELL BEING CHECK"






#disturbance, noise, complaint,conduct, disorderly, hazing, harassment(non-sexual)

toMatch_disturb <- c("disturbance", "noise", "complaint","disorderly","hazing")

matches_disturb <- grep(paste(toMatch_disturb, collapse = "|"), iitCleaned2$Notes, value = T)
match_disturb_ind <- match(matches_disturb,iitCleaned2$Notes)
match_disturb_ind <- unique(match_disturb_ind)

which(match_disturb_ind %in% c(match_wellbeing_ind,match_util_ind,match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_theft_ind,match_assault_ind, match_battery_ind))


iitCleaned2[match_disturb_ind[c(17,58)],]

match_disturb_ind <- match_disturb_ind[-1*c(17,58)]


iitCleaned2$temp_inc[match_disturb_ind] <- "DISTURBANCE"


#accident, vehicle

toMatch_accident <- c("accident", "vehicle", "hit and run")

matches_accident <- grep(paste(toMatch_accident, collapse = "|"), iitCleaned2$Notes, value = T)
match_accident_ind <- match(matches_accident,iitCleaned2$Notes)
match_accident_ind <- unique(match_accident_ind)

which(match_accident_ind %in% c(match_disturb_ind,match_wellbeing_ind,match_util_ind,match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_theft_ind,match_assault_ind, match_battery_ind))

iitCleaned2[match_accident_ind[c(2,6,18,20,24,26,28,34,35,36,41,44,48,53,61,65,69,73,83,87,88,91,100,104,111,113,118,121,128,129,132,137,140,142,144)],]
iitCleaned2[match_accident_ind[c(13,67,16,19,62,17,18,23,24,26,31,32,43,45,50,74,79,91,54,71,88,105)],]


match_accident_ind <- match_accident_ind[-1*c(2,6,18,20,24,26,28,34,35,36,41,44,48,53,61,65,69,73,83,87,88,91,100,104,111,113,118,121,128,129,132,137,140,142,144)]

match_accident_ind <- match_accident_ind[-1*c(13,67,16,19,62,17,18,23,24,26,31,32,43,45,50,74,79,91,54,71,88,105)]

iitCleaned2$temp_inc[match_accident_ind] <- "ACCIDENT"


#damage, property, vandal, window

toMatch_damage <- c("damage", "property", "vandal","window")

matches_damage <- grep(paste(toMatch_damage, collapse = "|"), iitCleaned2$Notes, value = T)
match_damage_ind <- match(matches_damage,iitCleaned2$Notes)
match_damage_ind <- unique(match_damage_ind)

which(match_damage_ind %in% c(match_accident_ind,match_disturb_ind,match_wellbeing_ind,match_util_ind,match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_theft_ind,match_assault_ind, match_battery_ind))

iitCleaned2[match_damage_ind[c(3,8,12,15,16,23,25,27,39,42,49,54,55,70,75,97,109,112,117)],]
iitCleaned2[match_damage_ind[c(1,46,5,14,35,73,23,44,51,74,53,55,57,104)],]



match_damage_ind <- match_damage_ind[-1*c(3,8,12,15,16,23,25,27,39,42,49,54,55,70,75,97,109,112,117)]
match_damage_ind <- match_damage_ind[-1*c(1,46,5,14,35,73,23,44,51,74,53,55,57,104)]

iitCleaned2$temp_inc[match_damage_ind] <- "DAMAGE TO PROPERTY"


#injured

toMatch_injury <- c("injur")

matches_injury <- grep(paste(toMatch_injury, collapse = "|"), iitCleaned2$Notes, value = T)
match_injury_ind <- match(matches_injury,iitCleaned2$Notes)
match_injury_ind <- unique(match_injury_ind)

which(match_injury_ind %in% c(match_damage_ind,match_accident_ind,match_disturb_ind,match_wellbeing_ind,match_util_ind,match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_theft_ind,match_assault_ind, match_battery_ind))

iitCleaned2[match_injury_ind[c(5,6,8,9,13,15,16,17,18,19,20,25,26,27,33,35,39,40,59,74)],]

match_injury_ind <- match_injury_ind[-1*c(5,6,8,9,13,15,16,17,18,19,20,25,26,27,33,35,39,40,59,74)]


iitCleaned2$temp_inc[match_injury_ind] <- "MEDICAL INCIDENT"


#trespass,enter,property

toMatch_trespass <- c("trespass","enter")

matches_trespass <- grep(paste(toMatch_trespass, collapse = "|"), iitCleaned2$Notes, value = T)
match_trespass_ind <- match(matches_trespass,iitCleaned2$Notes)
match_trespass_ind <- unique(match_trespass_ind)

which(match_trespass_ind %in% c(match_injury_ind,match_damage_ind,match_accident_ind,match_disturb_ind,match_wellbeing_ind,match_util_ind,match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_theft_ind,match_assault_ind, match_battery_ind))

iitCleaned2[match_trespass_ind[c(1,2,3,4,5,9,10,11,14,15,16,17,19,20,23,26,30,31,33,35,36,42,44,46,47,53,54,55,56,57,58,60,63)],]
iitCleaned2[match_trespass_ind[c(7,20,21,23,30)],]



match_trespass_ind <- match_trespass_ind[-1*c(1,2,3,4,5,9,10,11,14,15,16,17,19,20,23,26,30,31,33,35,36,42,44,46,47,53,54,55,56,57,58,60,63)]
match_trespass_ind <- match_trespass_ind[-1*c(7,20,21,23,30)]

iitCleaned2$temp_inc[match_trespass_ind] <- "TRESPASS"



#alcohol, intoxicated, (very few case look into at the end)

#toMatch_liquor <- c("alcohol","liquor")

#matches_liquor <- grep(paste(toMatch_liquor, collapse = "|"), iitCleaned2$Notes, value = T)
#match_liquor_ind <- match(matches_liquor,iitCleaned2$Notes)
#match_liquor_ind <- unique(match_liquor_ind)

#which(match_liquor_ind %in% c(match_trespass_ind,match_injury_ind,match_damage_ind,match_accident_ind,match_disturb_ind,match_wellbeing_ind,match_util_ind,match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_theft_ind,match_assault_ind, match_battery_ind))

#iitCleaned2[match_liquor_ind[c(3,8,12,14,15,18,19)],]

#match_liquor_ind <- match_liquor_ind[-1*c(3,8,12,14,15,18,19)]

``
#suspicious

toMatch_sus <- c("suspicious")

matches_sus <- grep(paste(toMatch_sus, collapse = "|"), iitCleaned2$Notes, value = T)
match_sus_ind <- match(matches_sus,iitCleaned2$Notes)
match_sus_ind <- unique(match_sus_ind)

which(match_sus_ind %in% c(match_trespass_ind,match_injury_ind,match_damage_ind,match_accident_ind,match_disturb_ind,match_wellbeing_ind,match_util_ind,match_narc_ind,match_med_ind,match_rob_ind,match_alarm_ind,match_theft_ind,match_assault_ind, match_battery_ind))

iitCleaned2[match_sus_ind[c(45)],]

match_sus_ind <- match_sus_ind[-1*c(45)]

iitCleaned2$temp_inc[match_sus_ind] <- "SUSPICION"


rec_manual <- which(is.na(iitCleaned2$temp_inc)) # record which were manually classified due to spelling mistakes or which were not captured.
##REMAINING HAD TO BE STANDARDIZED PHYSICALLY



write.csv(iitCleaned2,file = "iit_stand.csv")

# file after manual classification of incident types and addition of address and lat and long column with date standardized
load("D:/Github/CSP571/Project/campus_safety/iitCrime_with_add.rda")

iitCleaned_new <- iitCrime
