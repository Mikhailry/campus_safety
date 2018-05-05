setwd(gsub("\\\\","//", readClipboard()))
UC_Cleaned <- read.csv("Uchicago_campus_crimes_cleaned.csv")
UC_Cleaned <- UC_Cleaned[,-1]

#To create a table for standardizing incident type, execute only once.
#unique(UC_Cleaned$Incident)
#write.xlsx(UC_Cleaned$Incident, file = "merge_UC.xlsx")
library(xlsx)

merge_tab <- read.xlsx("merge_UC.xlsx", sheetIndex=1)


UC_stand <- merge(UC_Cleaned,merge_tab,by.x = "Incident", by.y = "x")

UC_mod <- UC_stand

inc_other <- which(UC_mod$Stand_inc == "OTHER")

#theft, burglary
toMatch_theft1 <- c("theft", "burglar")
matches_theft1 <- grep(paste(toMatch_theft1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_theft_ind1 <- match(matches_theft1,UC_mod$Comments...Nature.of.Fire)
match_theft_ind1 <- unique(match_theft_ind1)

UC_mod$Stand_inc[c(196, 147, 94, 91, 81, 80, 70, 66, 63, 59)] <- "THEFT"

inc_other <- which(UC_mod$Stand_inc == "OTHER")
#robbery, funds, fraudulent, Fraudulent, credit card

toMatch_rob1 <- c("robbery", "funds","fraudulent", "Fraudulent", "credit card")
matches_rob1 <- grep(paste(toMatch_rob1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_rob_ind1 <- match(matches_rob1,UC_mod$Comments...Nature.of.Fire)
match_rob_ind1 <- unique(match_rob_ind1)

UC_mod$Stand_inc[c(2915, 2644, 1907, 1413, 1106, 587, 583, 581, 579, 577, 578, 575, 85, 76)] <- "ROBBERY"

inc_other <- which(UC_mod$Stand_inc == "OTHER")

#disturbance

toMatch_dist1 <- c("disturbance", "noise", "complaint","disorderly","hazing")
matches_dist1 <- grep(paste(toMatch_dist1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_dist_ind1 <- match(matches_dist1,UC_mod$Comments...Nature.of.Fire)
match_dist_ind1 <- unique(match_dist_ind1)

UC_mod$Stand_inc[c(62, 1059, 1086, 1104, 1197, 1210, 1212, 1266, 1337, 1381, 1422, 1686, 4479)] <- "DISTURBANCE"

inc_other <- which(UC_mod$Stand_inc == "OTHER")

#trespass


toMatch_tres1 <- c("trespass","enter")
matches_tres1 <- grep(paste(toMatch_tres1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_tres_ind1 <- match(matches_tres1,UC_mod$Comments...Nature.of.Fire)
match_tres_ind1 <- unique(match_tres_ind1)

UC_mod$Stand_inc[c(1437, 1417, 1388, 1383, 1370, 1368, 1342, 1295,1288, 1288, 1271, 1261, 1245, 1243,1240,1216,1207,1204,1160,1132,1130,1093,1069,1057,1007,96,73)] <- "TRESPASS"

inc_other <- which(UC_mod$Stand_inc == "OTHER")

# accident, hit and run, hit & run, struck

toMatch_acc1 <- c("injur","medical", "injury", "slip", "emergency", "transport", "hit and run", "hit & run", "accident", "struck")
matches_acc1 <- grep(paste(toMatch_acc1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_acc_ind1 <- match(matches_acc1,UC_mod$Comments...Nature.of.Fire)
match_acc_ind1 <- unique(match_acc_ind1)

UC_mod$Stand_inc[c(4463,1357,1248,1219, 1149, 1105, 1048, 1031, 64, 3313, 1403, 1397, 1352,1314, 191)] <- "ACCIDENT"

UC_mod$Stand_inc[c(1423, 1416, 1398,1399,1379,1347,1311, 1305, 1302,1298,1287, 1274,1205, 1200,1190, 1183, 1154, 1147,1133,1127,1083,1079,1035,1015,1008,1006)]<- "MEDICAL INCIDENT"

inc_other <- which(UC_mod$Stand_inc == "OTHER")

# weapon, gun, rifle, ammunition

toMatch_weapon1 <- c("weapon", "gun", "rifle", "ammunition", "shot")
matches_weapon1 <- grep(paste(toMatch_weapon1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_weapon_ind1 <- match(matches_weapon1,UC_mod$Comments...Nature.of.Fire)
match_weapon_ind1 <- unique(match_weapon_ind1)

UC_mod$Stand_inc[c(1685,1441,1433,1411,1328,1208,1126,1089,1088,1023,964,944 ,934, 939, 897, 882,856,852,831, 811, 748, 735, 101, 98, 92, 90, 89, 79, 68, 67)] <- "WEAPON"

inc_other <- which(UC_mod$Stand_inc == "OTHER")

#well-being, well being


toMatch_wellbeing1 <- c("well-being", "well being")
matches_wellbeing1 <- grep(paste(toMatch_wellbeing1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_wellbeing_ind1 <- match(matches_wellbeing1,UC_mod$Comments...Nature.of.Fire)
match_wellbeing_ind1 <- unique(match_wellbeing_ind1)

UC_mod$Stand_inc[c(87, 139, 1021, 1054, 1109, 1117, 1170, 1198, 1221, 1235, 1410, 1894)] <- "WELL BEING CHECK"

inc_other <- which(UC_mod$Stand_inc == "OTHER")
#controlled substance, cannabis, narcotic, marijuana, drug, Drug

toMatch_narc1 <- c("controlled substance", "cannabis", "narcotic", "marijuana", "drug", "Drug")
matches_narc1 <- grep(paste(toMatch_narc1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_narc_ind1 <- match(matches_narc1,UC_mod$Comments...Nature.of.Fire)
match_narc_ind1 <- unique(match_narc_ind1)

UC_mod$Stand_inc[c(1942,1910 ,1407 ,1406 , 1346,1309 , 1286,1267 , 1203,1162 ,1159 ,1071, 1067, 1014, 954, 941, 929, 895, 857, 801, 769,757, 590, 756)] <- "NARCOTICS"

inc_other <- which(UC_mod$Stand_inc == "OTHER")

#alcohol, DUI, liquor, influence

toMatch_alc1 <- c("alcohol","DUI","liquor","influence")
matches_alc1 <- grep(paste(toMatch_alc1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_alc_ind1 <- match(matches_alc1,UC_mod$Comments...Nature.of.Fire)
match_alc_ind1 <- unique(match_alc_ind1)

spellchange <- which(UC_mod$Stand_inc == "LIQOUR LAW VIOLATION")
levels(UC_mod$Stand_inc) <- c(levels(UC_mod$Stand_inc), "LIQUOR LAW VIOLATION")
UC_mod$Stand_inc[c(spellchange)] <- "LIQUOR LAW VIOLATION"
UC_mod$Stand_inc <- factor(UC_mod$Stand_inc)

UC_mod$Stand_inc[c(match_alc_ind1)] <- "LIQUOR LAW VIOLATION"
inc_other <- which(UC_mod$Stand_inc == "OTHER")


#struck, battery,

toMatch_struck1 <- c("struck", "battery")
matches_struck1 <- grep(paste(toMatch_struck1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_struck_ind1 <- match(matches_struck1,UC_mod$Comments...Nature.of.Fire)
match_struck_ind1 <- unique(match_struck_ind1)

UC_mod$Stand_inc[c(1313, 1124, 1116)] <- "BATTERY"

inc_other <- which(UC_mod$Stand_inc == "OTHER")

# elevator, Elevator, water, Water, electricity, Electricity, leak, gas

toMatch_util1 <- c("elevator", "Elevator", "water", "Water", "electricity", "Electricity", "leak", "gas")
matches_util1 <- grep(paste(toMatch_util1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_util_ind1 <- match(matches_util1,UC_mod$Comments...Nature.of.Fire)
match_util_ind1 <- unique(match_util_ind1)

UC_mod$Stand_inc[c(1429, 1384, 1366, 1362, 1332, 1301, 1284, 1254, 1224,1199, 1195, 1169, 1157, 1155, 1112, 1096, 1063, 1062,1017, 1145)] <- "UTILITY INCIDENT"

inc_other <- which(UC_mod$Stand_inc == "OTHER")

#warrant, wanted

toMatch_warrant1 <- c("warrant", "wanted")
matches_warrant1 <- grep(paste(toMatch_warrant1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_warrant_ind1 <- match(matches_warrant1,UC_mod$Comments...Nature.of.Fire)
match_warrant_ind1 <- unique(match_warrant_ind1)



#harass,Annoy,annoy,dispute,
#Damage, damage



#inventoried, Inventoried, safekeeping, turned over, Turned over, lost, losing, wallet, found

toMatch_inv1 <- c("inventoried", "Inventoried", "safekeeping", "turned over", "Turned over","lost", "losing", "wallet", "found")
matches_inv1 <- grep(paste(toMatch_inv1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_inv_ind1 <- match(matches_inv1,UC_mod$Comments...Nature.of.Fire)
match_inv_ind1 <- unique(match_inv_ind1)


#recovered, stolen, discovered, license plate reader, License Plate Reader, unoccupied, sticker


toMatch_rec1 <- c("recovered", "stolen", "discovered", "license plate reader", "License Plate Reader", "unoccupied", "sticker")
matches_rec1 <- grep(paste(toMatch_rec1, collapse = "|"), UC_mod$Comments...Nature.of.Fire[inc_other], value = T)
match_rec_ind1 <- match(matches_rec1,UC_mod$Comments...Nature.of.Fire)
match_rec_ind1 <- unique(match_rec_ind1)

temp <- unique(c(match_rec_ind1,match_inv_ind1,match_warrant_ind1))
UC_mod[inc_other[!inc_other %in% temp],c("Stand_inc","Comments...Nature.of.Fire")]

#OTHER category includes warrant arrest, lost and found, finding stolen objects(cars, etc.)

#write.csv(UC_mod,"UC_stand.csv")


#######

