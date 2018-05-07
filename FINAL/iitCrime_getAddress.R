#load libraries
library('tidyr')
library('stringr')
library('rebus')
library('qdapRegex')
library('dplyr')


#load cleaned iitCrime data 'iitCrime'
load('iitCrime.rda')

#loading dictionaries of IIT buildings, parkings
iitDictBld <- read.csv('iit_dict_buildings.csv')
iitDictLots <- read.csv('iit_dict_parkings.csv')
iitDictNS <- read.csv('iit_dict_ns_streets.csv')

#removing multiple spaces between words in 'Location' and 'Notes'
iitCrime$Location <- rm_white(iitCrime$Location)
iitCrime$Notes <- rm_white(iitCrime$Notes)

#trimming spaces from both ends
iitDictBld <- sapply(iitDictBld, str_trim)
iitDictLots <- sapply(iitDictLots, str_trim)
iitDictNS <- sapply(iitDictNS, str_trim)

#converting to lower case
iitDictBld <- as.data.frame(tolower(iitDictBld), stringsAsFactors = F)
iitDictLots <- as.data.frame(tolower(iitDictLots), stringsAsFactors = F)
iitDictNS <- as.data.frame(tolower(iitDictNS), stringsAsFactors = F)

#add new columns Address, Latitude, Longitude
newColumns <- c("Address", "Latitude", "Longitude")
iitCrime[,newColumns] <- NA
#rearrange columns with select function of dplyr package
iitCrime <- select(iitCrime, Incident, Location, Address, Latitude, Longitude, Occured:Notes)

#----------------------------------------------------------------

#1 function to match iit building name and address
#takes as input an iit building from the dictionary
#search for non-strickt match for that building in the location column
#writes address of the building in the Address column for the rows with match
lookupBld <- function(building){
  iitCrime$Address[which(!is.na(str_match(iitCrime$Location, building)))] <<- iitDictBld$address[match(building, iitDictBld$building)]  
}

#search for buildings in location with dictionary
sapply(iitDictBld$building, lookupBld)

#observations still in search for address
naAddress <- iitCrime[which(is.na(iitCrime$Address)),]

#after the 1st search for address:
#matched: 1038 of 1604 obs
#not matched: 566 of 1604 obs
#----------------------------------------------------------------

#2 search for parking lots and CTA stations with dic
#function to match iit parking lots and near CTA stations and address
#takes as input a parking lot name or CTA station location from the dictionary
#search for non-strickt match for that parking/CTA location in the location column
#writes address of the parking lot/CTA location in the Address column for the rows with match,
#as well as Latitude and Longitude
#!!!parking lot address most likely will show you intersection,
#In contrary provided latitude/longitude shows the center of parking lot/CTA station
lookupLot <- function(parking){
  iitCrime$Address[which(!is.na(str_match(iitCrime$Location, parking)))] <<- iitDictLots$address[match(parking, iitDictLots$parking)]
  iitCrime$Latitude[which(!is.na(str_match(iitCrime$Location, parking)))] <<- iitDictLots$Latitude[match(parking, iitDictLots$parking)]
  iitCrime$Longitude[which(!is.na(str_match(iitCrime$Location, parking)))] <<- iitDictLots$Longitude[match(parking, iitDictLots$parking)]
}
#2 search for parkings in location with dictionary
sapply(iitDictLots$parking, lookupLot)

#observations still in search for address
naAddress <- iitCrime[which(is.na(iitCrime$Address)),]

#after the 2nd search for address:
#matched: 1127 of 1604 obs
#not matched: 477 of 1604 obs
#----------------------------------------------------------------

#3 search for buildings in column 'notes'
#function to match iit building name and address
#takes as input an iit building from the dictionary and iitCrime df
#search for non-strickt match for that building in the notes column
#writes address of the building in the Address column for the rows with match
lookupBldInNotes <- function(building, iitCrime){
  
  #building <- 'hermann'
  #find indices of matching elements
  ind <- which(!is.na(str_match(iitCrime$Notes, building)))
  #choose indices with empty address value
  ind2 <- is.na(iitCrime$Address[ind])
  ind3<-ind*ind2
  ind4<-ind3[ind3!=0]
  
  iitCrime$Address[ind4] <<- iitDictBld$address[match(building, iitDictBld$building)]
}

#search for buildings in notes with dictionary
sapply(iitDictBld$building, lookupBldInNotes, iitCrime)

#observations still in search for address
naAddress <- iitCrime[which(is.na(iitCrime$Address)),]

#after the 3rd search for address:
#matched: 1252 of 1604 obs
#not matched: 352 of 1604 obs
#----------------------------------------------------------------

#4 search for parking lots in column 'notes'
#function to match iit parking lots and address
#takes as input a parking lot name from the dictionary
#search for non-strickt match for that parking in the location column
#writes address of the parking lot in the Address column for the rows with match
#!!!parking lot address most likely will show you intersection,
#In contrary provided latitude/longitude shows the center of parking lot
lookupLotsInNotes <- function(parking, iitCrime){
  
  #find indices of matching elements
  ind <- which(!is.na(str_match(iitCrime$Notes, parking)))
  #choose indices with empty address value
  ind2 <- is.na(iitCrime$Address[ind])
  ind3<-ind*ind2
  ind4<-ind3[ind3!=0]
  
  iitCrime$Address[ind4] <<- iitDictLots$address[match(parking, iitDictLots$parking)]
  iitCrime$Latitude[ind4] <<- iitDictLots$Latitude[match(parking, iitDictLots$parking)]
  iitCrime$Longitude[ind4] <<- iitDictLots$Longitude[match(parking, iitDictLots$parking)]
}

#search for buildings in notes with dictionary
sapply(iitDictLots$parking, lookupLotsInNotes, iitCrime)

#observations still in search for address
naAddress <- iitCrime[which(is.na(iitCrime$Address)),]

#after the 4th search for address:
#matched: 1258 of 1604 obs
#not matched: 346 of 1604 obs
#----------------------------------------------------------------

#5 search with regex for address in location
#search for address of type '3424 s michigan', replace 's' with 'south'
#and add address value for instances with empty address

#first, defining pattern
pat1 <- dgt(4) %R% zero_or_more(SPC) %R% optional(or('s','south')) %R% zero_or_more(SPC) %R% one_or_more(WRD)
#find indices of matching elements
ind <- which(!is.na(str_match(iitCrime$Location, pat1)))
iitCrime$Location[ind]
#choose indices with empty address value
ind2 <- is.na(iitCrime$Address[ind])
ind3<-ind*ind2
ind4<-ind3[ind3!=0]

#vector of values extracted from location with empty address field
extr <- str_match(iitCrime$Location, pat1)[ind4]
#replace 's' standing for 'south', as address '3424 s michigan' won't work in comparison to '3424 south michigan'
extr_mod <- str_replace(extr," s ", " south ")
#write values to data frame
iitCrime$Address[ind4] <- extr_mod

#observations still in search for address
naAddress <- iitCrime[which(is.na(iitCrime$Address)),]

#after the 5th search for address:
#matched: 1322 of 1604 obs
#not matched: 282 of 1604 obs
#----------------------------------------------------------------
#6 search for street intersections in Location and Notes columns

#first adding to temporary column for matched streets going from west to east (31st street)
#and column for streets going from north to south that are crossing the campus or in close 
#proximity to it. For streets goint N to S a dictionary will be used
tempColumns <- c("WEstreets_loc", "WEstreets_not", "NSstreets_loc", "NSstreets_not")
iitCrime[,tempColumns] <- NA

#pattern to match street going west to east, for example, '31st street'
WEstreets <- dgt(2) %R% repeated(char_class("a-z"),2) %R% zero_or_more(SPC)

#find indices of matching elements in location and notes columns
ind_loc <- which(!is.na(str_match(iitCrime$Location, WEstreets)))
ind_not <- which(!is.na(str_match(iitCrime$Notes, WEstreets)))

#choose indices with empty address value
ind2_loc <- is.na(iitCrime$Address[ind_loc])
ind2_not <- is.na(iitCrime$Address[ind_not])
ind3_loc<-ind_loc*ind2_loc
ind3_not<-ind_not*ind2_not
ind4_loc<-ind3_loc[ind3_loc!=0]
ind4_not<-ind3_not[ind3_not!=0]

#adding vector of values extracted from location to 'WEstreets' columns with prefixes 'loc' and 'not'
iitCrime$WEstreets_loc[ind4_loc] <- str_match(iitCrime$Location, WEstreets)[ind4_loc]
iitCrime$WEstreets_not[ind4_not] <- str_match(iitCrime$Notes, WEstreets)[ind4_not]

NS_loc <- function(street, iitCrime, iitDictNS){
  
  #find indices of matching elements in location and notes columns
  ind_loc <- which(!is.na(str_match(iitCrime$Location, street)))
  ind_not <- which(!is.na(str_match(iitCrime$Notes, street)))
  
  #choose indices with empty address value
  ind2_loc <- is.na(iitCrime$Address[ind_loc])
  ind2_not <- is.na(iitCrime$Address[ind_not])
  ind3_loc<-ind_loc*ind2_loc
  ind3_not<-ind_not*ind2_not
  ind4_loc<-ind3_loc[ind3_loc!=0]
  ind4_not<-ind3_not[ind3_not!=0]
  
  #adding vector of values extracted from location to 'NSstreets' columns with prefixes 'loc' and 'not'
  iitCrime$NSstreets_loc[ind4_loc] <<- iitDictNS$address[match(street, iitDictNS$street)]
  iitCrime$NSstreets_not[ind4_not] <<- iitDictNS$address[match(street, iitDictNS$street)]
  
}

sapply(iitDictNS$street, NS_loc, iitCrime, iitDictNS)

#and finally pasting intersection
#priority is given to data extracted from location column
#indices of rows with WE and NS locations obtained from Location column present
ind_loc_loc <- which(!is.na(iitCrime$WEstreets_loc) & !is.na(iitCrime$NSstreets_loc) & is.na(iitCrime$Address))
#pasting locations and writing to address column
iitCrime$Address[ind_loc_loc] <- paste(iitCrime$WEstreets_loc[ind_loc_loc], " and ", iitCrime$NSstreets_loc[ind_loc_loc])

#doing the same for other combinations of locations obtained from Location and Notes columns:
#WEstreets_loc + NSstreets_not
#WEstreets_not + NSstreets_loc
#WEstreets_not + NSstreets_not

#indices of rows for combination of WE and NS locations - #WEstreets_loc + NSstreets_not
ind_loc_not <- which(!is.na(iitCrime$WEstreets_loc) & !is.na(iitCrime$NSstreets_not) & is.na(iitCrime$Address))
#pasting locations and writing to address column
iitCrime$Address[ind_loc_not] <- paste(iitCrime$WEstreets_loc[ind_loc_not], " and ", iitCrime$NSstreets_not[ind_loc_not])

#indices of rows for combination of WE and NS locations - #WEstreets_not + NSstreets_loc
ind_not_loc <- which(!is.na(iitCrime$WEstreets_not) & !is.na(iitCrime$NSstreets_loc) & is.na(iitCrime$Address))
#pasting locations and writing to address column
iitCrime$Address[ind_not_loc] <- paste(iitCrime$WEstreets_not[ind_not_loc], " and ", iitCrime$NSstreets_loc[ind_not_loc])

#indices of rows for combination of WE and NS locations - #WEstreets_not + NSstreets_not
ind_not_not <- which(!is.na(iitCrime$WEstreets_not) & !is.na(iitCrime$NSstreets_not) & is.na(iitCrime$Address))
#pasting locations and writing to address column
iitCrime$Address[ind_not_not] <- paste(iitCrime$WEstreets_not[ind_not_not], " and ", iitCrime$NSstreets_not[ind_not_not])

#observations still in search for address
#after this step will be left as 'NA'
naAddress <- iitCrime[which(is.na(iitCrime$Address)),]

#after the first search for address:
#matched: 1428 of 1604 obs
#not matched: 176 of 1604 obs

#the last step is to drop temporary columns
iitCrime <- iitCrime[,!names(iitCrime) %in% tempColumns]

#----------------------------------------------------------------
iitCrime$Incident<-(as.factor(iitCrime$Incident))
iitCrime$Location<-(as.factor(iitCrime$Location))
iitCrime$Address<-(as.factor(iitCrime$Address))

#rearrange columns with select function of dplyr package
iitCrime <- select(iitCrime, Incident, Location, Address, Latitude, Longitude, Occured:Notes)

summary(iitCrime)

#save iitCrime as .rda
save(iitCrime, file = "iitCrime.rda")
