# Obtaining data from web sources
# Webscraping (very basic introduction)
install.packages('rvest') # Webscraping package
install.packages('stringr') # Package that makes manipulating strings (text) easier
install.packages('tidyr') # Package for making data manipulation easier

library(rvest)
library(stringr)
library(tidyr)
library(xml2)
# Let's take a look at this webpage and how html tables are constructed
url <- 'https://incidentreports.uchicago.edu/incidentReportArchive.php?startDate=1388556000&endDate=1520917200&offset=0'
webpage <- read_html(url)

sb_table <- html_nodes(webpage, css = 'table')
#Extracting the "Next" URL for navigation
page_list <- webpage %>% html_nodes("a")

t <- sb_table[[1]]
sb <- html_table(sb_table)[[1]]


#extracting the current page and number of pages based on search
page_count <- trimws(webpage %>% html_nodes("li.page-count") %>% html_text())
current_page <- as.integer(trimws(unlist(strsplit(page_count, split = "/"))[1]))
total_pages <- as.integer(trimws(unlist(strsplit(page_count, split = "/"))[2]))



for (i in 1:length(page_list)) {
  if (str_detect(html_text(page_list[i]), 'Next'  ) ){
    next_page <- html_attr(page_list[i], 'href')
    break()
  }
}

next_page <- paste("https://incidentreports.uchicago.edu/", next_page, sep = "" )


count = 0
while(current_page <= total_pages) {
  count = count + 1
  webpage <- read_html(next_page)
  sb_table <- html_nodes(webpage, css = 'table')
  uchicago_data <- rbind(sb,html_table(sb_table)[[1]])
  page_count <- trimws(webpage %>% html_nodes("li.page-count") %>% html_text())
  current_page <- as.integer(trimws(unlist(strsplit(page_count, split = "/"))[1]))
  #total_pages <- as.integer(trimws(unlist(strsplit(page_count, split = "/"))[2]))
  page_list <- webpage %>% html_nodes("a")
  
  
  for (i in 1:length(page_list)) {
    if (str_detect(html_text(page_list[i]), 'Next'  ) ){
      next_page <- html_attr(page_list[i], 'href')
      break()
    }
    
  }
  next_page <- paste("https://incidentreports.uchicago.edu/", next_page, sep = "" )
  # print(current_page)
  # print(total_pages)
}


write.csv(uchicago_data, file = "Uchicago_campus_crimes_raw.csv", fileEncoding = "UTF-8")


# We want to read this hmtl table and convert it into a data frame


uchicago_data <- read.csv("Uchicago_campus_crimes_cleaned.csv")
View(uchicago_data)
head(uchicago_data)
uchicago_data <- read.csv("Uchicago_campus_crimes_raw.csv")
nrow(uchicago_data) #5371 rows
ncol(uchicago_data) # 8 columns
names(uchicago_data)
names(uchicago_data)[1] <- 'Sl.no'
#removed Sl no
drop = c("Sl.no")
uchicago_data <- uchicago_data[,!(names(uchicago_data) %in% drop)]
attach(uchicago_data)
summary(uchicago_data)
lapply(uchicago_data, class) #all are currently factor variables
unique(Incident)
#398 levels
Incident_df <- data.frame(table(uchicago_data$Incident, useNA = 'always'))
head(Incident_df)

attach(Incident_df)
ordered_Incident_df <- Incident_df[order(-Freq), ]
head(ordered_Incident_df,10)
#highest common incidents are
# 368                       Theft  864
# 2                             :  589
# 124                 Information  437
# 252               Lost Property  322
# 112              Found Property  237
# 247        Liquor Law Violation  190
# 392                        Void  150
# 142 Information / Armed Robbery  124
# 158      Information / Burglary  117
# 255                Medical Call  111

ordered_Incident_df <- Incident_df[order(Var1), ]
head(ordered_Incident_df,10)
#lets look at the places where : values are coming
library(dplyr)
filter(uchicago_data, uchicago_data$Incident == ":")
filter(uchicago_data, uchicago_data$Incident == "VOID")
filter(uchicago_data, uchicago_data$Incident == "Void")

#Delete the rows having these values
uchicago_data<-uchicago_data[!(uchicago_data$Incident==":" | uchicago_data$Incident == "Void" 
                               | uchicago_data$Incident == "VOID"),]
uchicago_data$Incident <- factor(uchicago_data$Incident)
nrow(uchicago_data) #4628 rows

Incident_df <- data.frame(table(uchicago_data$Incident, useNA = 'always'))
head(Incident_df)

attach(Incident_df)
ordered_Incident_df <- Incident_df[order(-Freq), ]
head(ordered_Incident_df,10)

# Var1 Freq
# 367                       Theft  864
# 123                 Information  437
# 251               Lost Property  322
# 111              Found Property  237
# 246        Liquor Law Violation  190
# 141 Information / Armed Robbery  124
# 157      Information / Burglary  117
# 254                Medical Call  111
# 242              Injured Person  105
# 258     Mental Health Transport   91


Location_df <- data.frame(table(uchicago_data$Location, useNA = 'always'))

ordered_Location_df <- Location_df[order(-Location_df$Freq), ]
head(ordered_Location_df,10)
#Var1 Freq
# 1126                5700 S. Maryland (CCD)  159
# 1332  5815 S. Maryland (Mitchell Hospital)  121
# 1508                        6054 S. Drexel   86
# 1725         901 E. 58th St. (Mitchell ER)   80
# 1480 6031 S. Ellis (Granville-Grossman RH)   74
# 1250               5758 S. Maryland (DCAM)   72
# 917            5530 S. Ellis (Ratner A.C.)   58
# 1697             850 E. 61st St. (UCPD HQ)   56
# 1186     5721 S. Maryland (Comer Hospital)   48
# 75   1100 E. 57th St. (Regenstein Library)   47


ordered_Location_df <- Location_df[order(Location_df$Var1), ]
head(ordered_Location_df,10)
filter(uchicago_data, uchicago_data$Location == "")
uchicago_data<-uchicago_data[!(uchicago_data$Location=="") ,]
uchicago_data$Location <- factor(uchicago_data$Location)

#use regular expression to pull the locationo detail from the address

#unlist(strsplit("the2quickbrownfoxeswere2tired)", '[(]'))[1]
data_cpy <- uchicago_data
# Location_char <- as.character(uchicago_data$Location)
# levels(uchicago_data$Location) <- c(levels(uchicago_data$Location), "924 E. 58th St. (Knapp Center)")
# which(uchicago_data$Location == '924 E. 58th St. (Knapp Center)')
# uchicago_data$Location[186] <- '924 E. 58th St. (Knapp Center)'
library(tidyr)
data_cpy <- separate(data_cpy, Location,c("Location Address", "Location Name"), sep = "[(]", extra = 'merge')

data_cpy$`Location Name` <- gsub("\\)", "", data_cpy$`Location Name`)

head(data_cpy$Reported, 100) #4567 levels
uchicago_data <- data_cpy
no_report_values <- data_cpy$`Location Address`[grepl("^No.*", data_cpy$`Location Address`)]
for (i in no_report_values){

  data_cpy <- data_cpy[!(data_cpy$`Location Address` == i),]  
}
data_cpy$`Location Address` <- factor(data_cpy$`Location Address`)
nrow(data_cpy)

View(data_cpy)
data_cpy[data_cpy$Incident == 'NA',]
data_cpy <- data_cpy[!(data_cpy$Incident == 'NA'),]  
data_cpy <- separate(data_cpy, Reported, c('Date', 'Time', 'AM/PM')
               , sep=' ' 
               , remove=TRUE)
data_cpy$Time <- paste(data_cpy$Time, data_cpy$`AM/PM` , sep =  " ")
# Warning messages:
#   1: Too many values at 2 locations: 352, 3542 
# 2: Too few values at 2 locations: 59, 2480 


head(data_cpy)
#some tasks to be done in reporting field

data_cpy$Time <- format(strptime(data_cpy$Time, "%I:%M %p"), format="%H:%M")

drop_col <- function(drops, df) {

  return(df[, !(names(df) %in% drops)])
}



write.csv(uchicago_data, file = "Uchicago_campus_crimes_partlycleaned.csv", fileEncoding = "UTF-8")
uchicago_data <- read.csv("Uchicago_campus_crimes_partlycleaned.csv")
head(uchicago_data)
data_cpy <- uchicago_data
Occured_df <- data.frame(table(data_cpy$Occurred, useNA = 'always'))
head(Occured_df)
attach(Occured_df)
ordered_Occured_df <- Occured_df[order(-Freq), ]
head(ordered_Occured_df)



data_cpy[which(data_cpy$Occurred == "Unknown date and time"),]


head(data_cpy)

date.pat <- '\\d{1,2}/\\d{1,2}/\\d{2,4}'
time.pat <- '\\d{1,2}:\\d{1,2} ([AaPp][Mm])'
x <- "12/31/13 to 10:00 PM to 1:30 AM"
regmatches(x, gregexpr(time.pat, x))

Occured_char <- as.character(data_cpy$Occurred)
Occured_char <- regmatches(Occured_char, gregexpr(date.pat, Occured_char))
length(Occured_char)
Occured_char <- lapply(Occured_char, unlist)
length(Occured_char)
data_cpy$'Occured Start Date' <- sapply(Occured_char, function(x) return (x[1]))
data_cpy$'Occured End Date' <- sapply(Occured_char, function(x) return (x[2]))
head(data_cpy)

Occured_char <- as.character(data_cpy$Occurred)
Occured_char <- regmatches(Occured_char, gregexpr(time.pat, Occured_char))
length(Occured_char)
Occured_char <- lapply(Occured_char, unlist)
length(Occured_char)
data_cpy$'Occured Start Time' <- sapply(Occured_char, function(x) return (x[1]))
data_cpy$'Occured End Time' <- sapply(Occured_char, function(x) return (x[2]))

head(data_cpy)
uchicago_data <- data_cpy
data_cpy[which(data_cpy$Occurred == "Unknown date and time"),]

data_cpy$`Occured Start Date`[which(data_cpy$Occurred == "Unknown date and time")] <- "Unknown Start Date"
data_cpy$`Occured End Date`[which(data_cpy$Occurred == "Unknown date and time")] <- "Unknown End Date"
data_cpy$`Occured Start Time`[which(data_cpy$Occurred == "Unknown date and time")] <- "Unknown Start Time"
data_cpy$`Occured End Time`[which(data_cpy$Occurred == "Unknown date and time")] <- "Unknown End Time"


data_cpy <- drop_col(c('Occurred'), data_cpy)
uchicago_data <- data_cpy
head(data_cpy)
View(uchicago_data)
data_cpy$`Occured Start Time` <- format(strptime(data_cpy$`Occured Start Time`, "%I:%M %p"), format="%H:%M")
data_cpy$`Occured End Time` <- format(strptime(data_cpy$`Occured End Time`, "%I:%M %p"), format="%H:%M")

View(uchicago_data)
data_cpy <- uchicago_data
data_cpy <- drop_col(c("X"), data_cpy)
View(data_cpy)
uchicago_data <- data_cpy
write.csv(uchicago_data, file = "Uchicago_campus_crimes_cleaned.csv", fileEncoding = "UTF-8")


uchicago_data <- read.csv("UC_stand.csv")
head(uchicago_data)
length(unique(uchicago_data$Location.Address)) #1560 unique addresses
attach(uchicago_data)
freq.addresses <- data.frame(table(uchicago_data$Location.Address, useNA = 'always'))

attach(freq.addresses)
ordered_freq.addresses <- freq.addresses[order(-freq.addresses$Freq), ]
nrow(ordered_freq.addresses)
head(ordered_freq.addresses,10)
tail(ordered_freq.addresses)
names(ordered_freq.addresses)[1] <- 'Original address'
library(ggmap)

library(dplyr)
ordered_freq.addresses <- ordered_freq.addresses[1:(nrow(ordered_freq.addresses) - 1),  ]
#Checking for empty addresses
filter(ordered_freq.addresses, ordered_freq.addresses$Location.Address == "")

#values without having the separations
subset_address_1 <- dplyr::filter(ordered_freq.addresses, !( grepl("/", ordered_freq.addresses$Location.Address)|
                                                          grepl(" to ", ordered_freq.addresses$Location.Address) |
                                                          grepl('between', ordered_freq.addresses$Location.Address) |
                                                            grepl('Between', ordered_freq.addresses$Location.Address)  ))
#values having separations
subset_address_2 <- dplyr::filter(ordered_freq.addresses, ( grepl("/", ordered_freq.addresses$Location.Address)|
                                                               grepl(" to ", ordered_freq.addresses$Location.Address) |
                                                               grepl('between', ordered_freq.addresses$Location.Address) |
                                                               grepl('Between', ordered_freq.addresses$Location.Address)  ))
#values having between and Between
subset_address_3 <- dplyr::filter(subset_address_2, ( grepl('between', subset_address_2$Location.Address) |
                                                               grepl('Between', subset_address_2$Location.Address)  ))
#values having to and /
subset_address_4 <- dplyr::filter(subset_address_2, !( grepl('between', subset_address_2$Location.Address) |
                                                        grepl('Between', subset_address_2$Location.Address)  ))


#checking whether ':' is present in addresses
#dplyr::filter(ordered_freq.addresses, (grepl(",", ordered_freq.addresses$Location.Address)))
nrow(subset_address)
head(subset_address)
#Basic splitting mechanism
#separating based on 'between' and 'Between'
toMatch <- c("Between", "between")
subset_address_3 <- separate(subset_address_3, Location.Address, c("Part1", 'Part2')
                     , sep=paste(toMatch,collapse="|") 
                     , remove=TRUE)
#nrow(dplyr::filter(subset_address, (grepl("&", subset_address$Part2))))
#splitting second part based on '&'
toMatch <- c("&", " and ")
subset_address_3 <- separate(subset_address_3, Part2, c("Part3", 'Part4')
                           , sep=paste(toMatch,collapse="|") 
                           , remove=TRUE)
#subset_address[,3:5]

#Merging part1 and part 3, part 1 and part 4
subset_address_3[is.na(subset_address)] <- ""
subset_address_3$'New Part 1' <- NULL
subset_address_3$'New Part 2' <- NULL
subset_address_3['New Part 1'] <- paste(subset_address_3$Part1, subset_address_3$Part3)
subset_address_3['New Part 2'] <- paste(subset_address_3$Part1, subset_address_3$Part4)

#separating based on 'to'

subset_address_4 <- separate(subset_address_4, Location.Address, c("New Part 1", 'New Part 2')
                             , sep=" to "
                             , remove=TRUE)
#nrow(dplyr::filter(subset_address, (grepl("&", subset_address$Part2))))
#splitting second part based on '&'

subset_address_4[is.na(subset_address_4)] <- ""
subset_address_3[is.na(subset_address_3)] <- ""
subset_address_3$'Part1' <- NULL
subset_address_3$'Part3' <- NULL
subset_address_3$'Part4' <- NULL

subset_address_5 <- rbind(subset_address_3, subset_address_4)
subset_address_5$'Location.Address' <- paste(subset_address_5$`New Part 1`, subset_address_5$`New Part 2`, sep = ",")
#nrow(subset_address_3) + nrow(subset_address_4 ) == nrow(subset_address_5)
?append
subset_address_5$'New Part 1' <- NULL
subset_address_5$'New Part 2' <- NULL
finalized_addresses <- rbind(subset_address_1, subset_address_5)
View(finalized_addresses)

write.csv(finalized_addresses, file = "Address dictionary_UChicago.csv", fileEncoding = "UTF-8")
#dplyr::filter(finalized_addresses, (grepl("//.", finalized_addresses$Location.Address)))


nrow(ordered_freq.addresses) == nrow(finalized_addresses)

geocode("Hyde Park Kimbark")

#install.packages("gsubfn")
#ibrary(gsubfn)

address_mod <- function(location) {
  changes <- gsub("E\\.", "East", location)
  changes <- gsub("W\\.", "West", changes)
  changes <- gsub('S\\.', "South", changes)
  changes <- gsub("N\\.", "North", changes)
  changes <- gsub("St\\.", "Street", changes)
  changes <- gsub("Pl\\.", "Place", changes)
  changes <- gsub("E\\,", "East", changes)
  changes <- gsub("W\\,", "West", changes)
  changes <- gsub('S\\,', "South", changes)
  changes <- gsub("N\\,", "North", changes)
  changes <- gsub("Hwy\\.", "Highway", changes)
  changes <- gsub("St\\,", "Street", changes)
  changes <- gsub(", IL", "", changes)
  changes <- gsub(", IN", "", changes)
  return (changes)
}


ordered_freq.addresses$Location.Address <- sapply(ordered_freq.addresses$`Original address`, address_mod)
head(ordered_freq.addresses)
nrow(ordered_freq.addresses)
tail(ordered_freq.addresses)
ordered_freq.addresses_V1 <- ordered_freq.addresses[1:(nrow(ordered_freq.addresses) - 1),  ]
head(ordered_freq.addresses_V1)
tail(ordered_freq.addresses_V1)
simple_addresses <- dplyr::filter(ordered_freq.addresses_V1, !(grepl('between', ordered_freq.addresses_V1$Location.Address) |
                                                              grepl('to', ordered_freq.addresses_V1$Location.Address)))

stopifnot(length(ordered_address_V1) == nrow(ordered_freq.addresses))
nrow(simple_addresses)

install.packages("googleway")
key <- 'AIzaSyAEmFXnK_n2Xw50Di72MY5a8sFj41zAh9Q'

coordinates <- function(x){
  
  address <- paste(x,"Chicago IL", sep = " ")
  
  temp<-geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  
  while(temp$status == "OVER_QUERY_LIMIT"){

    Sys.sleep(3)
    
    temp <- geocode(address, output='all', messaging=TRUE, override_limit=TRUE)

    
  }
  
  if(temp$status == "ZERO_RESULTS")#if there is no result
    
  {
    return(NA)
 
  }
 
  Latitude <- temp$results[[1]]$geometry$location$lat #returns lat
  Longitude <- temp$results[[1]]$geometry$location$lng #returns long
  
  
  return(paste(Latitude,Longitude,sep = ","))
  
}

coordinates("5815 South Maryland")
Sys.sleep(2)
address_ext()
library(googleway)
geocode("5815 South Maryland")
#lat_lon <- google_geocode(address = "5815 South Maryland", key = key)
#lat_lon$results$geometry$location
install.packages('devtools')
library(devtools)
install.packages('ggplot2')
devtools::install_github("dkahle/ggmap")
example <- sapply(simple_addresses$Location.Address[1:2], address_ext)

head(simple_addresses$`lat-lon1`)
example<- lapply(simple_addresses$Location.Address, coordinates)
simple_addresses$`lat-lon` <- example
simple_addresses$`lat-lon` <- NULL
head(simple_addresses)
View(simple_addresses)
tail(simple_addresses, 100)


