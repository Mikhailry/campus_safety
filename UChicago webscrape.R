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


write.csv(uchicago_data, file = "Uchicago_campus_crimes.csv", fileEncoding = "UTF-8")


# We want to read this hmtl table and convert it into a data frame



uchicago_data <- read.csv("Uchicago_campus_crimes.csv")
head(uchicago_data)
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

unlist(strsplit("the2quickbrownfoxeswere2tired)", '[(]'))[1]
data_cpy <- uchicago_data
# Location_char <- as.character(uchicago_data$Location)
# levels(uchicago_data$Location) <- c(levels(uchicago_data$Location), "924 E. 58th St. (Knapp Center)")
# which(uchicago_data$Location == '924 E. 58th St. (Knapp Center)')
# uchicago_data$Location[186] <- '924 E. 58th St. (Knapp Center)'
library(tidyr)
data_cpy <- separate(data_cpy, Location,c("Location Address", "Location Name"), sep = "[(]", extra = 'merge')
data_cpy$`Location Name` <- gsub("\\)", "", data_cpy$`Location Name`)

head(data_cpy$Reported, 100) #4567 levels
data_cpy <- uchicago_data
no_report_values <- data_cpy$`Location Address`[grepl("^No.*", data_cpy$`Location Address`)]
for (i in no_report_values){

  data_cpy <- data_cpy[!(data_cpy$`Location Address` == i),]  
}
data_cpy$`Location Address` <- factor(data_cpy$`Location Address`)
nrow(data_cpy)
data_cpy["352", ]
View(data_cpy)
data_cpy[data_cpy$Incident == 'NA',]
data_cpy["3542", ]
data_cpy["59", ]
data_cpy["2480", ]
data_cpy <- data_cpy[!(data_cpy$Incident == 'NA'),]  
data_cpy <- separate(data_cpy, Reported, c('Date', 'Time', 'AM/PM')
               , sep=' ' 
               , remove=TRUE)
data_cpy$Time <- paste(data_cpy$Time, data_cpy$`AM/PM` , sep =  " ")
head(data_cpy)
sb$Time <- format(strptime(sb$Time, "%I:%M %p"), format="%H:%M")
drops <- c("Occured Start Date", "Occured End Date" )
sb <- sb[, !(names(sb) %in% drops)]

# summary(sb$Time)
# sb$Occurred
# pattern <- "\\d+/\\d+/\\d+"
# 
# sb$DatesOcc <- mapply(str_extract_all, sb$Occurred, pattern)
# sb$DatesOcc <- mapply(as.character, sb$DatesOcc)
# 
# #sb$DatesOcc <- as.vector(mapply(str_extract_all, sb$Occurred, pattern))
# 
# sb <- separate(sb, DatesOcc, c('Occured Start Date', 'Occured End Date')
#                , sep=',' 
#                , remove=TRUE)
# 
# 
# head(sb)
# sb$DatesOcc <- as.character(sb$DatesOcc)
# length(sb$DatesOcc[1])
# class(sb$DatesOcc)
# testval <- strsplit(sb$DatesOcc[1], split =  ',')
