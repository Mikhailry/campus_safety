#loading library for web scrapping
library(rvest) #webscraping: extract pieces from html
library(lubridate) #manipulating dates
library(httr) #to check if page exists
library(rebus) #regular expression builder
library(stringr) #manipulating strings
library(purrr)  #to work with functions and vectors
library(plyr) #manipulating data structures
library(xlsx) #read/write Excel files
#library(rJava) #required to write to xlsx file

#static link for testing purposes
#testUrl<-c("https://blogs.iit.edu/public_safety/2015/09/")

#set a timeframe for data scrapping
startDate<-ymd(20150101)
endDate<-ymd(20180101)

#----------------------------------------------------------------
#function to generate a vector of all links for the timeframe
#takes 'startDate' and 'endDate' as input and returns a vector 'links' of links
getLinks<-function(startdate,endDate){
  
  #generate timeframe: a vector of format '2015/01' '2015/02'...
  timeFrame<-seq(from=startDate,to=endDate, by='month')
  timeFrame<-format(timeFrame, format="%Y/%m")
  
  #paste link
  links<-c(paste0("https://blogs.iit.edu/public_safety/",timeFrame,"/"))
  return (links)
}

#function to obtain links for the timeframe set
link<-getLinks(startDate, endDate)

#output sample - vector of links with only 1st page of posts:
#[1] "https://blogs.iit.edu/public_safety/2015/01/" "https://blogs.iit.edu/public_safety/2015/02/"
#[3] "https://blogs.iit.edu/public_safety/2015/03/" "https://blogs.iit.edu/public_safety/2015/04/"
#[5] "https://blogs.iit.edu/public_safety/2015/05/" "https://blogs.iit.edu/public_safety/2015/06/"

#----------------------------------------------------------------

#instantiating empty vector to store links to process
linkList<-vector(mode="character", 0)

#function that takes vector of links for the timeframe
#checks if additional pages for links exist
#returns 'linkList'
getLinksPages <- function(link){
  
  if (!http_error(link)){      #if link exists
    linkList<-append(linkList,link) #add link to vector of all links
  }
  
  page<-2 #starting page=2
  while(!http_error(paste0(link,"page/",page,"/"))){
    pageLink<-paste0(link,"page/",page,"/")
    linkList<-append(linkList,pageLink) #add link to vector of all links
    page<-page+1
  }
  
  return (linkList)
}
#get all links to parse with existing pages
linkList <- unlist(lapply(link, getLinksPages))
#get all links for the test link
#testLinkList<-getLinksPages(testUrl)

#output sample - vector of links with all pages of posts:
#[1] "https://blogs.iit.edu/public_safety/2015/01/"        "https://blogs.iit.edu/public_safety/2015/01/page/2/"
#[3] "https://blogs.iit.edu/public_safety/2015/01/page/3/" "https://blogs.iit.edu/public_safety/2015/02/"       
#[5] "https://blogs.iit.edu/public_safety/2015/02/page/2/" "https://blogs.iit.edu/public_safety/2015/03/"       

#----------------------------------------------------------------

#function 'xtrText' takes xml_node as input
#and returns content of type vector
xtrText <- function(url){
  url<-read_html(url)
  content<-url %>%
    html_nodes(".content") %>%   #extract content from elements of class '.content'
    html_text()
  
  #removing escape characters
  #q1<-str_replace_all(content, "\n", "")
  #q2<-str_replace_all(q1, "\t", "")
  #content<-str_replace_all(q2, "\r", "")
  
  #substituting rss* values with ""
  content<-gsub("RSS.*$","",content)
  #removing empty values
  content<-content[!content==""]
  #converting to lower case
  content2xtr<-tolower(content)
  
  return(content2xtr)
}

#extract posts from all pages (links from linkList)
content2xtr<-map(linkList,xtrText)
#extract posts for test linkList
#testContent2xtr<-map(testLinkList,xtrText)

#output sample - a list of all posts for the timeframe, one element (string) can have miltiple posts:
#[[86]]
#[1] "\n\t\t\t\t\t"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#[2] "\r\n\t\t\tthere is no information to report for the iitpsd public crime log for 1-8-2018.\n\t\t\t\r\n\t\t"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#[3] "\r\n\t\t\tgood morning and welcome back illinois tech!\nbelow is the public crime log for friday, january 5 thought sunday january 7 2018\nincident type:harassment : phone\nillinois institute of technology : kent campus – 565 w adams 1/5/2018 10:33 am\ndisposition: information only\nnotes: iitpsd responded to a phone harassment call.\n\nincident type:alarm : fire\nillinois institute of technology : vandercook 2 – 3125 s federal 1/5/2018 05:00 pm\ndisposition: facilities notified\nnotes: iitpsd responded to a call of an actived fire alarm at vandercook #2.\nincident type:assault-aggravated\nillinois institute of technology : vandercook 2 – 3125 s federal 1/6/2018 11:40 pm\ndisposition: cleared by arrest\nnotes: iitpsd responded to 31st and federal for a wellbeing check of a member of general public.\nincident type:utility incident : water\nillinois institute of technology : \mies campus : academic/administrative buildings : siegel hall – 3301 s dearborn 1/7/2018 04:36 pm\ndisposition: housekeeping notified\nnotes: iitpsd responded to siegel hall for a utility incident involving water.\nthank you\n\n\t\t\t\r\n\t\t"          

#----------------------------------------------------------------

#function to obtain number of incidents for the timeframe
#to check if it matches with number of incidents obtained for analysis
totalInc<-data.frame()

numIncidents<-function(content2xtr){
  
  #test block
  # t<-content2xtr[[65]][[3]]
  # t<-unlist(t)
  # str_view_all(t,dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% zero_or_more(SPC %R% dgt(1,2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% optional(or ("am", "pm"))))
  # str_view_all(t,"incident type")
  
  t<-content2xtr
  #date match pattern
  pat<-capture(dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% SPC %R% dgt(1,2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% or ("am", "pm"))
  #pat<-capture(dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% zero_or_more(SPC %R% dgt(1,2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% optional(or ("am", "pm"))))
  #matching all date values in the string
  a<-str_match_all(t, pat)
  #combining results in a data frame and assign it as vector
  a<-as.vector(ldply(a)[,1])
  #matching occurences of "incident type"
  b<-str_match_all(t, "incident type")
  #combining results in a data frame and assign it as vector
  b<-as.vector(ldply(b)[,1])
  
  #if number of incidents and date values match, when put date and OK flag in a df
  #elseif not match and date vector is empty put "no incident"
  if (length(a)==length(b) & !is_empty(a)) {
    totalInc<-rbind(totalInc, cbind(date=a,type="ok"))
  } else if (length(a)!=length(b) & !is_empty(b)) {
    totalInc<-rbind(totalInc, cbind(date=a,type="check"))
  }
  
  return(totalInc)
}

#unlist posts
content2xtr2<-unlist(content2xtr)
#apply numIncidents function to extract dates of incidents and records to check
n<-lapply(content2xtr2, numIncidents)
#remove empty elements
n<-n[lapply(n,length)>0]
#transform list elements to df
incidents<-ldply(n)
#show summary
summary(incidents)

#incidents with matching date - 1580
#incidents requiring manual check - 22
#ok   :1580  
#check:  22

#count number of incidents
nIncedents <- nrow(incidents)
nIncedents
#write df to csv
write.csv(incidents, file = "incidents.csv", fileEncoding = "UTF-8")

#output - number of incidents (nIncedents) = 1602
#and output file to check at which dates number of incidents in the post doesn't match number of dates

#----------------------------------------------------------------

#vector to hold singular posts
posts<-vector(mode="character", 0)

#function takes content2xtr vector as input
#returns vector of singular posts
content2posts <- function(content2xtr2){
  
  #test block
  #n[[876]]
  #z<-content2xtr2[[876]]
  # str_view(z,pattern=pattern)
  # str_split(z,pattern)
  
  #splitting posts on "incident type"
  post<-str_split(content2xtr2, lookahead("incident type"))
  
  posts<-append(posts, post)
  return(posts)
}

#getting the vector of singular posts
posts<-unlist(lapply(content2xtr2, content2posts))

#save posts as .rda
save(posts, file = "posts.rda")
#load(posts)

#output - a vector of all posts for the timeframe (each post in a single string):
#[996] "incident type: trespassing : bar notice\nillinois institute of technology : mies campus : stuart building – 2/5/2016 04:43 am\ndisposition: police notified\nnotes: iitpsd responded to stuart building for a trespassing report. iitpsd detained the individual and was issued a bar notice.\n \n"                                                                                                  
#[997] "incident type:assist other agency/department\nillinois institute of technology : mies campus : street locations : state-2/5/2016 08:30 pm\ndisposition: police notified\nnotes: iitpsd responded to 31st and state street for a report of shots fired in the area involving members of the general public.\n \nthank you,\n \n\t\t\t\r\n\t\t"                                                        
#[998] "\r\n\t\t\thello,\n \nthere is no information to report for the  iitpsd public crime log for 2/4/2016.\n \nthank you,\n \n\t\t\t\r\n\t\t"            

#----------------------------------------------------------------

#initializing empty df to store IIT incidents
iitCrime <- data.frame(Incident=character(), Location=character(),
                       Occured=character(), Disposition=character(),
                       Notes=character(),stringsAsFactors = F)

#function takes vector element as input
#extract data, writes to data frame and returns it
xtrData <- function(posts) {
  
   #patterns definitions
   incidentPattern <- "incident type:" %R% capture(one_or_more(PRINT)) %R% "illinois institute of technology"
   datePattern <- dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% zero_or_more(SPC %R% dgt(1,2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% optional(or ("am", "pm"))) 
   
   disposPattern <- "disposition:" %R% capture(one_or_more(PRINT))
   notesPattern <- "notes:" %R% capture(one_or_more(PRINT))
  
   #test block
   # posts2<-posts
   # posts<-posts2[1649]
   # posts
   # posts<-str_replace_all(posts, "\n", " ")
   # posts
   # str_view(posts, incidentPattern)#Incident <-
   # str_match(posts, incidentPattern)[,2]
   # 
   # str_view(posts, datePattern)#Occured <-
   # Occured <- str_match(posts, datePattern)[,1]
   # 
   # str_view(posts, locationPattern)#Location <-
   # str_match(posts, locationPattern)[,2]
   # 
   # str_view(posts, disposPattern)#Disposition <-
   # str_view(posts, notesPattern)#Notes <-
   
   #replace all \n symbols
   posts<-str_replace_all(posts, "\n", " ")
   
   #matching incidents
   Incident <- str_match(posts, incidentPattern)[,2]
   
   #matching date values
   Occured <- str_match(posts, datePattern)[,1]
   
   #matching disposition
   Disposition <- str_match(posts, disposPattern)[,2]
   #matching notes
   Notes <- str_match(posts, notesPattern)[,2]
   
   #matching location
   locationPattern <- capture("illinois institute of technology" %R% one_or_more(PRINT)) %R% Occured
   Location <- str_match(posts, locationPattern)[,2]
   
   #assembling data frame to return
   iitCrime<-as.data.frame(cbind(Incident, Location, Occured, Disposition, Notes))
   names(iitCrime)<-c("Incident", "Location", "Occured", "Disposition", "Notes")
   
   return(iitCrime)
}

#takes the vector of posts, extract data and puts in a df
iitCrime<-xtrData(posts)
#removing NA's
iitCrime <- iitCrime[!is.na(iitCrime$Incident),]
#show summary
summary(iitCrime)

#output  - df iitCrime
#----------------------------------------------------------------

#let's do some final cleaning on iitCrime df

#remove whitespaces at start and end of values of df
iitCleaned <- data.frame(lapply(iitCrime, trimws), stringsAsFactors = F)

#remove escape characters, characters from the set [-,–,:] at the end of string
removeEscapeChar <- function(x) {
  x <- str_trim(x, side="both")
  x <- str_replace_all(x, "\n", "")
  x <- str_replace_all(x, "\r", "")
  x <- str_replace_all(x, "\t", "")
  x <- str_replace_all(x, "[-,–,:]$", "")
}

iitCleaned2<-data.frame(lapply(iitCleaned,removeEscapeChar), stringsAsFactors = F)

#number or observations where location is missing
sum(is.na(iitCleaned2$Location)) #4
#finding indices and view the rows with missing Location values
ind <- which(is.na(iitCleaned2$Location))
# Look at the full rows for missing Location
iitCleaned2[ind, ]

#find empty dates indices
emptyDatesInd <- which(is.na(iitCleaned2$Occured))
iitCleaned2[emptyDatesInd,]

#find dates without time stamp
datesNoTimeInd <- which(is.na(as.POSIXct(iitCleaned2$Occured,tz="America/Chicago", format="%m/%d/%Y %I:%M %p")))
datesNoTime <- iitCleaned2$Occured[datesNoTimeInd]

#convert data type of 'occured' variable
iitCleaned2$Occured <- as.POSIXct(iitCleaned2$Occured,tz="America/Chicago", format="%m/%d/%Y %I:%M %p")

#add time stamp
iitCleaned2$Occured[datesNoTimeInd] <- as.POSIXct(paste(datesNoTime, "00:00 am"), format="%m/%d/%Y %H:%M %p")

#replace empty dates with "1/1/1970 00:00 am"
iitCleaned2$Occured[emptyDatesInd] <- as.POSIXct("1/1/1970 00:00 am", format="%m/%d/%Y %H:%M %p")

#check if number of incidents matches
stopifnot(nrow(iitCleaned2)==nIncedents | (nrow(iitCleaned2) > nIncedents*0.95 & nrow(iitCleaned2) < nIncedents*1.05))

#output - df iitCleaned2
#----------------------------------------------------------------

#save iitCleaned2 as iitCrime
iitCrime <- iitCleaned2

#writing output to the csv
write.csv(iitCrime, file = "iit_campus_crimes.csv")

#save iitCrimeCleaned as .rda
save(iitCrime, file = "iitCrime.rda")
