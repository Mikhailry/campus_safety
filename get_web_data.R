#loading library for web scrapping
library(rvest)
library(lubridate)
library(httr)
library(rebus)
library(stringr)
library(purrr)
library(plyr)

#static link for testing purposes
#testUrl<-c("https://blogs.iit.edu/public_safety/2015/09/")

#set a timeframe for data scrapping
startDate<-ymd(20150101)
endDate<-ymd(20180101)

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


#function 'xtrText' takes xml_node as input
#and returns content of type vector
xtrText <- function(url){
  url<-read_html(url)
  content<-url %>%
    html_nodes(".content") %>%   #extract content from elements of class '.content'
    html_text()
  
  #removing escape characters
  #q1<-gsub("\n", "", content)
  #q2<-gsub("\t", "" , content)
  #content<-gsub("\r", "", q2)
  
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

#=======================================================
#function to obtain number of incidents for the timeframe
#to check if it matches with number of incidents obtained for analysis
totalInc<-data.frame()

numIncidents<-function(content2xtr){
  
  #test block
  #t<-testContent2xtr[[2]][[11]]
  #t<-unlist(t)
  #str_view_all(t,dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% SPC %R% dgt(1,2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% or ("am", "pm"))
  #str_view_all(t,"incident type")
  
  t<-content2xtr
  #date match pattern
  pat<-capture(dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% SPC %R% dgt(1,2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% or ("am", "pm"))
  #matching all date values in the string
  a<-str_match_all(t, pat)
  #combining results in a data frame and assign it as vector
  a<-as.vector(ldply(a)[,1])
  #matching occurences of "incident type"
  b<-str_match_all(t, "incident type")
  #combining results in a data frame and assign it as vector
  b<-as.vector(ldply(b)[,1])
  
  #------------------------
  #TODO double-check cases
  #------------------------
  #if number of incidents and date values match, when put date and incident flag in a df
  #elseif not match and date vector is empty put "no incident"
  if (length(a)==length(b) & !is_empty(a)) {
    totalInc<-rbind(totalInc, cbind(date=a,type="incident"))
  } else if (length(a)!=length(b) & !is_empty(a)) {
    totalInc<-rbind(totalInc, cbind(date=a,type="no incident"))
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
#count number of incidents
nIncedents <- nrow(incidents[incidents$type=="incident",])
nIncedents
#write df to csv
write.csv(incidents, file = "incidents.csv", fileEncoding = "UTF-8")
#==============================

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


#initializing empty df to store IIT incidents
iitCrime <- data.frame(Incident=character(), Location=character(),
                       Occured=character(), Disposition=character(),
                       Notes=character(),stringsAsFactors = F)

#================================================
#function takes vector element as input
#extract data, writes to data frame and returns it
xtrData <- function(posts) {
  
   #test block
   #posts2<-posts
   #posts<-posts2[900]
  
   #patterns definitions
   incidentPattern <- "incident type:" %R% capture(one_or_more(PRINT)) #%R% repeated(SPC,3,3)
   datePattern <- dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% SPC %R% dgt(1,2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% or ("am", "pm")
   disposPattern <- "disposition:" %R% capture(one_or_more(PRINT))
   notesPattern <- "notes:" %R% capture(one_or_more(PRINT))
  
   #matching incidents
   Incident <- str_match(posts, incidentPattern)[,2]
   
   #mathcing location in 6 steps:
   #1. splitting post on date
   loc1 <- str_split(posts, lookahead(datePattern))
   #2. taking first part which contains incident type and location
   loc2 <- map(loc1,1)
   #3. splitting part obtained in step 2 on location and incident type
   loc3 <- str_split(loc2,lookbehind(Incident))
   #4. taking second part (with location)
   loc4 <- map(loc3,2)
   #5. substitute NULL's with NA's 
   loc4[sapply(loc4, is.null)] <- NA
   #6. unlist
   Location <- unlist(loc4)
     
   #matching date values
   Occured <- str_match(posts, datePattern)[,1]
   #matching disposition
   Disposition <- str_match(posts, disposPattern)[,2]
   #matching notes
   Notes <- str_match(posts, notesPattern)[,2]
  
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
#writing output to the csv
write.csv(iitCrime, file = "iit_campus_crimes_v3.csv", fileEncoding = "UTF-8")

#check if number of incidents matches
stopifnot(nrow(iitCrime)==nIncedents | (nrow(iitCrime) > nIncedents*0.95 & nrow(iitCrime) < nIncedents*1.05))
