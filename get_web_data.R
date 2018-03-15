#loading library for web scrapping
library(rvest)
library(lubridate)
library(httr)
library(rebus)
library(stringr)

#starting with static url
#url<-read_html("https://blogs.iit.edu/public_safety/2018/01/")

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

#get links function for the defined timeframe
link<-getLinks(startDate, endDate)

#instantiating empty vector to store links to process
linkList<-c()

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
#linkList<-unlist(linkList)


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

#extract posts from all pages (links)
#testLinkList<-linkList[86]
content2xtr<-sapply(linkList,xtrText)

#==============================
#test incident extraction
pattern2<-"incident type" %R% zero_or_more(printable()) %R% #incident type
  repeated(SPC,1,3) %R% zero_or_more(printable()) %R% #location and date
  repeated(SPC,1,3) %R% optional(zero_or_more(printable()) %R% #disposition
  repeated(SPC,1,3)) %R% zero_or_more(printable()) #notes
#
jan2015<-content2xtr[[1]]
str_view_all(content2xtr[[5]], pattern2)
#=============================


#vector to hold singular posts
posts<-c()

#function takes content2xtr vector as input
#returns vector of singular posts
content2posts <- function(content2xtr){

  pattern<-"incident type" %R% zero_or_more(printable()) %R% 
    repeated(SPC,3,3) %R% zero_or_more(printable()) %R%
    repeated(SPC,3,3) %R% zero_or_more(printable()) %R% 
    repeated(SPC,3,3) %R% zero_or_more(printable())
  
  post<-str_extract_all(content2xtr,pattern=pattern)
  posts<-append(posts, post)  
  return(posts)
}

#posts<-unlist(content2posts(content2xtr))
#postsFinal<-NULL

#getting the vector of singular posts
posts<-unlist(lapply(content2xtr, content2posts))
posts


#initializing empty df to store IIT incidents
iitCrime <- data.frame(Incident=character(), Location=character(), 
                       Occured=as.Date(character()), Disposition=character(),
                       Notes=character(),stringsAsFactors = F)


#function takes vector element as input
#extract data, writes to data frame and returns it
xtrData <- function(posts) {
  
  #patterns definitions
  onePattern<-"incident type:" %R% capture(one_or_more(PRINT)) %R% repeated(SPC,3,3) %R%  #capturing incident type
    capture(one_or_more(PRINT)) %R% #capturing location
    capture(dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% 
    SPC %R% dgt(2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% or ("am", "pm")) %R% repeated(SPC,3,3) %R% #capturing date
    "disposition:" %R% capture(one_or_more(PRINT)) %R% repeated(SPC,3,3) %R% #capturing disposition
    "notes:" %R% capture(one_or_more(PRINT)) #capturing notes
    
#  str_view(postsFinal[1], onePattern)
#  str_match(postsFinal[1], onePattern)
  
#  this works
#  locDatPattern<-capture(one_or_more(PRINT)) %R% capture(dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% 
#    SPC %R% dgt(2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% or ("am", "pm"))
#  str_match(b,locDatPattern)
  
#  incidentPattern <- "incident type:" %R% capture(one_or_more(PRINT)) %R% 
#    repeated(SPC,3,3)
#  locDatPattern<-capture(one_or_more(PRINT)) %R% capture(dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% 
#                                                           SPC %R% dgt(2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% or ("am", "pm"))
#  datePattern <- dgt(1,2) %R% "/" %R% dgt(1,2) %R% "/" %R% dgt(4) %R% 
#    SPC %R% dgt(2) %R% ":" %R%  dgt(2) %R% optional(SPC) %R% or ("am", "pm")
#  #datePattern2 <- "\\d{1,2}/\\d{1,2}/\\d{4}\\s\\d{2}:\\d{2}[\\s]?(?:am|pm)"
#  disposPattern <- "disposition:" %R% capture(one_or_more(PRINT))
#  notesPattern <- "notes:" %R% capture(one_or_more(PRINT))
    

#  str_view(postsFinal[1], incidentPattern)
#  str_match(postsFinal[1], incidentPattern)
  
#  Incident<-str_match(postsFinal, incidentPattern)
#  Location<-
#  Occured<-str_match(postsFinal, datePattern)
#  Disposition<-
#  Notes<-
 
  posts2xtr<-str_match(posts, onePattern)[,-1] 
  iitCrime<-rbind(iitCrime,posts2xtr)
  colnames(iitCrime)<-c("Incident", "Location","Occured", "Disposition", "Notes")
  #iitCrime<-rbind(Incident=posts2xtr[,1], Location=posts2xtr[,2], Occured=posts2xtr[,3], Disposition=posts2xtr[,4], Notes=posts2xtr[,5])
  return(iitCrime)
}

#takes the vector of posts, extract data and puts in a df
iitCrime<-xtrData(posts)
iitCrime
write.csv(iitCrime, file = "iit_campus_crimes.csv", fileEncoding = "UTF-8")
