#loading library for wev scrapping
library(rvest)
library(lubridate)
library(httr)

#starting with static url
url<-read_html("https://blogs.iit.edu/public_safety/2018/01/")

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
linkList <- lapply(links, getLinksPages)

#function 'xtrText' takes xml_node as input
#and returns content of type vector
xtrText <- fucntion(url){
  content<-url %>% 
  html_nodes(".content") %>%   #extract content from elements of class '.content'
  html_text()
  
  #removing escape characters
  q1<-gsub("\n", "", content)
  q2<-gsub("\t", "" , q1)
  content<-gsub("\r", "", q2)
  
  #substituting rss* values with ""
  content<-gsub("RSS.*$","",content)
  #removing empty values
  content<-content[!content==""]

return(content)
}


#initializing empty df to store IIT incidents
iitCrime <- data.frame(Incident=character(), Location=character(), 
                       Occured=as.Date(character()), Disposition=character(),
                       Notes=character(),stringsAsFactors = F)


#function takes vector element as input
#extract data, writes to data frame and returns it
xtrData <- function(content) {
  Incident<-
  Location<-
  Occured<-
  Disposition<-
  Notes<-
  
  iitCrime<-rbind(Incident, Location, Occured, Disposition, Notes)
}

