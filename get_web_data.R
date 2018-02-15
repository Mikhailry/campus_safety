
#xtrText(url)


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

