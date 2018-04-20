library(dplyr)
library(tidyr)
library(ggmap)

setwd(gsub("\\\\","//", readClipboard()))


#UC_stand <- read.csv("UC_stand.csv")
UC_add_dic <- read.csv("D:/Github/CSP571/Project/campus_safety/Address dictionary_UChicago.csv")
IIT_Crime <- iitCrime # Use file iitcrime_with_add in campus_safety


#function for getting lat and long based on address
coordinates <- function(x){
  address <- paste(x,"Chicago, IL", sep = ",")
  temp<-geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  while(temp$status == "OVER_QUERY_LIMIT"){
    
    Sys.sleep(3)
    temp = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    
  }
  if(temp$status == "ZERO_RESULTS")#if there is no result
  {
    return(NA)
  }
  
  Latitude <- temp$results[[1]]$geometry$location$lat #returns lat
  Longitude <- temp$results[[1]]$geometry$location$lng #returns long
  
  # if (as.numeric(Latitude) > 41.81856|as.numeric(Latitude) < 41.7606 ) {
  #   
  #   return(NA)
  #   
  # }
  # if (as.numeric(Longitude)> -87.56129|as.numeric(Longitude)< -87.64057) {
  #   
  #   return(NA)
  # }
  # else
  # {
   return(paste(Latitude,Longitude,sep = ","))
  # }
}

# UC Lat long 
UC_add_dic <- UC_add_dic %>% separate(Location.Address ,c("Add1", "Add2"), ",", remove = F)

one_point <- which(is.na(UC_add_dic$Add2))

two_point <- which(!is.na(UC_add_dic$Add2))

UC_add_dic$lat_long1 <- lapply(UC_add_dic$Add1, coordinates)

UC_add_dic$lat_long2[two_point] <- lapply(UC_add_dic$Add2[two_point], coordinates)

UC_add_dic <- UC_add_dic %>% separate(lat_long1, c("Latitude1", "Longitude1") , ",", remove = F)

UC_add_dic <- UC_add_dic %>% separate(lat_long2, c("Latitude2", "Longitude2") , ",", remove = F)

UC_add_dic$Latitude1 <- as.numeric(UC_add_dic$Latitude1)
UC_add_dic$Latitude2 <- as.numeric(UC_add_dic$Latitude2)

UC_add_dic$Longitude1 <- as.numeric(UC_add_dic$Longitude1)
UC_add_dic$Longitude2 <- as.numeric(UC_add_dic$Longitude2)

UC_add_dic <- UC_add_dic[,-which(names(UC_add_dic) %in% c("lat_long1","lat_long2"))]


#Function for calculating lat long withing bounds and take average of two addresses
final_lat_long <- function(Latitude1,Latitude2,Longitude1,Longitude2)
{
  Latitude <- NA
  Longitude <- NA
  
  if(is.na(Latitude1)|is.na(Longitude1))
  {
    if (is.na(Latitude2)|is.na(Longitude2)) 
      {
        Latitude <- NA
        Longitude <- NA
        return(c(Latitude,Longitude))
      }
    else
    {
      if ((Latitude2 < 41.7606)|(Latitude2 > 41.81856)|(Longitude2< -87.64057)|(Longitude2 > -87.56129)) 
        {
          Latitude <- NA
          Longitude <- NA
          return(c(Latitude,Longitude))
        }
      else
        {
          Latitude <- Latitude2
          Longitude <- Longitude2
          return(c(Latitude,Longitude))
        }
      
    }
  }
    
    if(is.na(Latitude2)|is.na(Longitude2)) 
      {
        if((Latitude1 < 41.7606)|(Latitude1 > 41.81856)|(Longitude1 < -87.64057)|(Longitude1 > -87.56129))
          {
            Latitude <- NA
            Longitude <- NA
            return(c(Latitude,Longitude))
            
          }
        else
        {
          Latitude <- Latitude1
          Longitude <- Longitude1
          return(c(Latitude,Longitude))
        }
          
      }
    else
    {
      if((Latitude1 < 41.7606)|(Latitude1 > 41.81856)|(Longitude1 < -87.64057)|(Longitude1 > -87.56129))
      {
        if((Latitude2 < 41.7606)|(Latitude2 > 41.81856)|(Longitude2 < -87.64057)|(Longitude2 > -87.56129))
        {
          Latitude <- NA
          Longitude <- NA
          return(c(Latitude,Longitude))
        }
        else
        {
          Latitude <- Latitude2
          Longitude <- Longitude2
          return(c(Latitude,Longitude))
        }
      }
      else
      {
        if((Latitude2 < 41.7606)|(Latitude2 > 41.81856)|(Longitude2 < -87.64057)|(Longitude2 > -87.56129))
        {
          Latitude <- Latitude1
          Longitude <- Longitude1
          return(c(Latitude,Longitude))
        }
        else
        {
          Latitude <- round(((Latitude1+Latitude2)/2),8)
          Longitude <- round(((Longitude1+Longitude2)/2),8)
          return(c(Latitude,Longitude))
        }
      }
    }
  
  #return(c(Latitude,Longitude))
}

test <- t(mapply(final_lat_long,UC_add_dic[,'Latitude1'],UC_add_dic[,'Latitude2'],UC_add_dic[,'Longitude1'],UC_add_dic[,'Longitude2']))

colnames(test)[c(1,2)] <- c("Latitude","Longitude")

test <- as.data.frame(test)

UC_add_dic$Latitutde <- test$Latitude
UC_add_dic$Longitude <- test$Longitude

#1556,1557,1558, 15527<- NA

#IIT_Crime %>% separate(IIT_Crime$Lat_Long, c(Lat,Long))


# Lat long for IIT
head(IIT_Crime)

IIT_Crime$lat_long <- lapply(IIT_Crime$Address, coordinates)

miss_add <- which(is.na(IIT_Crime$Address))

#176 missing addresses

park_lot <- which(!(is.na(IIT_Crime$Latitude)))

names(IIT_Crime)[4] <- "Lat"
names(IIT_Crime)[5] <- "Long"

IIT_Crime$lat_long[miss_add] <- NA

#separating the Latitude and Longitude
IIT_Crime <- IIT_Crime %>% separate(lat_long,c("Latitude", "Longitude"), ",", remove = F )

IIT_Crime$Latitude <- as.numeric(IIT_Crime$Latitude)
IIT_Crime$Longitude <- as.numeric(IIT_Crime$Longitude)

#Taking parking lot location directly as it was found earlier
IIT_Crime$Latitude[park_lot] <- IIT_Crime[park_lot,"Lat"] 
IIT_Crime$Longitude[park_lot] <- IIT_Crime[park_lot,"Long"]


beyond_bound <- function(Latitude,Longitude)
{
  flag <- NA
  if (is.na(Latitude)|is.na(Longitude)) 
    {
      flag = NA
      return(flag)
    }
  else{
    
  
  if(((Latitude <= 41.8603) & (Latitude >= 41.8025)) & ((Longitude <= -87.58759) & (Longitude >= -87.66687))) 
    {
      flag = "CORRECT"
      return(flag)
    }
  else
  {
    
  if(((Latitude <= 41.89363) & (Latitude >= 41.86465)) & ((Longitude <= -87.62239) & (Longitude >= -87.66203)))
    {
      flag = "CORRECT"
      return(flag)
    }
    else
    {
      flag = "INCORRECT"
      return(flag)
    }
  }
  
  }
 
}

test1 <- mapply(beyond_bound, IIT_Crime[,"Latitude"],IIT_Crime[,"Longitude"])

test1 <- as.vector(test1)

wrong_lat_long <- which(test1=="INCORRECT")

IIT_Crime[wrong_lat_long,]

# Address to be changed   
                        # > 32nd   and  south state str to State & 32nd Street
                        # > 3100 south to 3100 South State street
                        # > 3300 south to 3300 South State Street
                        # > 3500 south to 3500 South State Street
#OTHER CAMPUS                        
                        # > 201 e loop rd, wheaton, il is RICE campus
                        # > 6502 south archer rd. bedford park, il is moffett campus
                        
incorrect1 <- which(IIT_Crime$Address == "32nd   and  south state str")
incorrect2 <- which(IIT_Crime$Address == "3100 south")
incorrect3 <- which(IIT_Crime$Address == "3300 south")
incorrect4 <- which(IIT_Crime$Address == "3500 south")

levels(IIT_Crime$Address) <- c(levels(IIT_Crime$Address),"State & 32nd Street","3100 South State street","3300 South State Street","3500 South State Street")
IIT_Crime$Address[incorrect1] <- "State & 32nd Street"
IIT_Crime$Address[incorrect2] <- "3100 South State street"
IIT_Crime$Address[incorrect3] <- "3300 South State Street"
IIT_Crime$Address[incorrect4] <- "3500 South State Street"



all_incor <- c(incorrect1,incorrect2,incorrect3,incorrect4)

IIT_Crime$lat_long[all_incor] <- lapply(IIT_Crime$Address[all_incor],coordinates)

IIT_Crime <- IIT_Crime %>% separate(lat_long,c("Latitude", "Longitude"), ",", remove = F)

IIT_Crime$Latitude <- as.numeric(IIT_Crime$Latitude)
IIT_Crime$Longitude <- as.numeric(IIT_Crime$Longitude)


IIT_Crime$lat_long <- as.character(IIT_Crime$lat_long)

write.csv(IIT_Crime,file = "IIT_Crime_to_finstand.csv")


