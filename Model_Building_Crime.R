getwd()

#Load IIT crime data file

load("IIT_FINAL_AGG.Rda")

#initial analysis
nrow(IIT_FINAL_AGG)
ncol(IIT_FINAL_AGG)
summary(IIT_FINAL_AGG)
View(IIT_FINAL_AGG)

#the data is currently not ordered chronologically. first lets order it based on occured date and time
install.packages("chron")

library(chron)
?as.POSIXct()

# sample <- IIT_FINAL_AGG
# sample$OCCURED <- as.POSIXct(sample$OCCURED)
# sample <- sample[order(sample$OCCURED),]
# View(sample)

sample <- IIT_FINAL_AGG
IIT_FINAL_AGG$OCCURED <- as.POSIXct(IIT_FINAL_AGG$OCCURED)
IIT_FINAL_AGG <- IIT_FINAL_AGG[order(IIT_FINAL_AGG$OCCURED),]

#Changing index labels 
IIT_FINAL_AGG$X1 <- c(1:nrow(IIT_FINAL_AGG))
names(IIT_FINAL_AGG)[1] <- 'Index'

#Ordering is done .. now need to convert necessary labels to factor types


#First is incident type
levels(as.factor(IIT_FINAL_AGG$INCIDENT_TYPE2)) # 5 levels
IIT_FINAL_AGG$INCIDENT_TYPE2 <- as.factor(IIT_FINAL_AGG$INCIDENT_TYPE2)
class(IIT_FINAL_AGG$INCIDENT_TYPE2)
original_incidents_list <- IIT_FINAL_AGG$INCIDENT_TYPE1
#Removing the incdient type 1 columns since incident type 2 is there .. will plan to incorporate it
# while training the model to study the change in accuracy

#Now lets look at the sector data

levels(as.factor(IIT_FINAL_AGG$SECTOR)) #20 labels. This is our target variable

#Lets move the sector column to the end as part of the convention
sample <- IIT_FINAL_AGG
IIT_FINAL_AGG <- IIT_FINAL_AGG[,c(1:8, 10:19, 9)]


stopifnot(ncol(sample) == ncol(IIT_FINAL_AGG))



#lets check the same for month, day and time buckets

levels(as.factor(IIT_FINAL_AGG$MONTH))
levels(as.factor(IIT_FINAL_AGG$DAY))
levels(as.factor(IIT_FINAL_AGG$TIME_BUCKET))

IIT_FINAL_AGG$MONTH <- as.factor(IIT_FINAL_AGG$MONTH)
IIT_FINAL_AGG$DAY <- as.factor(IIT_FINAL_AGG$DAY)
IIT_FINAL_AGG$TIME_BUCKET <- as.factor(IIT_FINAL_AGG$TIME_BUCKET)

#Great .. lets get into weather now

levels(as.factor(IIT_FINAL_AGG$COND))#32 levels
levels(as.factor(IIT_FINAL_AGG$STAND_COND)) #7 levels
levels(as.factor(IIT_FINAL_AGG$SEVERITY)) # 3 levels

#we can do the same as we did for incident types . Removing the conditions and keep the standard 
#conditions as factors

weather_cond <- IIT_FINAL_AGG$COND 
IIT_FINAL_AGG$COND <- NULL

IIT_FINAL_AGG$STAND_COND <- as.factor(IIT_FINAL_AGG$STAND_COND)
IIT_FINAL_AGG$SEVERITY <- as.factor(IIT_FINAL_AGG$SEVERITY)

#cool .. lets look at summary once more
summary(IIT_FINAL_AGG)

#lets look at percent of Null values before proceeding
options(scipen = 999)
Null_Counter <- apply(IIT_FINAL_AGG, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "999" | x == "0" | x == "?"))/length(x))

#Ok , there are couple of NA values in the data. Lets start with Occured

sample <- IIT_FINAL_AGG

#removed rows with NA values in occured
IIT_FINAL_AGG <- IIT_FINAL_AGG[complete.cases(IIT_FINAL_AGG[,4]),]

#now lets check summary again

summary(IIT_FINAL_AGG)

# now there are NAs only in stand_cond, severity and wind
summary(IIT_FINAL_AGG)
NA_rows <- IIT_FINAL_AGG[rowSums(is.na(IIT_FINAL_AGG)) > 0 & IIT_FINAL_AGG$TYPE_OF_DATA == 'IIT-AREA',]
View(NA_rows)
