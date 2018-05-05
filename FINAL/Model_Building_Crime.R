getwd()

#Load IIT crime data file

load("IIT_FINAL_AGG.Rda")

prop.table(table(IIT_FINAL_AGG$INCIDENT_TYPE2))
#initial analysis
nrow(IIT_FINAL_AGG)
ncol(IIT_FINAL_AGG)
summary(IIT_FINAL_AGG)
View(IIT_FINAL_AGG)

#the data is currently not ordered chronologically. first lets order it based on occured date and time
install.packages("chron")
?gh_encode
library(chron)
?as.POSIXct()

# sample <- IIT_FINAL_AGG
# sample$OCCURED <- as.POSIXct(sample$OCCURED)
# sample <- sample[order(sample$OCCURED),]
# View(sample)

#sample <- IIT_FINAL_AGG
IIT_FINAL_AGG$OCCURED <- as.POSIXct(IIT_FINAL_AGG$OCCURED)
IIT_FINAL_AGG <- IIT_FINAL_AGG[order(IIT_FINAL_AGG$OCCURED),]

#Changing index labels 
IIT_FINAL_AGG$X1 <- c(1:nrow(IIT_FINAL_AGG))
names(IIT_FINAL_AGG)[1] <- 'Index'

#Ordering is done . Lets change the occured to numeric to be added as predictor variable
IIT_FINAL_AGG$OCCURED <- as.numeric(IIT_FINAL_AGG$OCCURED)
#Ordering is done .. now need to convert necessary labels to factor types


#First is incident type
levels(as.factor(IIT_FINAL_AGG$INCIDENT_TYPE2)) # 5 levels
#lets build two classes out of the incidents
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE2=="NON CRIMINAL")] <- "MILD INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE2=="PROPERTY CRIME")] <- "SERIOUS INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE2=="SUBSTANCE CRIME")] <- "SERIOUS INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE2=="PERSON CRIME")] <- "SERIOUS INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE2=="SERIOUS CRIME")] <- "SERIOUS INCIDENTS"

IIT_FINAL_AGG$INCIDENT_TYPE2 <- as.factor(IIT_FINAL_AGG$INCIDENT_TYPE2)
#class(IIT_FINAL_AGG$INCIDENT_TYPE2)
#original_incidents_list <- IIT_FINAL_AGG$INCIDENT_TYPE1
#Removing the incdient type 1 columns since incident type 2 is there .. will plan to incorporate it
# while training the model to study the change in accuracy

#Now lets look at the sector data

levels(as.factor(IIT_FINAL_AGG$SECTOR)) #20 labels. This is our target variable


#lets check the same for month, day and time buckets

levels(as.factor(IIT_FINAL_AGG$MONTH))
levels(as.factor(IIT_FINAL_AGG$DAY))
levels(as.factor(IIT_FINAL_AGG$TIME_BUCKET))

IIT_FINAL_AGG$TYPE_OF_DATA<-as.factor(IIT_FINAL_AGG$TYPE_OF_DATA)
IIT_FINAL_AGG$MONTH <- as.factor(IIT_FINAL_AGG$MONTH)
IIT_FINAL_AGG$DAY <- as.factor(IIT_FINAL_AGG$DAY)
IIT_FINAL_AGG$TIME_BUCKET <- as.factor(IIT_FINAL_AGG$TIME_BUCKET)

#Great .. lets get into weather now

levels(as.factor(IIT_FINAL_AGG$COND))#32 levels
levels(as.factor(IIT_FINAL_AGG$STAND_COND)) #7 levels
levels(as.factor(IIT_FINAL_AGG$SEVERITY)) # 3 levels

#we can do the same as we did for incident types . Removing the conditions and keep the standard 
#conditions as factors

#weather_cond <- IIT_FINAL_AGG$COND 
#IIT_FINAL_AGG$COND <- NULL
IIT_FINAL_AGG$COND <- as.factor(IIT_FINAL_AGG$COND)
IIT_FINAL_AGG$STAND_COND <- as.factor(IIT_FINAL_AGG$STAND_COND)
IIT_FINAL_AGG$SEVERITY <- as.factor(IIT_FINAL_AGG$SEVERITY)

#cool .. lets look at summary once more
summary(IIT_FINAL_AGG)

#lets look at percent of Null values before proceeding
options(scipen = 999)
Null_Counter <- apply(IIT_FINAL_AGG, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "999"  | x == "?"))/length(x))

#Ok , there are couple of NA values in the data. Lets start with Occured

sample <- IIT_FINAL_AGG

#removed rows with NA values in occured column
IIT_FINAL_AGG <- IIT_FINAL_AGG[complete.cases(IIT_FINAL_AGG[,4]),]

#now lets check summary again

summary(IIT_FINAL_AGG)

# now there are NAs only in stand_cond, severity and wind
#summary(IIT_FINAL_AGG)
NA_rows <- IIT_FINAL_AGG[rowSums(is.na(IIT_FINAL_AGG)) > 0 ,]
View(NA_rows)
sample <- IIT_FINAL_AGG
#removing rows with unknown weather condition
IIT_FINAL_AGG <- IIT_FINAL_AGG[!(IIT_FINAL_AGG$COND == "Unknown"),]
#deleting row with wrong date

IIT_FINAL_AGG <- IIT_FINAL_AGG[-1,]

summary(IIT_FINAL_AGG$WIND)
#lets plug NA values in wind with mean

sample <- IIT_FINAL_AGG
IIT_FINAL_AGG[is.na(IIT_FINAL_AGG[,19]), 19] <- colMeans(IIT_FINAL_AGG[,19], na.rm = TRUE)
class(IIT_FINAL_AGG$WIND)
names(IIT_FINAL_AGG[,18])

#there are two remaining unknown locations in Locations column. Lets place them as "No Location name'
IIT_FINAL_AGG[is.na(IIT_FINAL_AGG[,2]), 2] <- "No Location Specified"

#Before moving to model building , let change the SECTOR to factor variables as well
sample <- IIT_FINAL_AGG
IIT_FINAL_AGG$SECTOR <- as.factor(IIT_FINAL_AGG$SECTOR)
IIT_FINAL_AGG <- as.data.frame(IIT_FINAL_AGG)

levels(IIT_FINAL_AGG$INCIDENT_TYPE2) <- c("MILD_INCIDENTS", "SERIOUS_INCIDENTS")
# IIT_FINAL_AGG$SECTOR <- as.factor(IIT_FINAL_AGG$SECTOR)
# levels(IIT_FINAL_AGG$SECTOR)
# hist(as.numeric(IIT_FINAL_AGG$SECTOR))
# table(as.numeric(IIT_FINAL_AGG$SECTOR))
#Great !! All NA values are dealt with in this case. Now its model building time

#first let build a model solely for IIT campus
class(IIT_FINAL_AGG)
#simple training model - Yet to be tested
simple_model_testing <- function(data_input,target,xVars ,model_name) {
  modelForm<-createModelFormula(target,xVars)
  train<-data_input[1:(.8*length(data_input$Index)),]
  test<-data_input[(.8*length(data_input$Index)):length(data_input$Index),]
  fit <- model_name(modelForm, train)
  pred<-predict(fit, test)
  print(confusionMatrix(pred,test[,target]))
  
}

targetVar<-'SECTOR'
xVars<-colnames(IIT_FINAL_AGG)[c(4,8:9,11:20)]
simple_model_testing(IIT_FINAL_AGG, targetVar, xVars, naiveBayes)
#ACCURACY OF 33.78 AND BALANCED ACCURACY OF 51 PERCENT for entire sector data


#Naive Bayes on IIT campus data alone for Sector info

iitdata <- IIT_FINAL_AGG[IIT_FINAL_AGG$TYPE_OF_DATA == "IIT-CAMPUS",]
iitdata$SECTOR <- factor(iitdata$SECTOR)
View(iitdata)

simple_model_testing(iitdata, targetVar, xVars, naiveBayes)

#accuracy of 53.52 and balanced accuracy of 51 percent

#Naive Bayes on Incident Type on entire data

targetVar<-'INCIDENT_TYPE2'
xVars<-colnames(IIT_FINAL_AGG)[c(4,6:8,10:20)]

simple_model_testing(IIT_FINAL_AGG, targetVar, xVars, naiveBayes)
#55.21 percent total accuracy, while 95.42 percent total accuracy

accuracy.meas(test$INCIDENT_TYPE2,NB_pred)
roc.curve(test$INCIDENT_TYPE2,NB_pred)
#55.2

#Applying naive bayes on incident data for iit campus

simple_model_testing(iitdata, targetVar, xVars, naiveBayes)
#52 percent accuracy, #52.7percent ROC curve area

accuracy.meas(test$INCIDENT_TYPE2,NB_pred)
roc.curve(test$INCIDENT_TYPE2,NB_pred, plotit = F)

#Now , lets apply geo hashing with precision 6

IIT_FINAL_AGG$GeoHash <- apply(IIT_FINAL_AGG,1,
                               function(x) 
                                 return(gh_encode(as.double(x[6]), as.double(x[7]), precision=6)))


IIT_FINAL_AGG$GeoHash <- as.factor(IIT_FINAL_AGG$GeoHash)
levels(IIT_FINAL_AGG$GeoHash)
targetVar<-'GeoHash'

xVars<-colnames(IIT_FINAL_AGG)[c(4,8:20)]

simple_model_testing(IIT_FINAL_AGG, targetVar, xVars, naiveBayes)
#the accuracy levels again went down


#lets try geohashing with IIT area
iitdata <- IIT_FINAL_AGG[IIT_FINAL_AGG$TYPE_OF_DATA == "IIT-CAMPUS",]
iitdata$GeoHash <- factor(iitdata$GeoHash)
levels(iitdata$GeoHash)
targetVar<-'GeoHash'
xVars<-colnames(IIT_FINAL_AGG)[c(4,9, 11:20)]

decoded_values = c()
for (i in levels(iitdata$GeoHash)) {
  decoded_values <- c(decoded_values, gh_decode(i))
}

decoded_values <- lapply(levels(iitdata$GeoHash), gh_decode)
decoded_values
simple_model_testing(iitdata, targetVar, xVars, naiveBayes)
#accuracy is not so good again

#lets drop all the kent campus sector values
filtered_IIT_AGG <- IIT_FINAL_AGG[!(IIT_FINAL_AGG$SECTOR==17 | 
                                      IIT_FINAL_AGG$SECTOR ==18 |
                                      IIT_FINAL_AGG$SECTOR == 19 |
                                      IIT_FINAL_AGG$SECTOR == 20), ]

#Another variation would be to train individually for each sector, and use those for
View(sector_1)
sector_20<- IIT_FINAL_AGG[IIT_FINAL_AGG$SECTOR==20,]
sector_20$GeoHash <- factor(sector_20$GeoHash)
levels(sector_11$GeoHash)
#testing for a sample sector . Some sectors have good accuracy. while others dont
simple_model_testing(sector_20, targetVar, xVars, naiveBayes)
#accuracy levels are again bizzare

prop.table(table(IIT_FINAL_AGG$INCIDENT_TYPE2))
#so i believe we should go with crime classification and related strategies

#installing sampling packages to reduce effects due to imbalanced classes

install.packages("ROSE")
library(ROSE)
table(train$INCIDENT_TYPE2)
nrow(train)

targetVar<-'INCIDENT_TYPE2'
xVars<-colnames(IIT_FINAL_AGG)[c(4,6:8,10:20)]
modelForm<-createModelFormula(targetVar,xVars)

levels(train$INCIDENT_TYPE2)
train<-IIT_FINAL_AGG[1:(.8*length(IIT_FINAL_AGG$Index)),]
test<-IIT_FINAL_AGG[(.8*length(IIT_FINAL_AGG$Index)):length(IIT_FINAL_AGG$Index),]
#oversampling
data_balanced_over <- ovun.sample(modelForm, data = train, method = "over",N = 196416)$data
table(data_balanced_over$INCIDENT_TYPE2)

#undersampling
data_balanced_under <- ovun.sample(modelForm, data = train, method = "under", N = 10936, seed = 1)$data

table(data_balanced_under$INCIDENT_TYPE2)

#both undersampling and oversampling

data_balanced_both <- ovun.sample(modelForm, data = train, method = "both", p=0.5, N=196416, seed = 1)$data

table(data_balanced_both$INCIDENT_TYPE2)

class(train$COND)
#synthetic generation of data
data.rose <- ROSE(modelForm, data = train, seed = 1)$data
table(data.rose$INCIDENT_TYPE2)

#build naive bayes models

nb.rose <- naiveBayes(modelForm, data = data.rose)
nb.over <- naiveBayes(modelForm, data = data_balanced_over)
nb.under <- naiveBayes(modelForm, data = data_balanced_under)
nb.both <- naiveBayes(modelForm, data = data_balanced_both)

#make predictions

pred.nb.rose <- predict(nb.rose, newdata = test)
pred.nb.over <- predict(nb.over, newdata = test)
pred.nb.under <- predict(nb.under, newdata = test)
pred.nb.both <- predict(nb.both, newdata = test)

head(pred.tree.rose)
head(pred.tree.rose)
accuracy.meas(test$INCIDENT_TYPE2,pred.nb.rose)
roc.curve(test$INCIDENT_TYPE2, pred.nb.rose)

accuracy.meas(test$INCIDENT_TYPE2,pred.nb.over)
roc.curve(test$INCIDENT_TYPE2,pred.nb.over)

accuracy.meas(test$INCIDENT_TYPE2,pred.nb.under)
roc.curve(test$INCIDENT_TYPE2,pred.nb.under)

accuracy.meas(test$INCIDENT_TYPE2,pred.nb.both)
roc.curve(test$INCIDENT_TYPE2,pred.nb.both)

#the balanced accuracy has improved. best ROC area of 64.7 percent

#lets try some more techniques
install.packages("pROC")
library(pROC)
# ctrl <- trainControl(method = "repeatedcv",
#                      number = 10,
#                      repeats = 5,
#                      summaryFunction = twoClassSummary,
#                      classProbs = TRUE)
# 
# orig_fit <- train(modelForm,
#                   data = train,
#                   method = "gbm",
#                   verbose = FALSE,
#                   metric = "ROC",
#                   trControl = ctrl)

#Using treebag model in caret to evaluate the incident type

ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(modelForm, data = train, method = "treebag",
                 trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

#------------------------------------------------------------------------------------------------#

#WE ARE STARTING AGAIN ON IIT-ONLY CURTAILED DATA FOR CAMPUS AND AREA

#load iit only
load("iit_only.rda")
summary(iit3)

View(iit3)

IIT_FINAL_AGG <- iit3

prop.table(table(IIT_FINAL_AGG$INCIDENT_TYPE2))
prop.table(table(IIT_FINAL_AGG$INCIDENT_TYPE1))


IIT_FINAL_AGG$OCCURED <- as.POSIXct(IIT_FINAL_AGG$OCCURED)
IIT_FINAL_AGG <- IIT_FINAL_AGG[order(IIT_FINAL_AGG$OCCURED),]

#Changing index labels 
IIT_FINAL_AGG$X1 <- c(1:nrow(IIT_FINAL_AGG))
names(IIT_FINAL_AGG)[1] <- 'Index'

#Ordering is done . Lets change the occured to numeric to be added as predictor variable
IIT_FINAL_AGG$OCCURED <- as.numeric(IIT_FINAL_AGG$OCCURED)
#Ordering is done .. now need to convert necessary labels to factor types


#First is incident type
levels(as.factor(IIT_FINAL_AGG$INCIDENT_TYPE1)) # 5 levels
#lets build two classes out of the incidents
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="ARSON")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="ASSAULT")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="BATTERY")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="HOMICIDE")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="KIDNAPPING")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="MISSING PERSON")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="NARCOTICS")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="OFFENSE INVOLVING CHILDREN")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="ROBBERY")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="SEXUAL CRIME")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="THEFT")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="WEAPON")] <- "SERIOUS_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="ACCIDENT")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="ALARM")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="DAMAGE TO PROPERTY")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="DISTURBANCE")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="LIQUOR LAW VIOLATION")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="MEDICAL INCIDENT")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="OTHER")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="STALKING")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="SUSPICION")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="TRESPASS")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="UTILITY INCIDENT")] <- "MILD_INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE1=="WELL BEING CHECK")] <- "MILD_INCIDENTS"

prop.table(table(IIT_FINAL_AGG$INCIDENT_TYPE2))
IIT_FINAL_AGG$INCIDENT_TYPE2 <- as.factor(IIT_FINAL_AGG$INCIDENT_TYPE2)
#class(IIT_FINAL_AGG$INCIDENT_TYPE2)
#original_incidents_list <- IIT_FINAL_AGG$INCIDENT_TYPE1
#Removing the incdient type 1 columns since incident type 2 is there .. will plan to incorporate it
# while training the model to study the change in accuracy

#Now lets look at the sector data

levels(as.factor(IIT_FINAL_AGG$SECTOR)) 


#lets check the same for month, day and time buckets

levels(as.factor(IIT_FINAL_AGG$MONTH))
levels(as.factor(IIT_FINAL_AGG$DAY))
levels(as.factor(IIT_FINAL_AGG$TIME_BUCKET))

IIT_FINAL_AGG$TYPE_OF_DATA<-as.factor(IIT_FINAL_AGG$TYPE_OF_DATA)
IIT_FINAL_AGG$MONTH <- as.factor(IIT_FINAL_AGG$MONTH)
IIT_FINAL_AGG$DAY <- as.factor(IIT_FINAL_AGG$DAY)
IIT_FINAL_AGG$TIME_BUCKET <- as.factor(IIT_FINAL_AGG$TIME_BUCKET)

#Great .. lets get into weather now

levels(as.factor(IIT_FINAL_AGG$COND))#32 levels
levels(as.factor(IIT_FINAL_AGG$STAND_COND)) #7 levels
levels(as.factor(IIT_FINAL_AGG$SEVERITY)) # 3 levels

#we can do the same as we did for incident types . Removing the conditions and keep the standard 
#conditions as factors

#weather_cond <- IIT_FINAL_AGG$COND 
#IIT_FINAL_AGG$COND <- NULL
IIT_FINAL_AGG$COND <- as.factor(IIT_FINAL_AGG$COND)
IIT_FINAL_AGG$STAND_COND <- as.factor(IIT_FINAL_AGG$STAND_COND)
IIT_FINAL_AGG$SEVERITY <- as.factor(IIT_FINAL_AGG$SEVERITY)

#cool .. lets look at summary once more
summary(IIT_FINAL_AGG)

#lets look at percent of Null values before proceeding
options(scipen = 999)
Null_Counter <- apply(IIT_FINAL_AGG, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "999"  | x == "?"))/length(x))

#Ok , there are couple of NA values in the data. Lets start with Occured

sample <- IIT_FINAL_AGG


#now lets check summary again

summary(IIT_FINAL_AGG)

# now there are NAs only in stand_cond, severity and wind
#summary(IIT_FINAL_AGG)
NA_rows <- IIT_FINAL_AGG[rowSums(is.na(IIT_FINAL_AGG)) > 0 ,]
View(NA_rows)
sample <- IIT_FINAL_AGG
#there are two remaining unknown locations in Locations column. Lets place them as "No Location name'
IIT_FINAL_AGG[is.na(IIT_FINAL_AGG[,2]), 2] <- "No Location Specified"

#Before moving to model building , let change the SECTOR to factor variables as well
sample <- IIT_FINAL_AGG
IIT_FINAL_AGG$SECTOR <- as.factor(IIT_FINAL_AGG$SECTOR)
IIT_FINAL_AGG <- as.data.frame(IIT_FINAL_AGG)


# IIT_FINAL_AGG$SECTOR <- as.factor(IIT_FINAL_AGG$SECTOR)
# levels(IIT_FINAL_AGG$SECTOR)
# hist(as.numeric(IIT_FINAL_AGG$SECTOR))
# table(as.numeric(IIT_FINAL_AGG$SECTOR))
#Great !! All NA values are dealt with in this case. Now its model building time

prop.table(table(IIT_FINAL_AGG$SECTOR))
targetVar<-'INCIDENT_TYPE2'
xVars<-colnames(IIT_FINAL_AGG)[c(4,10:19,21)]

#simple_model_testing(IIT_FINAL_AGG, targetVar, xVars, naiveBayes)


modelForm<-createModelFormula(targetVar,xVars)
train<-IIT_FINAL_AGG[1:(.8*length(IIT_FINAL_AGG$Index)),]
test<-IIT_FINAL_AGG[(.8*length(IIT_FINAL_AGG$Index)):length(IIT_FINAL_AGG$Index),]

View(test)
nrow(train) + nrow(test) == nrow(IIT_FINAL_AGG)
prop.table(table(train$INCIDENT_TYPE2))
prop.table(table(test$INCIDENT_TYPE2))

#using naive bayes
simple_model_testing(IIT_FINAL_AGG,targetVar, xVars, naiveBayes)


#using decision trees
fit <- rpart(modelForm, train)
summary(fit)
fancyRpartPlot(fit)
pred<-predict(fit, test, type="class")
print(confusionMatrix(pred,test[,targetVar]))


#using random forests
fit <- randomForest(modelForm, data=train )
pred<-predict(fit, test)
head(pred)
print(confusionMatrix(pred,test[,targetVar]))


#lets add the geohash values to encode the lat and lons for adding to the model and see accuracy
IIT_FINAL_AGG$GeoHash <- apply(IIT_FINAL_AGG,1,
                               function(x) 
                                 return(gh_encode(as.double(x[6]), as.double(x[7]), precision=7)))
table(IIT_FINAL_AGG$GeoHash)
length(levels(as.factor(IIT_FINAL_AGG$GeoHash)))
IIT_FINAL_AGG$GeoHash <- as.factor(IIT_FINAL_AGG$GeoHash)
targetVar<-'INCIDENT_TYPE2'
xVars<-colnames(IIT_FINAL_AGG)[c(4, 8,10:21)]

#simple_model_testing(IIT_FINAL_AGG, targetVar, xVars, naiveBayes)
simple_model_testing(IIT_FINAL_AGG,targetVar, xVars, naiveBayes)

modelForm<-createModelFormula(targetVar,xVars)

#using decision trees
fit <- rpart(modelForm, train)
summary(fit)
fancyRpartPlot(fit)
pred<-predict(fit, test, type="class")
print(confusionMatrix(pred,test[,targetVar]))

gh_decode("dp3wn0d")
iit_data_sample <- IIT_FINAL_AGG[IIT_FINAL_AGG$SECTOR==1,]
table(iit_data_sample$GeoHash)
#function that extracts month from the date and converts to the factor required

val <- 'january'
substring(val,1,3)
levels(IIT_FINAL_AGG$TIME_BUCKET)
library(lubridate)

extract_month <- function(date) {
  month_val <- months(date)
  #print(month_val)
  return(substring(month_val,1,3))
}

levels(IIT_FINAL_AGG$DAY)
extract_days <- function(date) {
  days_val <- weekdays(date)
  #print(month_val)
  return(substring(days_val,1,3))
}


#function that creates the crime prediction dataset (Need to add lats and lons to the set)
crime_prediction_model <- function(date_range) {
  
  sector_datetime <- c()
  sector_range <- c()
  sector_timebucket <- c()
  sector_month <- c()
  sector_day <- c()
  sector_typeofarea <- c()
  sector_geohash <- c()
  
  for (i in 1:date_range) {
    if (i == 1) 
      date_value <- Sys.Date() + days(1)
    else
      date_value <- as.Date(date_value) + days(1)
    #print("yes")
    month_val <- extract_month(date_value)
    day_val <- extract_days(date_value)
    sector_geohash <- c(sector_geohash, unlist(lapply(levels(IIT_FINAL_AGG$GeoHash), function(x) rep(x,8))))
    sector_month <- c(sector_month, rep(month_val, length(sector_geohash)) )
    sector_day<- c(sector_day, rep(day_val, length(sector_geohash)) )
    #sector_range <- c(sector_range,unlist(lapply(1:8, function(x) rep(x,8))))
    sector_timebucket <- c(sector_timebucket,rep(levels(IIT_FINAL_AGG$TIME_BUCKET), 132))
    #sector_typeofarea <- c(sector_typeofarea, rep(z, 64))
    
    start <- as.POSIXct(paste(date_value, "01:30:00", sep = " "))
    #print(start)
    interval <- 60
    end <- start + as.difftime(0.95, units="days")
    sector_datetime<- c(sector_datetime, rep(seq(from=start, by=interval*180, to=end),132))
    #print(sector_datetime)
    #print(length(sector_datetime))
    
  }
  
  
  
  test_dataset <- data.frame(as.numeric(sector_datetime),as.factor(sector_timebucket),as.factor(sector_month), as.factor(sector_day), as.factor(sector_geohash))
  test_dataset[,3] <- ordered(test_dataset[,3])
  test_dataset[,4] <- ordered(test_dataset[,4])
  names(test_dataset) <- c("OCCURED", "TIME_BUCKET","MONTH", "DAY",  "GEOHASH")
  test_dataset[,'LATITUDE'] <- apply(test_dataset, 1, function(x) return(as.numeric(gh_decode(x[5])$lat)))
  test_dataset[,'LONGITUDE'] <- apply(test_dataset,1,  function(x) return(as.numeric(gh_decode(x[5])$lng)))
  
  return(test_dataset)  
}

remove(test_dataset)
future_test <- crime_prediction_model(1)
summary(future_test)
View(future_test)
apply(future_test, 2, class)
save(future_test, file="future_test.rda")
for (i in 1:ncol(future_test)) {
  print(class(future_test[,i]))
}
targetVar<-'INCIDENT_TYPE2'
xVars<-colnames(IIT_FINAL_AGG)[c(4,6:7, 10:20)]
modelForm<-createModelFormula(targetVar,xVars)
fit <- rpart(modelForm, train)
summary(fit)
test$PREDICTED <- predict(fit, test, type="class")
confusionMatrix(test$PREDICTED, test$INCIDENT_TYPE2)


#for calculating the ration of serious incidents to total incidents
for (i in 1:8 ) {
  sector_data <- test[test$SECTOR == i,]   
  print(nrow(sector_data))
  table_val <- table(sector_data[,'PREDICTED'])
  serious_incidents <- table_val[names(table_val) == "SERIOUS_INCIDENTS"]
  print(paste(" Serious incident percent for sector", i, sep = " "))
  print((serious_incidents / nrow(sector_data))* 100)
  
}

sample_data <- IIT_FINAL_AGG[IIT_FINAL_AGG$TYPE_OF_DATA=="IIT-AREA",]

prop.table(table(sample_data$INCIDENT_TYPE2))

