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

# IIT_FINAL_AGG$SECTOR <- as.factor(IIT_FINAL_AGG$SECTOR)
# levels(IIT_FINAL_AGG$SECTOR)
# hist(as.numeric(IIT_FINAL_AGG$SECTOR))
# table(as.numeric(IIT_FINAL_AGG$SECTOR))
#Great !! All NA values are dealt with in this case. Now its model building time

#first let build a model solely for IIT campus
class(IIT_FINAL_AGG)

#split into 80%, 20%
train<-IIT_FINAL_AGG[1:(.8*length(IIT_FINAL_AGG$Index)),]
test<-IIT_FINAL_AGG[(.8*length(IIT_FINAL_AGG$Index)):length(IIT_FINAL_AGG$Index),]



#Naive Bayes

targetVar<-'INCIDENT_TYPE2'

xVars<-colnames(IIT_FINAL_AGG)[c(4,6:8,10:20)]
levels(test$INCIDENT_TYPE2)
#use naive bayes
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, train)
NB_pred<-predict(naiveBayesModel, test)
NB_pred
mean(NB_pred == test$GeoHash)
confusionMatrix(NB_pred,test$INCIDENT_TYPE2)

accuracy.meas(test$INCIDENT_TYPE2,NB_pred)
roc.curve(test$INCIDENT_TYPE2,NB_pred, plotit = F)


#installing sampling packages
install.packages("ROSE")
library(ROSE)
table(train$INCIDENT_TYPE2)
nrow(train)

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

#the balanced accuracy has improved.

#lets try some more techniques

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

