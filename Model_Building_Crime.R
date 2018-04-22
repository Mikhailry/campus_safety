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
#class(IIT_FINAL_AGG$INCIDENT_TYPE2)
#original_incidents_list <- IIT_FINAL_AGG$INCIDENT_TYPE1
#Removing the incdient type 1 columns since incident type 2 is there .. will plan to incorporate it
# while training the model to study the change in accuracy

#Now lets look at the sector data

levels(as.factor(IIT_FINAL_AGG$SECTOR)) #20 labels. This is our target variable

#Lets move the sector column to the end as part of the convention
sample <- IIT_FINAL_AGG
IIT_FINAL_AGG <- IIT_FINAL_AGG[,c(1:9, 11:ncol(IIT_FINAL_AGG), 10)]


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

#weather_cond <- IIT_FINAL_AGG$COND 
#IIT_FINAL_AGG$COND <- NULL

IIT_FINAL_AGG$STAND_COND <- as.factor(IIT_FINAL_AGG$STAND_COND)
IIT_FINAL_AGG$SEVERITY <- as.factor(IIT_FINAL_AGG$SEVERITY)

#cool .. lets look at summary once more
summary(IIT_FINAL_AGG)

#lets look at percent of Null values before proceeding
options(scipen = 999)
Null_Counter <- apply(IIT_FINAL_AGG, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "999" | x == "0" | x == "?"))/length(x))

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
IIT_FINAL_AGG[is.na(IIT_FINAL_AGG[,18]), 18] <- colMeans(IIT_FINAL_AGG[,18], na.rm = TRUE)
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

#Getting IIT campus data in to a Dframe, we will exclude index, location, addrees, occured, incident type1,
#type of data, latitude, longitude, condition
View(IIT_FINAL_AGG)

iitdata <- IIT_FINAL_AGG[IIT_FINAL_AGG$TYPE_OF_DATA == 'IIT-CAMPUS',-c(1:8,13)]
#lets reduce the levels in SECTOR data first
iitdata$SECTOR <- factor(iitdata$SECTOR)

#lets split data in to training and testing in chronological order

row_id <- (0.8 * nrow(iitdata))
iitdata_train <- iitdata[1:(0.8 * nrow(iitdata)),]
iitdata_test <- iitdata[(row_id+1):nrow(iitdata),  ]
stopifnot(nrow(iitdata_train) + nrow(iitdata_test) == nrow(iitdata)) 

#first , let s fit a naive bayes model

library(e1071)
Bayes_model <- naiveBayes(SECTOR ~. , data = iitdata_train)
#lets predict it on test data
predict_nb <- predict(Bayes_model, iitdata_test)
#lets look at the confusion matrix
Actual <- iitdata_test$SECTOR
confusionMatrix(reference = Actual, data = predict_nb)


#lets run Naive bayes on entire data to see if i could mock his results


iitdata_complete <- IIT_FINAL_AGG[,-c(1:8,13)]
#lets reduce the levels in SECTOR data first


#lets split data in to training and testing in chronological order

row_id <- as.integer(0.8 * nrow(iitdata_complete))
iitdata_train_complete <- iitdata_complete[1:row_id,]
iitdata_test_complete <- iitdata_complete[(row_id+1):nrow(iitdata_complete),  ]
stopifnot(nrow(iitdata_train_complete) + nrow(iitdata_test_complete) == nrow(iitdata_complete)) 

#first , let s fit a naive bayes model

library(e1071)
Bayes_model <- naiveBayes(SECTOR ~. , data = iitdata_train_complete)
#lets predict it on test data
predict_nb_complete <- predict(Bayes_model, iitdata_test_complete)
#lets look at the confusion matrix
Actual <- iitdata_test_complete$SECTOR
confusionMatrix(reference = Actual, data = predict_nb_complete)























library(rpart)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(SECTOR ~. ,
             data=iitdata_train, 
            # control=rpart.control(minsplit=50, cp = 0.004),
             method="class")
?rpart
fit
summary(fit)
plot(fit)
fancyRpartPlot(fit)

predicted <- predict(fit, iitdata_test[,-ncol(iitdata_test)], type = "class")
length(predicted)
View(iitdata_test[,-ncol(iitdata_test)])

length(predicted)
table(predicted, iitdata_test$SECTOR)
Actual <- iitdata_test$SECTOR
levels(Actual)
length(Actual)
library(caret)

confusionMatrix(reference = Actual, data = predicted)
#Ok , well the results are not so encouraging. The decision tree is not splitting most probably due to
#lack of values for certain labels.

#Let us now run the model for the entire dataset

iitdata_complete <- IIT_FINAL_AGG[,-c(1:3,5:8,13)]

#lets split data in to training and testing in chronological order

row_id <- as.integer(0.8 * nrow(iitdata_complete))
iitdata_train <- iitdata_complete[1:row_id,]
iitdata_test <- iitdata_complete[(row_id+1):nrow(iitdata_complete),  ]
stopifnot(nrow(iitdata_train) + nrow(iitdata_test) == nrow(iitdata_complete)) 
View(iitdata_train)
class(iitdata_train)
#lets fit new decision tree model

fit_complete <- rpart(SECTOR ~. ,
             data=iitdata_train, 
             control=rpart.control(minsplit=2, cp = 0),
             method="class")

summary(fit_complete)
plot(fit)
fancyRpartPlot(fit)

predicted <- predict(fit_complete, iitdata_test[,-ncol(iitdata_test)], type = "class")
length(predicted)
View(iitdata_test[,-ncol(iitdata_test)])

length(predicted)
table(predicted, iitdata_test$SECTOR)
Actual <- iitdata_test$SECTOR
length(Actual)
confusionMatrix(reference = Actual, data = predicted)

#Large amount of overfitting is happening when prediction model is being created

#Lets try random forest
library(randomForest)
# x = iitdata_train[,-ncol(iitdata_train)]
# y = iitdata_train[,ncol(iitdata_train), drop=FALSE]
?randomForest
fit4 <- randomForest(SECTOR ~.
                     , data=iitdata_train )
                     #importance=TRUE,
                     # fit 2000 decision trees!
                     #ntree=2000)
predictions <- predict(fit4, iitdata_test[,-ncol(iitdata_test)])
confusionMatrix(reference=Actual, data=predictions)


remove(fit4)
hist(as.numeric(iitdata_complete$SECTOR))

#trying gradient boost
library(gbm)
install.packages("gbm")
iitdata_train$OCCURED <- as.numeric(iitdata_train$OCCURED)
View(iitdata_train)
?gbm
fit <- gbm(SECTOR ~. , data = iitdata_train , distribution = "multinomial")
predict.gbm <- function (object, newdata, n.trees, type = "link", single.tree = FALSE, ...) {
  if (missing(n.trees)) {
    if (object$train.fraction < 1) {
      n.trees <- gbm.perf(object, method = "test", plot.it = FALSE)
    }
    else if (!is.null(object$cv.error)) {
      n.trees <- gbm.perf(object, method = "cv", plot.it = FALSE)
    }
    else {
      n.trees <- length(object$train.error)
    }
    cat(paste("Using", n.trees, "trees...\n"))
    gbm::predict.gbm(object, newdata, n.trees, type, single.tree, ...)
  }
}
levels(iitdata_complete$SECTOR)
unique(iitdata_test$SECTOR)
levels(iitdata_test$SECTOR)
levels(iitdata_train$SECTOR)
table(iitdata_complete$SECTOR)
predictions <- predict(fit,iitdata_test[,-ncol(iitdata_test)], type = "response") 
                       #n.trees=fit$n.trees)

predictions
p.prediction <- apply(predictions, 1, function(x) colnames(predictions)[which.max(x)])
length(predictions)
length(p.prediction)
nrow(predictions)
predictions[1:3,,]
length(Actual)
table(p.prediction, Actual)
confusionMatrix(reference=Actual, data=p.prediction)
