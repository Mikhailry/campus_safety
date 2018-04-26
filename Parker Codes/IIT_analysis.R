library(nnet)
library(MASS)
library(caret)
library(e1071)
load("~/Documents/Math 571 project/IIT_FINAL_AGG.Rda")
load("~/Documents/Math 571 project/iit_only.Rda")
IIT_FINAL_AGG<-na.omit(IIT_FINAL_AGG)

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = ' + ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = ' + '), -1))
  }
  return(modelForm)
}
#Categorize as serious or mild crime
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE2=="NON CRIMINAL")] <- "MILD INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE2=="PROPERTY CRIME")] <- "SERIOUS INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE2=="SUBSTANCE CRIME")] <- "SERIOUS INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE2=="PERSON CRIME")] <- "SERIOUS INCIDENTS"
IIT_FINAL_AGG$INCIDENT_TYPE2[which(IIT_FINAL_AGG$INCIDENT_TYPE2=="SERIOUS CRIME")] <- "SERIOUS INCIDENTS"
#Change chr to factor
IIT_FINAL_AGG$INCIDENT_TYPE1<-as.factor(IIT_FINAL_AGG$INCIDENT_TYPE1)
IIT_FINAL_AGG$INCIDENT_TYPE2<-as.factor(IIT_FINAL_AGG$INCIDENT_TYPE2)
IIT_FINAL_AGG$TYPE_OF_DATA<-as.factor(IIT_FINAL_AGG$TYPE_OF_DATA)
IIT_FINAL_AGG$DAY<-as.factor(IIT_FINAL_AGG$DAY)
IIT_FINAL_AGG$COND<-as.factor(IIT_FINAL_AGG$COND)
IIT_FINAL_AGG$STAND_COND<-as.factor(IIT_FINAL_AGG$STAND_COND)
IIT_FINAL_AGG$SEVERITY<-as.factor(IIT_FINAL_AGG$SEVERITY)
IIT_FINAL_AGG$SECTOR<-factor(IIT_FINAL_AGG$SECTOR)


#sort on time occured
attach(IIT_FINAL_AGG)
IIT_FINAL_AGG<-IIT_FINAL_AGG[order(OCCURED),]
IIT_FINAL_AGG$X1 <- c(1:nrow(IIT_FINAL_AGG))
detach(IIT_FINAL_AGG)

#split into 80%, 20%
train<-IIT_FINAL_AGG[1:(.8*length(IIT_FINAL_AGG$X1)),]
test<-IIT_FINAL_AGG[(.8*length(IIT_FINAL_AGG$X1)):length(IIT_FINAL_AGG$X1),]

#Naive Bayes for full campus
targetVar<-'SECTOR'
xVars<-c('INCIDENT_TYPE1', 'TYPE_OF_DATA', 'INCIDENT_TYPE2', 'MONTH', 'DAY', 'TIME_BUCKET', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP')
xVars<-c('TYPE_OF_DATA', 'INCIDENT_TYPE1', 'DAY', 'HUM')
#use naive bayes
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, train)
NB_pred<-predict(naiveBayesModel, test)
confusionMatrix(NB_pred,test$SECTOR)
#BEST=.3432
#No change: SEVERITY and WIND


#Off campus
offcamp<-IIT_FINAL_AGG[IIT_FINAL_AGG$TYPE_OF_DATA=='IIT-AREA',]
trainOff<-offcamp[1:(.8*length(offcamp$X1)),]
testOff<-offcamp[(.8*length(offcamp$X1)):length(offcamp$X1),]
targetVar<-'SECTOR'
xVars<-c('INCIDENT_TYPE1', 'INCIDENT_TYPE2', 'MONTH', 'DAY', 'TIME_BUCKET', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP')
xVars<-c( 'INCIDENT_TYPE1', 'DAY')
#use naive bayes
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, trainOff)
NB_pred<-predict(naiveBayesModel, testOff)
confusionMatrix(NB_pred,testOff$SECTOR)
#BEST=.3401
#next: 
#No change: HUM

#On campus
oncamp<-IIT_FINAL_AGG[IIT_FINAL_AGG$TYPE_OF_DATA=='IIT-CAMPUS',]
trainOn<-oncamp[1:(.8*length(oncamp$X1)),]
testOn<-oncamp[(.8*length(oncamp$X1)):length(oncamp$X1),]
targetVar<-'SECTOR'
xVars<-c('INCIDENT_TYPE1', 'TYPE_OF_DATA', 'INCIDENT_TYPE2', 'MONTH', 'DAY', 'TIME_BUCKET', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP')
xVars<-c('INCIDENT_TYPE2', 'DAY', 'HUM')
#use naive bayes
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, trainOn)
NB_pred<-predict(naiveBayesModel, testOn)
confusionMatrix(NB_pred,testOn$SECTOR)
#BEST=.6643
#next:
#No change: wind, temp, severity, stand_cond, time_bucket, 
#Predicts all 11.


##IIT ONLY
#Categorize as serious or mild crime
iit3$INCIDENT_TYPE2[which(iit3$INCIDENT_TYPE2=="NON CRIMINAL")] <- "MILD INCIDENTS"
iit3$INCIDENT_TYPE2[which(iit3$INCIDENT_TYPE2=="PROPERTY CRIME")] <- "SERIOUS INCIDENTS"
iit3$INCIDENT_TYPE2[which(iit3$INCIDENT_TYPE2=="SUBSTANCE CRIME")] <- "SERIOUS INCIDENTS"
iit3$INCIDENT_TYPE2[which(iit3$INCIDENT_TYPE2=="PERSON CRIME")] <- "SERIOUS INCIDENTS"
iit3$INCIDENT_TYPE2[which(iit3$INCIDENT_TYPE2=="SERIOUS CRIME")] <- "SERIOUS INCIDENTS"
#Change chr to factor
iit3$INCIDENT_TYPE1<-as.factor(iit3$INCIDENT_TYPE1)
iit3$INCIDENT_TYPE2<-as.factor(iit3$INCIDENT_TYPE2)
iit3$TYPE_OF_DATA<-as.factor(iit3$TYPE_OF_DATA)
iit3$DAY<-as.factor(iit3$DAY)
iit3$COND<-as.factor(iit3$COND)
iit3$STAND_COND<-as.factor(iit3$STAND_COND)
iit3$SEVERITY<-as.factor(iit3$SEVERITY)
iit3$SECTOR<-factor(iit3$SECTOR)
iit3$DAY<-factor(iit3$DAY, ordered = FALSE)
iit3$MONTH<-factor(iit3$MONTH, ordered = FALSE)


#sort on time occured
attach(iit3)
iit3<-iit3[order(OCCURED),]
iit3$X1 <- c(1:nrow(iit3))
detach(iit3)

#split into 80%, 20%
train<-iit3[1:(.8*length(iit3$X1)),]
test<-iit3[(.8*length(iit3$X1)):length(iit3$X1),]

#Logistic Regression
targetVar<-'INCIDENT_TYPE2'
xVars<-c('TYPE_OF_DATA', 'DAY', 'HUM')
modelForm<-createModelFormula(targetVar,xVars)
model<-glm(modelForm, family=binomial(link='logit'),data=train)
AIC(model)

#scope models
startmodel<-model<-glm(INCIDENT_TYPE2 ~ 1, family=binomial(link='logit'),data=train)
allVars<-c('TYPE_OF_DATA', 'MONTH', 'DAY', 'TIME_BUCKET', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP', 'SECTOR', 'LATITUDE', 'LONGITUDE')
endmodel<-glm(createModelFormula(targetVar, allVars), family = binomial(link = 'logit'), data=train)
finalModel<-stepAIC(model,direction = 'both', scope = list(upper=endmodel,lower=startmodel))
#From step selection, we get the variables TYPE_OF_DATA, SECTOR, TIME_BUCKET, LONGITUDE, LATITUDE and MONTH
xVars<-c('TYPE_OF_DATA', 'SECTOR', 'TIME_BUCKET', 'MONTH','LONGITUDE', 'LATITUDE')
modelForm<-createModelFormula(targetVar,xVars)
fitted.results <- predict(finalModel
                          ,newdata = test[,xVars]
                          # Specifying response means we want the probabilities
                          ,type='response')

hist(fitted.results)

test[,'fitted.results'] <- fitted.results


# We output the probabilities, but we want to turn the probabilities into
# a classification of survived or not. .5 is a reasonable starting cutoff.
# We will be revisiting this in lecture 11 heavily
survived.pred <- ifelse(fitted.results > 0.52,1,0)

mean(survived.pred)
mean(train[,targetVar])

survived.pred<-as.factor(as.integer(survived.pred))
levels(survived.pred)<-c('MILD INCIDENTS', 'SERIOUS INCIDENTS')

# Let's use a confusion matrix to evaluate how good our results are
confusion <- confusionMatrix(data = survived.pred
                             , reference = test$INCIDENT_TYPE2
                             , dnn = c("Predicted Surival", 'Actual Survival')
)
confusion
#88.3%

# Let's look at the ROC curve
library(ROCR)
pr <- prediction(fitted.results, test$INCIDENT_TYPE2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

library(geohash)
iit3$GeoHash <- apply(iit3,1,
                      function(x) 
                        return(gh_encode(as.double(x[6]), as.double(x[7]), precision=7)))
iit3$OCCURED<-as.numeric(iit3$OCCURED)
#ROSE
iit3$GeoHash<-factor(iit3$GeoHash)
library(ROSE)
targetVar<-'INCIDENT_TYPE2'
xVars<-c('TIME_BUCKET', 'OCCURED', 'MONTH','DAY', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP', 'GeoHash')
modelForm<-createModelFormula(targetVar,xVars)
data.rose<-ROSE(modelForm, iit3, seed=1)$data

set.seed(34543)
inTrain <- createDataPartition(y = data.rose[,targetVar], list = FALSE, p = .8)
train2 <- data.rose[inTrain,]
test2 <- data.rose[-inTrain,]
stopifnot(nrow(train) + nrow(test) == nrow(data.rose))

#xVars<-c('TIME_BUCKET', 'MONTH','DAY', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP', 'GeoHash')
xVars<-c('TIME_BUCKET', 'MONTH','GeoHash', 'DAY')
modelForm<-createModelFormula(targetVar,xVars)
finalModel<-glm(modelForm, family = binomial(link = 'logit'), data=train2)
fitted.results <- predict(finalModel
                          ,newdata = test2[,xVars]
                          # Specifying response means we want the probabilities
                          ,type='response')

hist(fitted.results)



# We output the probabilities, but we want to turn the probabilities into
# a classification of survived or not. .5 is a reasonable starting cutoff.
# We will be revisiting this in lecture 11 heavily
survived.pred <- ifelse(fitted.results > 0.50,1,0)

survived.pred<-as.factor(as.integer(survived.pred))
levels(survived.pred)<-c('SERIOUS INCIDENTS', 'MILD INCIDENTS')

# Let's use a confusion matrix to evaluate how good our results are
confusion <- confusionMatrix(data = survived.pred
                             , reference = test2$INCIDENT_TYPE2
                             , dnn = c("Predicted Surival", 'Actual Survival')
)
confusion
#.789
survived.pred <- ifelse(fitted.results > 0.70,1,0)

survived.pred<-as.factor(as.integer(survived.pred))
levels(survived.pred)<-c('SERIOUS INCIDENTS', 'MILD INCIDENTS')

# Let's use a confusion matrix to evaluate how good our results are
confusion <- confusionMatrix(data = survived.pred
                             , reference = test2$INCIDENT_TYPE2
                             , dnn = c("Predicted Surival", 'Actual Survival')
)
confusion

#Future test
load("~/Documents/Math 571 project/future_test.rda")
xVars<-c('TIME_BUCKET', 'MONTH','GEOHASH', 'DAY')
colnames(future_test)<-c('OCCURED', 'TIME_BUCKET', 'MONTH', 'DAY', 'GeoHash', 'LATITUDE', 'LONGITUDE')

fitted.results <- predict(finalModel
                          ,newdata = future_test
                          # Specifying response means we want the probabilities
                          ,type='response')

hist(fitted.results)



#Naive Bayes with ROSE
targetVar<-'INCIDENT_TYPE2'
xVars<-c('TIME_BUCKET', 'MONTH')
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, train2)
NB_pred<-predict(naiveBayesModel, test2)
confusionMatrix(NB_pred,test2$INCIDENT_TYPE2)
#comment
