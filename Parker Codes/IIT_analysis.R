library(nnet)
library(MASS)
library(caret)
library(e1071)
load("~/Documents/Math 571 project/IIT_FINAL_AGG.Rda")
IIT_FINAL_AGG<-na.omit(IIT_FINAL_AGG)

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = ' + ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = ' + '), -1))
  }
  return(modelForm)
}

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

#split into 80%, 20%
train<-IIT_FINAL_AGG[1:(.8*length(IIT_FINAL_AGG$X1)),]
test<-IIT_FINAL_AGG[(.8*length(IIT_FINAL_AGG$X1)):length(IIT_FINAL_AGG$X1),]

#Logistic Regression
model<-glm(SECTOR ~ 1, data=train)
AIC(model)
finalModel<-stepAIC(model, direction = 'forward')


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

