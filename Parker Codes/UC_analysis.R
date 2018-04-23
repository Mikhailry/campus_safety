library(nnet)
library(MASS)
library(caret)
library(e1071)
load("~/Documents/Math 571 project/UC_FINAL_AGG.Rda")
UC_FINAL_AGG<-na.omit(UC_FINAL_AGG)

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = ' + ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = ' + '), -1))
  }
  return(modelForm)
}

#Change chr to factor
UC_FINAL_AGG$INCIDENT_TYPE1<-as.factor(UC_FINAL_AGG$INCIDENT_TYPE1)
UC_FINAL_AGG$INCIDENT_TYPE2<-as.factor(UC_FINAL_AGG$INCIDENT_TYPE2)
UC_FINAL_AGG$TYPE_OF_DATA<-as.factor(UC_FINAL_AGG$TYPE_OF_DATA)
UC_FINAL_AGG$DAY<-as.factor(UC_FINAL_AGG$DAY)
UC_FINAL_AGG$COND<-as.factor(UC_FINAL_AGG$COND)
UC_FINAL_AGG$STAND_COND<-as.factor(UC_FINAL_AGG$STAND_COND)
UC_FINAL_AGG$SEVERITY<-as.factor(UC_FINAL_AGG$SEVERITY)
UC_FINAL_AGG$SECTOR<-factor(UC_FINAL_AGG$SECTOR)

#sort on time occured
attach(UC_FINAL_AGG)
UC_FINAL_AGG<-UC_FINAL_AGG[order(OCCURED),]

#split into 80%, 20%
train<-UC_FINAL_AGG[1:(.8*length(UC_FINAL_AGG$X1)),]
test<-UC_FINAL_AGG[(.8*length(UC_FINAL_AGG$X1)):length(UC_FINAL_AGG$X1),]

#Logistic Regression
model<-glm(SECTOR ~ 1, data=train)
AIC(model)
finalModel<-stepAIC(model, direction = 'forward')


#Naive Bayes
targetVar<-'SECTOR'
xVars<-c('INCIDENT_TYPE1', 'TYPE_OF_DATA', 'INCIDENT_TYPE2', 'MONTH', 'DAY', 'TIME_BUCKET', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP')
xVars<-c('TYPE_OF_DATA', 'INCIDENT_TYPE1', 'HUM', 'MONTH', 'WIND')
#use naive bayes
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, train)
NB_pred<-predict(naiveBayesModel, test)
confusionMatrix(NB_pred,test$SECTOR)
#BEST=.1429
#No change: COND

#Off campus
offcamp<-UC_FINAL_AGG[UC_FINAL_AGG$TYPE_OF_DATA=='UC-AREA',]
trainOff<-offcamp[1:(.8*length(offcamp$X1)),]
testOff<-offcamp[(.8*length(offcamp$X1)):length(offcamp$X1),]
targetVar<-'SECTOR'
xVars<-c('INCIDENT_TYPE1', 'INCIDENT_TYPE2', 'MONTH', 'DAY', 'TIME_BUCKET', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP')
xVars<-c('INCIDENT_TYPE1', 'HUM', 'MONTH')
#use naive bayes
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, trainOff)
NB_pred<-predict(naiveBayesModel, testOff)
confusionMatrix(NB_pred,testOff$SECTOR)
#BEST=.1337
#next:
#No change: STAND_COND

#On campus
oncamp<-UC_FINAL_AGG[UC_FINAL_AGG$TYPE_OF_DATA=='UC-CAMPUS',]
trainOn<-oncamp[1:(.8*length(oncamp$X1)),]
testOn<-oncamp[(.8*length(oncamp$X1)):length(oncamp$X1),]
targetVar<-'SECTOR'
xVars<-c('INCIDENT_TYPE1', 'TYPE_OF_DATA', 'INCIDENT_TYPE2', 'MONTH', 'DAY', 'TIME_BUCKET', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP')
xVars<-c('INCIDENT_TYPE1', 'HUM', 'MONTH')
#use naive bayes
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, trainOn)
NB_pred<-predict(naiveBayesModel, testOn)
confusionMatrix(NB_pred,testOn$SECTOR)
#BEST=.3951

