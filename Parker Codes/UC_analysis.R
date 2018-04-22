library(nnet)
library(MASS)
library(caret)
library(e1071)
load("~/Documents/Math 571 project/UC_FINAL_AGG.Rda")

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

#Split campus and off campus
data1<-UC_FINAL_AGG[,UC_FINAL_AGG$TYPE_OF_DATA=='UC-CAMPUS']

#split into 80%, 20%
train<-UC_FINAL_AGG[1:(.8*length(UC_FINAL_AGG$X1)),]
test<-UC_FINAL_AGG[(.8*length(UC_FINAL_AGG$X1)):length(UC_FINAL_AGG$X1),]

#Logistic Regression
model<-glm(SECTOR ~ 1, data=train)
AIC(model)
finalModel<-stepAIC(model, direction = 'forward')


#Naive Bayes
targetVar<-'SECTOR'
xVars<-colnames(UC_FINAL_AGG)[c(5:9,11:20)]
#use naive bayes
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, train)
NB_pred<-predict(naiveBayesModel, test)
confusionMatrix(NB_pred,test$SECTOR)