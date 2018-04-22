library(nnet)
library(MASS)
library(caret)
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
xVars<-colnames(Data)[-24]
#need to change the decsion variable into a factor
Data2<-Data
train2<-train
test2<-test
Data2$default<-as.factor(Data2$default)
test2$default<-as.factor(test2$default)
train2$default<-as.factor(train2$default)
#use naive bayes
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, train2)
NB_pred<-predict(naiveBayesModel, test2)
confusionMatrix(NB_pred,test2$default)



