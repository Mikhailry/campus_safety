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

#Categorize as serious or mild crime
UC_FINAL_AGG$INCIDENT_TYPE2[which(UC_FINAL_AGG$INCIDENT_TYPE2=="NON CRIMINAL")] <- "MILD INCIDENTS"
UC_FINAL_AGG$INCIDENT_TYPE2[which(UC_FINAL_AGG$INCIDENT_TYPE2=="PROPERTY CRIME")] <- "SERIOUS INCIDENTS"
UC_FINAL_AGG$INCIDENT_TYPE2[which(UC_FINAL_AGG$INCIDENT_TYPE2=="SUBSTANCE CRIME")] <- "SERIOUS INCIDENTS"
UC_FINAL_AGG$INCIDENT_TYPE2[which(UC_FINAL_AGG$INCIDENT_TYPE2=="PERSON CRIME")] <- "SERIOUS INCIDENTS"
UC_FINAL_AGG$INCIDENT_TYPE2[which(UC_FINAL_AGG$INCIDENT_TYPE2=="SERIOUS CRIME")] <- "SERIOUS INCIDENTS"
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
UC_FINAL_AGG$X1 <- c(1:nrow(UC_FINAL_AGG))
detach(UC_FINAL_AGG)
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

##UC ONLY
#Categorize as serious or mild crime
UC_FINAL_AGG$INCIDENT_TYPE2[which(UC_FINAL_AGG$INCIDENT_TYPE2=="NON CRIMINAL")] <- "MILD INCIDENTS"
UC_FINAL_AGG$INCIDENT_TYPE2[which(UC_FINAL_AGG$INCIDENT_TYPE2=="PROPERTY CRIME")] <- "SERIOUS INCIDENTS"
UC_FINAL_AGG$INCIDENT_TYPE2[which(UC_FINAL_AGG$INCIDENT_TYPE2=="SUBSTANCE CRIME")] <- "SERIOUS INCIDENTS"
UC_FINAL_AGG$INCIDENT_TYPE2[which(UC_FINAL_AGG$INCIDENT_TYPE2=="PERSON CRIME")] <- "SERIOUS INCIDENTS"
UC_FINAL_AGG$INCIDENT_TYPE2[which(UC_FINAL_AGG$INCIDENT_TYPE2=="SERIOUS CRIME")] <- "SERIOUS INCIDENTS"
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
UC_FINAL_AGG$X1 <- c(1:nrow(UC_FINAL_AGG))
detach(UC_FINAL_AGG)

#split into 80%, 20%
train<-UC_FINAL_AGG[1:(.8*length(UC_FINAL_AGG$X1)),]
test<-UC_FINAL_AGG[(.8*length(UC_FINAL_AGG$X1)):length(UC_FINAL_AGG$X1),]

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
xVars<-c('TYPE_OF_DATA', 'SECTOR', 'TIME_BUCKET', 'MONTH','DAY', 'LATITUDE', 'STAND_COND', 'HUM', 'TEMP', 'LONGITUDE')
modelForm<-createModelFormula(targetVar, xVars)
finalModel<-glm(modelForm, family = binomial(link = 'logit'), data=train)
fitted.results <- predict(finalModel
                          ,newdata = test[,xVars]
                          # Specifying response means we want the probabilities
                          ,type='response')

hist(fitted.results)

test[,'fitted.results'] <- fitted.results


# We output the probabilities, but we want to turn the probabilities into
# a classification of survived or not. .5 is a reasonable starting cutoff.
# We will be revisiting this in lecture 11 heavily
survived.pred <- ifelse(fitted.results > 0.61,1,0)

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
#.9193

# Let's look at the ROC curve
library(ROCR)
pr <- prediction(fitted.results, test$INCIDENT_TYPE2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#ROSE
targetVar<-'INCIDENT_TYPE2'
xVars<-c('TYPE_OF_DATA', 'SECTOR', 'TIME_BUCKET', 'MONTH','DAY', 'LATITUDE', 'STAND_COND', 'HUM', 'TEMP', 'LONGITUDE')
modelForm<-createModelFormula(targetVar,xVars)
library(ROSE)
test2<-ROSE(modelForm, train, seed=1)
finalModel<-glm(modelForm, family = binomial(link = 'logit'), data=train)
fitted.results <- predict(finalModel
                          ,newdata = test2$data[,xVars]
                          # Specifying response means we want the probabilities
                          ,type='response')

hist(fitted.results)



# We output the probabilities, but we want to turn the probabilities into
# a classification of survived or not. .5 is a reasonable starting cutoff.
# We will be revisiting this in lecture 11 heavily
survived.pred <- ifelse(fitted.results > 0.92,1,0)

survived.pred<-as.factor(as.integer(survived.pred))
levels(survived.pred)<-c('MILD INCIDENTS', 'SERIOUS INCIDENTS')

# Let's use a confusion matrix to evaluate how good our results are
confusion <- confusionMatrix(data = survived.pred
                             , reference = test2$data$INCIDENT_TYPE2
                             , dnn = c("Predicted Surival", 'Actual Survival')
)
confusion

#naive Bayes for serious or mild
targetVar<-'INCIDENT_TYPE2'
xVars<-c('SECTOR','LONGITUDE','LATITUDE','TYPE_OF_DATA', 'MONTH', 'DAY', 'TIME_BUCKET', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP')
xVars<-c('SECTOR', 'MONTH', 'STAND_COND')
#use naive bayes
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, train)
NB_pred<-predict(naiveBayesModel, test2$data$INCIDENT_TYPE2)
confusionMatrix(NB_pred,test2$data$INCIDENT_TYPE2)
#.4983
#next: