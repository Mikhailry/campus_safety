library(nnet)
library(MASS)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
load("~/Documents/Math 571 project/iit_only.Rda")

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = ' + ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = ' + '), -1))
  }
  return(modelForm)
}

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
library(geohash)
iit3$GeoHash <- apply(iit3,1,
                      function(x) 
                        return(gh_encode(as.double(x[6]), as.double(x[7]), precision=7)))
iit3$OCCURED<-as.numeric(iit3$OCCURED)
iit3$GeoHash<-factor(iit3$GeoHash)
iit3<-iit3[!as.character(iit3$GeoHash)=='dp3tvz5',]
iit3<-iit3[!as.character(iit3$GeoHash)=='dp3wjbv',]

#sort on time occured
attach(iit3)
iit3<-iit3[order(OCCURED),]
iit3$X1 <- c(1:nrow(iit3))
detach(iit3)

#split into 80%, 20%
train<-iit3[1:(.8*length(iit3$X1)),]
test<-iit3[(.8*length(iit3$X1)):length(iit3$X1),]

#Logistic Regression and step selection
startmodel<-model<-glm(INCIDENT_TYPE2 ~ 1, family=binomial(link='logit'),data=train)
allVars<-c('TYPE_OF_DATA', 'MONTH', 'DAY', 'TIME_BUCKET', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP', 'SECTOR', 'LATITUDE', 'LONGITUDE')
endmodel<-glm(createModelFormula(targetVar, allVars), family = binomial(link = 'logit'), data=train)
finalModel<-stepAIC(model,direction = 'both', scope = list(upper=endmodel,lower=startmodel))
#From step selection, we get the variables TYPE_OF_DATA, SECTOR, TIME_BUCKET, LONGITUDE, LATITUDE and MONTH
xVars<-c('TYPE_OF_DATA', 'SECTOR', 'TIME_BUCKET', 'MONTH','LONGITUDE', 'LATITUDE')
modelForm<-createModelFormula(targetVar,xVars)
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
survived.pred <- ifelse(fitted.results > 0.52,1,0)

mean(survived.pred)
mean(train[,targetVar])

survived.pred<-as.factor(as.integer(survived.pred))
levels(survived.pred)<-c('MILD INCIDENTS', 'SERIOUS INCIDENTS')

# Let's use a confusion matrix to evaluate how good our results are
confusion <- confusionMatrix(data = survived.pred
                             , reference = test$INCIDENT_TYPE2
                             , dnn = c("Predicted Incidents", 'Actual Incidents')
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

#Decision Tree on data
xVars<-c('TYPE_OF_DATA', 'SECTOR', 'TIME_BUCKET', 'MONTH','LONGITUDE', 'LATITUDE')
modelForm<-createModelFormula(targetVar,xVars)
fit <- rpart(modelForm, train)
summary(fit)
pred<-predict(fit, test, type="class")
confusionMatrix(pred,test$INCIDENT_TYPE2)

#Naive Bayes on data
targetVar<-'INCIDENT_TYPE2'
xVars<-c('TIME_BUCKET', 'MONTH')
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, train)
NB_pred<-predict(naiveBayesModel, test)
confusionMatrix(NB_pred,test$INCIDENT_TYPE2)

#Random Forest on Data
fit <- randomForest(modelForm, data=train )
pred<-predict(fit, test)
head(pred)
confusionMatrix(pred,test$INCIDENT_TYPE2)

#ROSE
library(ROSE)
targetVar<-'INCIDENT_TYPE2'
xVars<-c('TIME_BUCKET', 'OCCURED', 'MONTH','DAY', 'COND', 'STAND_COND', 'SEVERITY', 'TEMP', 'HUM', 'WIND', 'PRECIP', 'GeoHash')
modelForm<-createModelFormula(targetVar,xVars)
data.rose<-ROSE(modelForm, iit3, seed=1)$data

attach(data.rose)
data.rose<-data.rose[order(OCCURED),]
detach(data.rose)

train2<-data.rose[1:(.8*length(iit3$OCCURED)),]
test2<-data.rose[(.8*length(iit3$OCCURED)):length(iit3$OCCURED),]

#set.seed(34543)
#inTrain <- createDataPartition(y = data.rose[,targetVar], list = FALSE, p = .8)
#train2 <- data.rose[inTrain,]
#test2 <- data.rose[-inTrain,]
#stopifnot(nrow(train) + nrow(test) == nrow(data.rose))

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
                             , dnn = c("Predicted Incidents", 'Actual Incidents')
)
confusion
#.789 for the rose test data

#TEST original test data
fitted.results <- predict(finalModel
                          ,newdata = test[,xVars]
                          # Specifying response means we want the probabilities
                          ,type='response')
fitted.results<-abs(1-fitted.results)
hist(fitted.results)
survived.pred <- ifelse(fitted.results > 0.5,1,0)

survived.pred<-as.factor(as.integer(survived.pred))
levels(survived.pred)<-c('MILD INCIDENTS', 'SERIOUS INCIDENTS')

# Let's use a confusion matrix to evaluate how good our results are
confusion <- confusionMatrix(data = survived.pred
                             , reference = test$INCIDENT_TYPE2
                             , dnn = c("Predicted Incidents", 'Actual Incidents')
)
confusion

pr <- prediction(fitted.results, test$INCIDENT_TYPE2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Decision Tree with ROSE
xVars<-c('TIME_BUCKET', 'MONTH','GeoHash', 'DAY')
modelForm<-createModelFormula(targetVar,xVars)
fit <- rpart(modelForm, train2)
summary(fit)
pred<-predict(fit, test, type="class")
confusionMatrix(pred,test$INCIDENT_TYPE2)

#Naive Bayes with ROSE
targetVar<-'INCIDENT_TYPE2'
xVars<-c('TIME_BUCKET', 'MONTH','GeoHash', 'DAY')
modelForm<-createModelFormula(targetVar,xVars)
naiveBayesModel<-naiveBayes(modelForm, train2)
NB_pred<-predict(naiveBayesModel, test)
confusionMatrix(NB_pred,test$INCIDENT_TYPE2)

#Random Forest with ROSE
xVars<-c('TIME_BUCKET', 'MONTH', 'DAY')
modelForm<-createModelFormula(targetVar,xVars)
fit <- randomForest(modelForm, data=train2 )
pred<-predict(fit, test)
head(pred)
confusionMatrix(pred,test$INCIDENT_TYPE2)

#Future test
load("~/Documents/Math 571 project/future_test.rda")
xVars<-c('TIME_BUCKET', 'MONTH','GEOHASH', 'DAY')
colnames(future_test)<-c('OCCURED', 'TIME_BUCKET', 'MONTH', 'DAY', 'GeoHash', 'LATITUDE', 'LONGITUDE')
future_test<-future_test[!as.character(future_test$GeoHash)=='dp3tvz5',]
future_test<-future_test[!as.character(future_test$GeoHash)=='dp3wjbv',]
fitted.results <- predict(finalModel
                          ,newdata = future_test
                          # Specifying response means we want the probabilities
                          ,type='response')

future_test$PROB<-abs(1-fitted.results)
save(finalModel, file = 'LogModel.rda')