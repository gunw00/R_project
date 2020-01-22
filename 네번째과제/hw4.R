rm(list=ls())
library(ggplot2)
library(dplyr)
library(ISLR)
library(randomForest)
library(ROCR)
library(glmnet)
library(C50)

setwd('C:/Users/NT500R/Desktop/AIR이론 18년도 2학기')
adult <- read.csv('adult.data',header=FALSE,strip.white = T)
names(adult) <- c('age','workclass','fnlwgt','education','education_num','marital_status','occupation','relationship','race','sex','capital_gain','capital_loss','hours_per_week','native_country','wage')


adult<- adult %>%
  filter(workclass!='?' & occupation!='?' & native_country!='?')

adult$wage <- as.factor(adult$wage) 

set.seed(201711517)
n <- nrow(adult)
idx <- 1:n
training_idx <- sample(idx,n*0.6)
idx <- setdiff(idx,training_idx)
validation_idx <- sample(idx,n*0.2)
test_idx <- setdiff(idx,validation_idx)

train <- adult[training_idx,]
validation <- adult[validation_idx,]
test <- adult[test_idx,]


xx <- model.matrix(wage~.-1,data=adult)
x <- xx[training_idx,]
y <- ifelse(train$wage=='>50K',1,0)


xx %>% head(5)

glmnet_fit <- glmnet(x,y)

cvfit <- cv.glmnet(x,y,family='binomial')
cvfit

x11()
par(mfrow=c(1,2))
plot(glmnet_fit,main='glmnet')
plot(cvfit,main='cv.glmnet')

log(cvfit$lambda.min)

log(cvfit$lambda.1se)

set.seed(201711517)
cv1 <- cv.glmnet(x,y,family='binomial',alpha=1)
cv.5 <- cv.glmnet(x,y,family='binomial',alpha=0.5)
cv0 <- cv.glmnet(x,y,family='binomial',alpha=0)




yhat_glmnet <- predict(cvfit,s='lambda.1se',newx=xx[validation_idx,],type='response')
yhat_glmnet %>% head(10)
yhat_glmnet <- yhat_glmnet[,1]
yhat_glmnet %>% head(5)

y1 <- ifelse(validation$wage=='>50K',1,0)
pred_glmnet <- prediction(yhat_glmnet,y1)
perf_glmnet <- performance(pred_glmnet,measure = 'tpr',x.measure = 'fpr')

x11()
plot(perf_glmnet)
abline(0,1)

performance(pred_glmnet,'auc')@y.values[[1]]

y2 <- ifelse(test$wage=='>50K',1,0)
yhat_glmnet_test <- predict(cvfit,s='lambda.1se',newx=xx[test_idx,],type='response')
yhat_glmnet_test <- yhat_glmnet[,1]

pred_glmnet_test <- prediction(yhat_ridge,y_ridge)
perf_glmnet_test <- performance(pred_glmnet_test,measure = 'tpr',x.measure = 'fpr')

x11()
plot(perf_glmnet_test)
abline(0,1)

performance(pred_glmnet_test,'auc')@y.values[[1]]



###########
###ridge medel###

yhat_ridge <- predict(cv0,s='lambda.1se',newx=xx[validation_idx,],type='response')
yhat_ridge <- yhat_ridge[,1]

y_ridge <- ifelse(validation$wage=='>50K',1,0)
pred_ridge <- prediction(yhat_ridge,y_ridge)
perf_ridge <- performance(pred_ridge,measure = 'tpr',x.measure = 'fpr')

x11()
plot(perf_ridge)
abline(0,1)

performance(pred_ridge,'auc')@y.values[[1]]

###Elastic medel###

yhat_ela <- predict(cv.5,s='lambda.1se',newx=xx[validation_idx,],type='response')
yhat_ela <- yhat_ela[,1]

y_ela <- ifelse(validation$wage=='>50K',1,0)
pred_ela <- prediction(yhat_ela,y_ela)
perf_ela <- performance(pred_ela,measure = 'tpr',x.measure = 'fpr')

x11()
plot(perf_ela)
abline(0,1)

performance(pred_ela,'auc')@y.values[[1]]

###RandamForest model###
rf <- randomForest(wage~.,train)

yhat_rf <- predict(rf, newdata=validation, type='prob')[,'>50K']
y_rf <- ifelse(validation$wage == '>50K', 1, 0)
pred_rf <- prediction(yhat_rf, y_rf)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')

x11()
plot(perf_rf)
abline(0,1)

performance(pred_rf,'auc')@y.values[[1]]


###Decision tree model###
train$wage <- ifelse(train$wage=='>50K',1,0)
train$wage <- as.factor(train$wage) 
DT <- C5.0(train[,-15],train$wage)

yhat_DT <- predict(DT, newdata=validation, type='prob')[,'1']
y_DT <- ifelse(validation$wage == '>50K', 1, 0)
pred_DT <- prediction(yhat_DT, y_DT)
perf_DT <- performance(pred_DT, measure='tpr', x.measure='fpr')

x11()
plot(perf_DT)
abline(0,1)

performance(pred_DT,'auc')@y.values[[1]]
