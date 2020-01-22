setwd('C:/Users/NT500R/Desktop/AIR이론 18년도 2학기')
data <- read.csv('credit.csv')
summary(data)

library(C50)
library(gmodels)
str(data$default)
data$default <- ifelse(data$default==1,'no','yes')
data$default <- as.factor(data$default)
str(data$default)

set.seed(201711517)
train <- sample(1000,900)
data_train <- data[train,]
data_test <- data[-train,]
prop.table(table(data_train$default))
prop.table(table(data_test$default))

data_model <- C5.0(data_train[,-17],data_train$default)
summary(data_model)
data_pred <- predict(data_model,data_test)
CrossTable(data_test$default,data_pred,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))

#########################################################################
#의사결정나무모형
library(car)
library(randomForest)

plot(data$months_loan_duration,data$amount) #두변수간의 상관관계확인.
plot(data$personal_status,data$default) #

pairs(data[,c(2,5,17)]) #기간보다 대출금액이 더 목적변수에 있어 조밀한 값을 가짐


imp <- randomForest(data_train$default~.,data=data_train[,-17],importance=TRUE)
importance(imp)
varImpPlot(imp, main="varImpPlot of data") #residence_history,dependents,foreign_worker,telephone 변수 중요도 최하 네가지확인.

data_model_2 <- C5.0(data_train[,-c(17,18,19,20,21,9)],data_train$default) #변수 중요도테스트로 확인한 다섯가지제거후 모델
summary(data_model_2)
data_pred_2 <- predict(data_model_2,data_test)
CrossTable(data_test$default,data_pred_2,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(65+9)/100 으로 정확도가 74%로 증가


data_model_2_1 <- C5.0(data_train[,-c(17,18,19,20,21,9,4)],data_train$default) # 변수중요도테스트에서 나온 네가지 변수와 주관적으로 중요하지 않다생각한 purpose 변수삭제
data_pred_2_1 <- predict(data_model_2_1,data_test)
CrossTable(data_test$default,data_pred_2_1,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(64+9)/100으로 정확도가 73%로 감소.



##########################
#GLM 로지스틱 회귀모형
data_model_3 <- glm(data_train$default~.,data=data_train[,-17],family='binomial')
summary(data_model_3)
data_pred_3 <- ifelse(predict(data_model_3, data_test, type="response")>0.5,'yes','no')
CrossTable(data_test$default,data_pred_3,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(65+16)/100으로 정확도가 81%로 증가

vif(data_model_3) # 기간과 대출금액이 상관관계를 이루는 것 같지만 다중공선성은 발견되지 않음.

data_model_3_1 <- glm(data_train$default~.,data=data_train[,-c(17,18,19,20,21,9)],family = 'binomial') #변수중요도테스트를 통해 발견한 변수제거
summary(data_model_3_1)
data_pred_3_1 <- ifelse(predict(data_model_3_1, data_test, type="response")>0.5,'yes','no')
CrossTable(data_test$default,data_pred_3_1,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(65+19)/100으로 정확도가 84%로 확인

data_model_3_1_no <- glm(data_train$default~.-1,data=data_train[,-c(17,18,19,20,11)],family = 'binomial') #변수중요도테스트를 통해 발견한 변수제거
summary(data_model_3_1_no)
data_pred_3_1_no <- ifelse(predict(data_model_3_1_no, data_test, type="response")>0.5,'yes','no')
CrossTable(data_test$default,data_pred_3_1_no,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(65+19)/100으로 정확도가 84%로 확인 - 상수항이 있던 없던 정확도는 동일.


data_model_3_2 <- glm(data_train$default~.,data=data_train[,-c(17,18,19,20,21,9,4,6,8,10,11,12,13,14,15,16,5,7)],family = 'binomial') #변수중요도테스트를 통해 제거한 변수와 주관적으로 중요하지않다 생각한 변수제거
summary(data_model_3_2)
data_pred_3_2 <- ifelse(predict(data_model_3_2,data_test, type="response")>0.5,'yes','no')
CrossTable(data_test$default,data_pred_3_2,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(65+13)/100 78%의 정확도

###best model : logistic을 사용한 변수중요도를 통해 네가지 변수를 제거한 모델


########################################################################################################################################
#personal_status변수를 보면 남성에 대해서만 세분화가 되었고 여성은 그저 남성과 여성으로만으로 나뉨. -> 남성,여성 분리하여 모델링.
#여성모델링시에 위의 변수 삭제.




data_female <- data[which(data[,9]=='female'),]
data_male <- data[-which(data[,9]=='female'),]

data_female <- data_female[,-9]
summary(data_female) #여성 데이터 행의 개수 : 310
summary(data_male) #남성 데이터 행의 개수 : 690


#먼저 남성에 대한 분석
train_male <- sample(690,480)
data_male_train <- data_male[train_male,]
data_male_test <- data_male[-train_male,]
prop.table(table(data_male_train$default))
prop.table(table(data_male_test$default))

#test,train set에 있어 잘 나뉨.

#의사결정나무 모델링

male_model <- C5.0(data_male_train[,-17],data_male_train$default)
summary(male_model)
male_pred <- predict(male_model,data_male_test)
CrossTable(data_male_test$default,male_pred,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(138+17)/210으로 정확도가 약 74% 확인.

imp_male <- randomForest(data_male_train$default~.,data=data_male_train[,-17],importance=TRUE)
importance((imp_male))
varImpPlot(imp_male, main="varImpPlot of male data") # personal_status,telephone,existing_credits,foreign_worker,dependents,other_debtors 변수중요도 최하확인

male_model_2 <- C5.0(data_male_train[,-c(17,9,10,16,18,19,20)],data_male_train$default)
summary(male_model_2)
male_pred_2 <- predict(male_model_2,data_male_test)
CrossTable(data_male_test$default,male_pred_2,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(142+15)/210으로 정확도가 약 75%로 증가.

male_model_3 <- C5.0(data_male_train[,-c(17,9,10,16,18,19,20,4)],data_male_train$default) #변수중요도+주관적 불필요 변수 제거
summary(male_model_3)
male_pred_3 <- predict(male_model_3,data_male_test)
CrossTable(data_male_test$default,male_pred_3,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(134+18)/210으로 정확도가 약 72%로 감소.

##가장 좋은 의사결정모델은 변수중요도 테스트를 통해 확인한 변수들만 제거한 모델이 약 75%로 가장 좋은 정확도를 보임

#############로지스틱############################

male_model_4 <- glm(data_male_train$default~.,data=data_male_train[,-17],family='binomial')
summary(male_model_4)
male_pred_4 <- ifelse(predict(male_model_4, data_male_test, type="response")>0.5,'yes','no')
CrossTable(data_male_test$default,male_pred_4,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(135+22)/210으로 정확도가 약 75% 확인.

vif(male_model_4) #property가 6.25로 가장 높지만 다중공선성 의심 x (보통 10이상이면 의심)

male_model_5 <- glm(data_male_train$default~.,data=data_male_train[,-c(17,9,10,16,18,19,20)],family='binomial')
summary(male_model_5)
male_pred_5 <- ifelse(predict(male_model_5, data_male_test, type="response")>0.5,'yes','no')
CrossTable(data_male_test$default,male_pred_5,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(131+19)/210으로 정확도가 약 71% 감소.

vif(male_model_5)#property가 5.69로 가장 높지만 다중공선성 의심 x 

male_model_6 <- glm(data_male_train$default~.,data=data_male_train[,-c(17,9,10,16,18,19,20,4)],family='binomial')
summary(male_model_6)
male_pred_6 <- ifelse(predict(male_model_6, data_male_test, type="response")>0.5,'yes','no')
CrossTable(data_male_test$default,male_pred_6,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(139+20)/210으로 정확도가 약 76%로 증가.

vif(male_model_6)#property가 4.91로 가장 높지만 x

##남성 최고 정확도의 모델은 로지스틱을 사용하여 변수중요도체크를통한 변수들과 주관적인 생각으로 변수들을 제거한 모델.(76%)



#여성에 대한 분석
set.seed(201711517)
train_female <- sample(310,210)
data_female_train <- data_female[train_female,]
data_female_test <- data_female[-train_female,]
prop.table(table(data_female_train$default))
prop.table(table(data_female_test$default))



#의사결정나무 모델링

female_model <- C5.0(data_female_train[,-16],data_female_train$default)
summary(female_model)
female_pred <- predict(female_model,data_female_test)
CrossTable(data_female_test$default,female_pred,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#(50+17)/100으로 정확도가 67%로 확인.

imp_female <- randomForest(data_female_train$default~.,data=data_female_train[,-16],importance=TRUE)
importance((imp_female))
varImpPlot(imp_female, main="varImpPlot of female data") # telephone,installment_plan,foreign_worker,dependents,employment_length

female_model_2 <- C5.0(data_female_train[,-c(16,7,13,17,18,19)],data_female_train$default)
summary(female_model_2)
female_pred_2 <- predict(female_model_2,data_female_test)
CrossTable(data_female_test$default,female_pred_2,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#73%

female_model_3 <- C5.0(data_female_train[,-c(16,7,13,17,18,19,4)],data_female_train$default)
summary(female_model_3)
female_pred_3 <- predict(female_model_3,data_female_test)
CrossTable(data_female_test$default,female_pred_3,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#71%

##73% best

#######logistic###############


female_model_4 <- glm(data_female_train$default~.,data=data_female_train[,-16],family='binomial')
summary(female_model_4)
female_pred_4 <- ifelse(predict(female_model_4, data_female_test, type="response")>0.5,'yes','no')
CrossTable(data_female_test$default,female_pred_4,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#68%


female_model_5 <- glm(data_female_train$default~.,data=data_female_train[,-c(16,7,13,17,18,19)],family='binomial')
summary(female_model_5)
female_pred_5 <- ifelse(predict(female_model_5, data_female_test, type="response")>0.5,'yes','no')
CrossTable(data_female_test$default,female_pred_5,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#71%

female_model_6 <- glm(data_female_train$default~.,data=data_female_train[,-c(16,7,13,17,18,19,4)],family='binomial')
summary(female_model_6)
female_pred_6 <- ifelse(predict(female_model_6, data_female_test, type="response")>0.5,'yes','no')
CrossTable(data_female_test$default,female_pred_6,prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE,dnn=c('actual default','predict default'))
#63%


##71% best

####의사결정나무 모델 중 73%적합도를 지닌 모델이 가장 정확도 best