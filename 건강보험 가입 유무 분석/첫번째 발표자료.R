setwd('C:/Users/NT500R/Desktop/AIR 19년도 여름방학/첫번째 공공 데이터')
customer <- read.table('customer.txt',sep='\t',header=T)
summary(customer)
library(dplyr)
glimpse(customer)
customer$is.employed <- as.factor(customer$is.employed)
customer$marital.stat <- as.factor(customer$marital.stat)
customer$health.ins <- as.factor(customer$health.ins)
customer$num.vehicles <- as.factor(customer$num.vehicles)
customer$recent.move <- as.factor(customer$recent.move)
summary(customer)
glimpse(customer)

##변수설명
#custid : 고객 번호
#secustomer : 고객 성별
#is.employed : 직업의 유무
#income : 월 소득
#marital.stat : 결혼상태
#health.ins : 건강보험 가입의 유무
#housing.type : 거주지 소유 상태
#recent.move : 최근 이사 여부
#num.vehicles : 소유 자동차 수
#age : 고객의 나이
#state.of.res : 고객의 거주지

customer <- customer[,-1]
summary(customer)

############EDA#################
##is.employed 처리.

A<-customer[which(customer$income>53505),]
summary(A)

#소득이 있다면 고용되었을 것이다 -> 평균값보다 소득이 크면 고용 -> but, 평균값보다 소득이 커도 일을안한 사람이 존재 -> NA 대체값을 찾기 어려움으로 다 제거하기로 결정.

customer <- customer[-which(is.na(customer$is.employed)),]
summary(customer)

##income 처리.
boxplot(customer$income)
#사업을 하는 사람입장에서 수입이 가변비용(음식재료비 등등..)보다 작으면 소득이 (-)가 될 수 있다. -> 소득이 (-)인 데이터를 건드리지 않기로 결정.
boxplot(customer$income)$stats
#165200보다 큰 관측치를 이상치라고 판단 후 제거하기로 결정.
length(which(customer$income>165200))

customer <- customer[-which(customer$income>165200),] #이상치 제거.
summary(customer)
boxplot(customer$income)

##marital.stat
#Widowed 관측치의 갯수가 너무 적으므로 이혼관측치에 병합. -> 범주명을 'alone'으로.
customer$marital.stat <- as.character(customer$marital.stat)
customer[which(customer$marital.stat=='Widowed'),4] <- "alone"
customer[which(customer$marital.stat=='Divorced/Separated'),4] <- "alone"
customer$marital.stat <- as.factor(customer$marital.stat)
summary(customer$marital.stat)


##housing.type
customer <- customer[-which(is.na(customer$housing.type)),] #NA 제거
summary(customer)

##num.vehicles
customer$num.vehicles <- as.numeric(as.character(customer$num.vehicles)) #0을 살리기위해 캐릭터화 후 뉴메릭화 시켜줌.
length(which(customer$num.vehicles>3))
customer[which(customer$num.vehicles>3),8] <- '>=4'
customer$num.vehicles <- as.factor(customer$num.vehicles)
summary(customer)

##age
summary(customer$age)
boxplot(customer$age)$stats
customer <- customer[-which(customer$age==0),]
customer <- customer[-which(customer$age>77),]
boxplot(customer$age)$stats
#0인 데이터와 상자그림으로 보았을때의 이상치라고 판단된 데이터들 삭제.


##state.of.res
customer$part[customer$state.of.res=='Nevada']<-'west'
customer$part[customer$state.of.res=='New Mexico']<-'west'
customer$part[customer$state.of.res=='Montana']<-'west'
customer$part[customer$state.of.res=='Arizona']<-'west'
customer$part[customer$state.of.res=='Idaho']<-'west'
customer$part[customer$state.of.res=='Alaska']<-'west'
customer$part[customer$state.of.res=='Oregon']<-'west'
customer$part[customer$state.of.res=='Wyoming']<-'west'
customer$part[customer$state.of.res=='Washington']<-'west'
customer$part[customer$state.of.res=='Utah']<-'west'
customer$part[customer$state.of.res=='California']<-'west'
customer$part[customer$state.of.res=='Colorado']<-'west'
customer$part[customer$state.of.res=='Hawaii']<-'west'
customer$part[customer$state.of.res=='Nebraska']<-'midwest'
customer$part[customer$state.of.res=='North Dakota']<-'midwest'
customer$part[customer$state.of.res=='Minnesota']<-'midwest'
customer$part[customer$state.of.res=='Michigan']<-'midwest'
customer$part[customer$state.of.res=='Missouri']<-'midwest'
customer$part[customer$state.of.res=='South Dakota']<-'midwest'
customer$part[customer$state.of.res=='Ohio']<-'midwest'
customer$part[customer$state.of.res=='Iowa']<-'midwest'
customer$part[customer$state.of.res=='Wisconsin']<-'midwest'
customer$part[customer$state.of.res=='Indiana']<-'midwest'
customer$part[customer$state.of.res=='Illinois']<-'midwest'
customer$part[customer$state.of.res=='Kansas']<-'midwest'
customer$part[customer$state.of.res=='New Jersey']<-'northeast'
customer$part[customer$state.of.res=='New York']<-'northeast'
customer$part[customer$state.of.res=='New Hampshire']<-'northeast'
customer$part[customer$state.of.res=='Rhode Island']<-'northeast'
customer$part[customer$state.of.res=='Massachusetts']<-'northeast'
customer$part[customer$state.of.res=='Maine']<-'northeast'
customer$part[customer$state.of.res=='Vermont']<-'northeast'
customer$part[customer$state.of.res=='Connecticut']<-'northeast'
customer$part[customer$state.of.res=='Pennsylvania']<-'northeast'
customer$part[customer$state.of.res=='Alabama']<-'south'
customer$part[customer$state.of.res=='Delaware']<-'south'
customer$part[customer$state.of.res=='Florida']<-'south'
customer$part[customer$state.of.res=='Georgia']<-'south'
customer$part[customer$state.of.res=='Kentucky']<-'south'
customer$part[customer$state.of.res=='Louisiana']<-'south'
customer$part[customer$state.of.res=='Maryland']<-'south'
customer$part[customer$state.of.res=='Mississippi']<-'south'
customer$part[customer$state.of.res=='North Carolina']<-'south'
customer$part[customer$state.of.res=='Oklahoma']<-'south'
customer$part[customer$state.of.res=='South Carolina']<-'south'
customer$part[customer$state.of.res=='Tennessee']<-'south'
customer$part[customer$state.of.res=='Texas']<-'south'
customer$part[customer$state.of.res=='Virginia']<-'south'
customer$part[customer$state.of.res=='West Virginia']<-'south'
customer$part[customer$state.of.res=='Arkansas']<-'south'
customer$part <- as.factor(customer$part)
summary(customer$part)
#주로 볼때 범주가 너무 많으므로 구역변수를 만들어 대체하기로 결정.
summary(customer)

####목적변수와 설명변수간 관계 파악.####
X <- customer
library(ggplot2)
##성별과 Y의 관계.
attach(X)
A <- table(sex,health.ins)
A[1,2]/sum(A[1,]) #여성의 보험가입률 -> 0.844
A[2,2]/sum(A[2,]) #남성의 보험가입률 -> 0.808
#여성이 남성보다 보험에 가입되어있을 확률이 더 크다.

##직업의 유무와 Y
A <- table(is.employed,health.ins)
A[1,2]/sum(A[1,]) #직업이 없는사람의 보험가입률 -> 0.614
A[2,2]/sum(A[2,]) #직업이 있는사람의 보험가입률 -> 0.85
#미국의 건강보험료는 매우 비쌈 -> 직업이 있어야 꾸준히 보험료를 낼 수 있음 -> 직업이 있다면 가입률이 높을 것이다.

##income & Y
ggplot(X,aes(x=health.ins,y=income,fill=health.ins))+geom_boxplot()
#소득이 클수록 건강보험에 가입되어있을 확률이 크다.

##marital & Y
table(marital.stat,health.ins)
#결혼을 한적이 있으면 보험에 가입되어있을 확률이 크다고 볼 수 있다.(하지만 생각을 해보면 영향을 주는 변수라고 생각 x)

##housing & Y
table(housing.type,health.ins)
#영향을 주는 변수라고 생각 x

##recent & Y -> 전혀 상관이 없을거라 생각하여 탐색 x

##num. & Y
table(num.vehicles,health.ins)
#자동차 수가 2대 이상일 경우 보험에 가입되어있을 확률이 크다고 볼 수 있다.

##age & Y
ggplot(X,aes(x=health.ins,y=age,fill=health.ins))+geom_boxplot()
#나이가 많을수록 보험에 가입되어있을 확률이 크다고 볼 수 있다.

##state & Y
table(X$state.of.res,X$health.ins)
#각 범주별 관측치가 너무 적으므로 part변수로 대체하여 볼 것.

##part & Y
table(part,health.ins)
#크게 상관이 있다고 할 수 없다.



#########분석##########
set.seed(1234)
n <- nrow(X)
idx <- 1:n
train_idx <- sample(idx,0.7*n,replace=F)
train <- X[train_idx,]
test <- X[-train_idx,]

glm_0 <- glm(health.ins~.-state.of.res,train,family='binomial')
summary(glm_0)
#AIC : 346.8, 


glm_1 <- glm(health.ins~.-1-state.of.res,train,family='binomial')
summary(glm_1)


glm_s <- step(glm_0,direction='both')
summary(glm_s) #최종모델

#test 적합.
library(ROCR)
pred  <- predict(glm_s,newdata=test)
pr <- prediction(pred,test$health.ins)
perf <- performance(pr,measure = 'tpr',x.measure = 'fpr')
plot(perf)
abline(0,1)

performance(pr,measure = 'auc')@y.values[[1]]
#최종모델의 auc값이 약0.6921로 이는 모델의 정분류율이 약69.2% 임을 의미한다.

##########결론##########
summary(glm_s) 
#모델은 intercept, sex, income, marital, housing 변수만이 사용 되었고, AIC값은 330.86이다.
#ROC curve와 AUC값으로 보았을 때, 최종모델의 auc값이 약0.6921로 이는 모델의 정분류율이 약69.2% 임을 의미한다.


################################두번째 발표자료########################################
##Decision tree
library(rpart)
train0 <- train[,-10]
test0 <- test[,-10]

tr_0 <- rpart(health.ins~.,data=train0)
tr_0

plot(tr_0, margin=0.1)
text(tr_0,cex=.8)

pred_tr <- predict(tr_0,newdata=test0,type='class')
pr <- prediction(c(pred_tr),test0$health.ins)
perf <- performance(pr,measure = 'tpr',x.measure = 'fpr')
plot(perf)
abline(0,1)

performance(pr,measure = 'auc')@y.values[[1]]
##쓸모없는 모델....

##RF
library(randomForest)
rf_0 <- randomForest(health.ins~.,data=train0,importance=TRUE)
rf_0
plot(rf_0)

pred_rf <- predict(rf_0,newdata=test0,type='class')
pr <- prediction(c(pred_rf),test0$health.ins)
perf <- performance(pr,measure = 'tpr',x.measure = 'fpr')
plot(perf)
abline(0,1)
performance(pr,measure = 'auc')@y.values[[1]]
##쓸모없는모델..

##Bayes model
library(e1071)
nb_0 <- naiveBayes(health.ins~.,data=train0)
nb_0
summary(nb_0)
pred_nb <- predict(nb_0,newdata=test0,type='class')
pr <- prediction(c(pred_nb),test0$health.ins)
perf <- performance(pr,measure = 'tpr',x.measure = 'fpr')
plot(perf)
abline(0,1)
performance(pr,measure = 'auc')@y.values[[1]]






