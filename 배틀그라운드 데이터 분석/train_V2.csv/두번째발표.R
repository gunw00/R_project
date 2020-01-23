setwd('C:\\Users\\NT500R\\Desktop\\AIR 19년도 여름방학\\두번째 분석 데이터\\train_V2.csv')
library(data.table)
data <- fread('solo_PUBG.csv')
str(data)
X <- data[,-c(1,2,3,4,5,8,11,12,16,17,18,19,20,21,25,29)]
str(X)
#boosts - 사용된 부스트 품목 수입니다.
#damageDealt - 총 손상 발생. 참고: 자체적인 손상은 감산한다.
#HeadshotKills - 헤드샷으로 죽은 적군의 수입니다.
#heals - 사용된 치료 항목의 수입니다.
#killStreaks - 짧은 시간 내에 사망한 최대 적 선수 수입니다.-> 시작하자마자 죽인 적의 수 
#kills - 살해된 적군의 수입니다.
#LonggestKill - 사망 시 사망한 선수와 선수 사이의 가장 긴 거리.
#rideDistance - 미터 단위로 측정한 차량에서 이동한 총 거리
#roadKills - 차량 내에서의 살인 수입니다.
#swimDistance - 미터 단위로 측정한 수영에 의한 총 이동 거리.
#vehicleDestroys - 파괴된 차량 수입니다. 
#walkDistance - 미터 단위로 측정한 도보로 이동한 총 거리
#weaponsAcquired - 픽업된 무기 수입니다.
#winPlacePerc - 예측 대상. 이것은 백분위수 승점인데, 여기서 1은 1위, 0은 경기에서 꼴찌에 해당한다.

#################EDA#################
boxplot(X$winPlacePerc) #정규성을 따른다고 볼 수 있다.




#boosts
boxplot(X$boosts)$stats
length(which(X$boosts>5)) #사용된 부스트 품목 수가 5개보다 많은 데이터들을 삭제하기로 결정.

X <- X[-which(X$boosts>5),]
boxplot(X$boosts)$stats

library(ggplot2)
library(dplyr)
X %>%
  ggplot(aes(x=boosts,y=winPlacePerc))+geom_point()+geom_jitter() #사용된 부스트 품목수가 많을 수록 낮은 승점을 받지 않는다고 볼 수 있다.



#damageDealt
boxplot(X$damageDealt)$stats #500단위로 범주화

X$damage <- as.factor( ifelse(X$damageDealt<500,'0-500',
               ifelse(X$damageDealt<1000,'500-1000',
                    ifelse(X$damageDealt<1500,'1000-1500',
                         ifelse(X$damageDealt<2000,'1500-2000','2000-2500')))))
summary(X$damage)

X %>%
  ggplot(aes(x=damage,y=winPlacePerc,fill=damage))+geom_boxplot() #그닥 차이가 없어보임


#headshotkills
boxplot(X$headshotKills)
summary(X$headshotKills)
X$headshotKills <- as.factor(X$headshotKills)
summary(X$headshotKills)
X$headshotKills <- as.numeric(as.character(X$headshotKills))
idx <- which(X$headshotKills>=4)

X$headshotKills <- as.factor(X$headshotKills)
X[idx,3] <- '>=4'
summary(X$headshotKills)

X %>%
  ggplot(aes(x=headshotKills,y=winPlacePerc,fill=headshotKills))+geom_boxplot() # 헤드샷을 많이 할 수록 점수가 올라감.


#heals
boxplot(X$heals)
summary(X$heals)
X$heals <- as.factor(X$heals)
summary(X$heals)

X$heals <- as.numeric(as.character(X$heals))
X$heals <- as.factor(ifelse(X$heals==0,'0',
                  ifelse(X$heals<=5,'1-5','6-')))
summary(X$heals)

X %>%
  ggplot(aes(x=heals,y=winPlacePerc,fill=heals))+geom_boxplot() #치료용 품목을 많이 사용 할 수록 점수가 올라감을 알 수 있다.


#killStreaks
boxplot(X$killStreaks)
X$killStreaks <- as.factor(X$killStreaks)
summary(X$killStreaks)

X$killStreaks <- as.numeric(as.character(X$killStreaks))
X <- X[-which(X$killStreaks>2),]
X$killStreaks <- as.factor(X$killStreaks)
summary(X$killStreaks)
X$killStreaks <- as.numeric(as.character(X$killStreaks))

X %>%
  ggplot(aes(x=killStreaks,y=winPlacePerc))+geom_point()+geom_jitter()


#kills
boxplot(X$kills)
X$kills <- as.factor(X$kills)
summary(X$kills)

X %>%
  ggplot(aes(x=kills,y=winPlacePerc))+geom_point()+geom_jitter() #킬수가 많을수록 승점이 높다고 볼 수 있다.

#LonggestKill
boxplot(X$longestKill)

X %>%
  ggplot(aes(x=longestKill,y=winPlacePerc))+geom_point()+geom_jitter() #상대방을 죽인 거리가 멀면 승점이 낮다고 볼 수 없다.

#rideDistance
boxplot(X$rideDistance)

X %>%
  ggplot(aes(x=rideDistance,y=winPlacePerc))+geom_point()+geom_jitter() #이동수단을 탄 거리가 높을수록 승점은 낮다고 볼 수 없다.



#roadKills
boxplot(X$roadKills)
X$roadKills <- as.factor(X$roadKills)
summary(X$roadKills)
X$roadKills <- as.factor(ifelse(X$roadKills=='0','No','Yes'))
summary(X$roadKills)

X %>%
  ggplot(aes(x=roadKills,y=winPlacePerc,fill=roadKills))+geom_boxplot() #roadkill을 했으면 승점이 높다고 볼 수 있다.

#swimDistance
boxplot(X$swimDistance)

X %>%
  ggplot(aes(x=swimDistance,y=winPlacePerc))+geom_point()


#vehicleDestroys
boxplot(X$vehicleDestroys)
X$vehicleDestroys <- as.factor(X$vehicleDestroys)
summary(X$vehicleDestroys)

X$vehicleDestroys<- as.factor(ifelse(X$vehicleDestroys=='0','No','Yes'))
summary(X$vehicleDestroys)

X %>%
  ggplot(aes(x=vehicleDestroys,y=winPlacePerc,fill=vehicleDestroys))+geom_boxplot() # 이동수단을 파괴를 한사람이 안한사람보다 승점이 높다고 볼 수 있다.


#walkDistance
boxplot(X$walkDistance)

length(which(X$walkDistance==0))

X <- X[-which(X$walkDistance==0),]
boxplot(X$walkDistance)
summary(X$walkDistance)

X %>%
  ggplot(aes(x=walkDistance,y=winPlacePerc))+geom_point() # 많이 움직인 사람은 승점이 낮다고 볼 수 없다.


#weaponsAcquired
boxplot(X$weaponsAcquired)
X$weaponsAcquired <- as.factor(X$weaponsAcquired)
summary(X$weaponsAcquired)

X$weaponsAcquired <- as.numeric(as.character(X$weaponsAcquired))
X <- X[-which(X$weaponsAcquired>=15),] #15개 이상의 무기를 주운 데이터는 이상치로 판단하여 삭제.
boxplot(X$weaponsAcquired)

X %>%
  ggplot(aes(x=weaponsAcquired,y=winPlacePerc))+geom_point()+geom_jitter() #무기를 많이 주울수록 승점이 낮다고 볼 수 없다.



#################################분석###########################################

###GBM
library(caret)
summary(X)
set.seed(1234)
training_idx <- createDataPartition(X$damage, times = 1, p = 0.7, list = F)
train <- X[training_idx,]
test <- X[-training_idx,]

summary(train)
summary(test)

##randomForest
library(randomForest)
library(Metrics)
library(rpart)
rf_mod1 <- randomForest(winPlacePerc~.,train)
y_hat_rf <- predict(rf_mod1,test_data)
rmse(test_data$winPlacePerc,y_hat_rf)

###GBM
library(gbm)
library(ROCR)
gbm0 <- gbm(winPlacePerc~.-damageDealt, data=train, distribution='gaussian',n.trees = 2000)
summary(gbm0)
#중요변수로 walkDistance가 나옴.
pp = predict(gbm0, newdata=test,type='response', n.trees = 2000)
library(Metrics)
rmse(test$winPlacePerc,pp)


##Y값을 상, 중, 하 로 범주화 시키기로 결정.(클러스터링을 하기위해)
summary(X$winPlacePerc)

X$rate <- ifelse(X$winPlacePerc<=0.3333,'하', 
                ifelse(X$winPlacePerc<=0.6666, '중','상')) 
X$rate <- as.factor(X$rate)
summary(X)


X_s <- X[,-c(14,2)] #쓸모없는 변수를 지운 새로운 데이터셋 지정
summary(X_s)

training_idx <- createDataPartition(X_s$damage, times = 1, p = 0.7, list = F)
train <- X_s[training_idx,]
test <- X_s[-training_idx,]



###bayes
library(e1071)

m <- naiveBayes(rate ~ ., data = train)
pp = predict(m, newdata=test)
tb_bayes <- table(pp,test$rate)
tb_bayes

sum(diag(tb_bayes))/sum(tb_bayes)


###Decision tree(변수들의 관계를 눈으로 보기 위해 사용)
library(rpart)
rpartmod <- rpart(rate~. , data=train)
plot(rpartmod)
text(rpartmod)
summary(rpartmod)
pp <- predict(rpartmod, newdata=test, type = 'class')
tb_tr <- table(pp,test$rate)
tb_tr

sum(diag(tb_tr))/sum(tb_tr)


###rf
library(randomForest)
rf <- randomForest(rate~.,data= train)
pp <- predict(rf, newdata=test, response = 'class')
tb_rf <- table(pp,test$rate)
tb_rf

sum(diag(tb_rf))/sum(tb_rf)
