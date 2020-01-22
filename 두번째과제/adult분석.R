###전처리###
summary(adult)
glimpse(adult)
apply(adult, 2, function(x){sum(x=='?')})
adult <- adult[-which(adult[,2]=='?'),]
adult <- adult[-which(adult[,7]=='?'),]
adult <- adult[-which(adult[,14]=='?'),]
#데이터가 ?인 데이터 모두 제거


####EDA####
###직업과 성별에 따른 중산층의 비율 히스토그램

adult %>%
  mutate(mid=ifelse(wage=='>50K',1,0))%>%
  group_by(sex,occupation,mid)%>%
  tally()%>%
  group_by(sex,occupation)%>%
  mutate(prob_mid=n/sum(n))%>%
  filter(mid==1)%>%
  ggplot(aes(x=occupation,y=prob_mid,colour=sex,group=sex))+
  geom_point()+geom_line()

adult[which(adult[,7]=='Armed-Forces'),10] #여자 표본이 없음.

adult[which(adult[,7]=='Priv-house-serv'),10] #남자 표본이 있으나 중산층비율이 없다고 해석 할 수 있음.

adult[which(adult[,7]=='Priv-house-serv' & adult[,10]=='Male'),15] #이 직업의 중산층 남자의 표본은 없음.

#대체로 모든 직업에 있어 남성 중산층의 비율이 여성 중산층의 비율보다 크다.

##자본이익기록, 자본손실기록
table(adult$capital_gain)
table(adult$capital_loss)
cor(adult$capital_gain,adult$capital_loss) #두개의 변수를 합칠려 했지만 서로의 상관관계가 거의 없음을 확인함으로 무산.

plot(adult$capital_gain,adult$capital_loss)
plot(adult$wage,adult$capital_loss)

adult %>%
  select(capital_gain, capital_loss) %>%
  filter(capital_gain!=0 & capital_loss!=0)  #두개의 변수 모두 0이 아닌 데이터 행은 없음.
  

###고용형태와 성별에 따른 중산층 비율  
adult %>%
  mutate(mid=ifelse(wage=='>50K',1,0))%>%
  group_by(sex,workclass,mid)%>%
  tally()%>%
  group_by(sex,workclass)%>%
  mutate(prob_mid=n/sum(n))%>%
  filter(mid==1)%>%
  ggplot(aes(x=workclass,y=prob_mid,colour=sex,group=sex))+
  geom_point()+geom_line()

#고용형태에 따라 중산층의 비율이 확실히 차이가난다고 볼 수 있다.


###결혼상태와 성별에 따른 중산층 비율 
adult %>%
  mutate(mid=ifelse(wage=='>50K',1,0))%>%
  group_by(sex,marital_status,mid)%>%
  tally()%>%
  group_by(sex,marital_status)%>%
  mutate(prob_mid=n/sum(n))%>%
  filter(mid==1)%>%
  ggplot(aes(x=marital_status,y=prob_mid,colour=sex,group=sex))+
  geom_point()+geom_line()

#결혼상태에 따라 중산층의 비율이 확실히 차이가 난다고 볼 수 있다.


###순위형 교육수준과 수치형 교육수준의 비교
adult %>%
  select(education,education_num) %>%
  table()
#각각의 변수에 포함된 데이터의 갯수가 동일 -> 두개의 변수는 동일한 가치임을 확인.(그럼 왜 두개의 변수가 작용하는 중요도가 다른걸까?)

##순위형 교육수준에 따른 중산층의 비율
adult %>%
  group_by(education_num, wage) %>%
  tally() %>%
  group_by(education_num) %>%
  mutate(rate_middle = n / sum(n)) %>%
  filter(wage==">50K") %>%
  ggplot(aes(x=education_num, y=rate_middle, fill=education_num)) + geom_bar(stat='identity')
#교육수준에 따라 중산층의 비율도 증가하는 관계가 있다고 볼 수 있음.
  



#####rf#####
set.seed(201711517)
n <- nrow(adult)
idx <- 1:n
training_idx <- sample(idx, n*.6)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n*.2)
test_idx <- setdiff(idx, validate_idx)
length(training_idx)
length(validate_idx)
length(test_idx)
training <- adult[training_idx,]
validation <- adult[validate_idx,]
test <- adult[test_idx,]

glimpse(training)

###rf_case1### 모든변수
rf_c1 <- randomForest(wage~.,training)
rf_c1
plot(rf_c1)

imp <- importance(rf_c1)
imp[order(-imp[,1]), 1, drop=F]
varImpPlot(rf_c1) #하위변수 sex,race 확인.

y_hat_rf <- predict(rf_c1, newdata=validation, type='prob')[,'>50K']
y_obs <- ifelse(validation$wage == '>50K', 1, 0)
pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')
abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]]


y_hat_rf <- predict(rf_c1, newdata=test, type='prob')[,'>50K']
y_obs <- ifelse(test$wage == '>50K', 1, 0)
pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')
abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]]

###rf_case2### sex,race 변수 제거 모델
rf_c2 <- randomForest(wage~.-sex-race,training)

plot(rf_c2)

imp <- importance(rf_c2)
imp[order(-imp[,1]), 1, drop=F]
varImpPlot(rf_c2)

y_hat_rf <- predict(rf_c2, newdata=validation, type='prob')[,'>50K']
y_obs <- ifelse(validation$wage == '>50K', 1, 0)
pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')

abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]]

y_hat_rf <- predict(rf_c2, newdata=test, type='prob')[,'>50K']
y_obs <- ifelse(test$wage == '>50K', 1, 0)
pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')
abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]]



###rf_case3### sex,race와 edu,edu_num이 같은 의미를 지녔다고 생각하였으므로 둘중 변수중요도가 낮게나온 education변수 제거.
rf_c3 <- randomForest(wage~.-sex-race-education,training)
plot(rf_c3)

imp <- importance(rf_c3)
imp[order(-imp[,1]), 1, drop=F]
varImpPlot(rf_c3)

y_hat_rf <- predict(rf_c3, newdata=validation, type='prob')[,'>50K']
y_obs <- ifelse(validation$wage == '>50K', 1, 0)
pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')
abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]]


y_hat_rf <- predict(rf_c3, newdata=test, type='prob')[,'>50K']
y_obs <- ifelse(test$wage == '>50K', 1, 0)
pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')
abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]]


###rf_case4### rf_c3의 변수중요도테스트 최하위 변수 capital_loss제거
rf_c4 <- randomForest(wage~.-sex-race-education-capital_loss,training)
plot(rf_c4)

imp <- importance(rf_c4)
imp[order(-imp[,1]), 1, drop=F]
varImpPlot(rf_c4)



y_hat_rf <- predict(rf_c4, newdata=validation, type='prob')[,'>50K']
y_obs <- ifelse(validation$wage == '>50K', 1, 0)
pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')
abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]]


y_hat_rf <- predict(rf_c4, newdata=test, type='prob')[,'>50K']
y_obs <- ifelse(test$wage == '>50K', 1, 0)
pred_rf <- prediction(y_hat_rf, y_obs)
perf_rf <- performance(pred_rf, measure='tpr', x.measure='fpr')
plot(perf_rf, col='black',main='ROC Curve')
abline(0,1)
legend('bottomright',
       legend='RF', col='black', lty=1, lwd=2)
performance(pred_rf, "auc")@y.values[[1]]

#######rf_c3의 auc값이 0.9098635로 가장 높은 값을 지님.