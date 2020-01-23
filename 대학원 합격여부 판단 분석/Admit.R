library(data.table)
setwd('C:/Users/NT500R/Desktop/AIR 19년도 1학기/graduate')
data <- fread('Admission_Predict.csv',header = T)
data$Research <- as.factor(data$Research)
data$`University Rating` <- as.factor(data$`University Rating`)
summary(data)
#Serial No. : 대학생정보에 부여된 임의의 숫자
#GRE Score :  미국의 대학원수학자격시험 점수(0~346)
#TOEFL Score : 토플 점수(0~120)
#University Rating : 진학 대학교의 등급(1~5)
#SOP : 대학원에 진학하는 동기와 계획을 정리하는 학업계획서(SOP) 점수(1~5)
#LOR : 추천서의 점수(1~5)
#CGPA : 학점(0~10)
#Research : 실험 경험 유무(0 or 1)
#Chance of Admit : 대학원에 합격할 확률. (0~1) -> 이해가 안되는 변수 -> 가정 : (1)대학원에 있는 사람중 한 행과 입학데이터가 같은 사람의 비율
                                                                              
#################################EDA##################################

##Serial No. 변수는 쓸모없는 변수이므로 제거하기로 결정.
data <- data[,-1]

##corrplot
#install.packages('corrplot')
library(corrplot)
#연속형 변수간 corrplot그리기
data_cor<-cor(data[,-c(3,7)])
corrplot(data_cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#대부분의 변수들 간의 선형성이 매우 크다고 볼 수 있다.
#따라서 만약 회귀분석을 진행한다면 다중공선성이 의심 될 수 도 있다.
summary(data)
library(dplyr)
library(ggplot2)
##GRE Score
boxplot(data$`GRE Score`,ylab='GRE Score')
#거의 대칭적인 분포모양을 하고 있다.

#목적변수와의 관계
data %>%
  ggplot(aes(x=`Chance of Admit`,y=`GRE Score`))+geom_point()
#GRE점수가 높아질수록 대학원의 합격률이 높아진다고 볼 수 있다.


##TOEFL Score
boxplot(data$`TOEFL Score`,ylab='TOEFL Score')
#약간 오른쪽으로 치우쳐진 분포모양을 한다.

#목적변수와의 관계
data %>%
  ggplot(aes(x=`Chance of Admit`,y=`TOEFL Score`))+geom_point()
#토플점수가 높아질수록 대학원의 합격률이 높아진다고 볼 수 있다.



##University Rating
data %>%
  group_by(`University Rating`) %>%
  tally() %>%
  mutate(num = n) %>%
  group_by(`University Rating`,num) %>%
  ggplot(aes(x=`University Rating`,y=num,fill=`University Rating`))+geom_histogram(stat='identity')
#관측치의 개수로 보면 3>2>4>5>! 순으로 관측치의 갯수가 크다.
  
#목적변수와의 관계
data %>%
  ggplot(aes(x=`University Rating`,y=`Chance of Admit`,fill=`University Rating`))+geom_boxplot()
#대학의 등급이 높을수록 대학원의 합격률이 높아진다고 볼 수 있다.


##SOP
boxplot(data$SOP,ylab='SOP')
#오른쪽으로 치우쳐진 분포모형임을 알 수 있다.

#목적변수와의 관계
data %>%
  ggplot(aes(x=SOP,y=`Chance of Admit`))+geom_point()+geom_jitter()
#학업계획서의 점수가 높아질수록 대학원 합격률이 높아진다고 볼 수 있다.


##LOR
boxplot(data$LOR,ylab='LOR')

#목적변수와의 관계
data %>%
  ggplot(aes(x=LOR,y=`Chance of Admit`))+geom_point()+geom_jitter()
#추천서의 점수가 높아질수록 대학원의 합격률이 높아진다고 볼 수 있다.


##CGPA
boxplot(data$CGPA,ylab='CGPA')

#목적변수와의 관계
data %>%
  ggplot(aes(x=CGPA,y=`Chance of Admit`))+geom_point()
#학점이 높을수록 대학원의 합격률이 높아진다고 볼 수 있다.


##Research
data %>%
  group_by(Research) %>%
  tally() %>%
  mutate(num = n) %>%
  group_by(Research,num) %>%
  ggplot(aes(x=Research,y=num,fill=Research))+geom_histogram(stat='identity')

#목적변수와의 관계
data %>%
  ggplot(aes(x=Research,y=`Chance of Admit`,fill=Research))+geom_boxplot()
#실험경험이 있으면 대학원의 합격률이 더 높다고 볼 수 있다.


##대학등급에 따른 GRE점수에 대해 차이가 있는가?
data %>%
  ggplot(aes(x=`University Rating`,y=`GRE Score`,fill=`University Rating`))+geom_boxplot()
#대학등급이 높을수록 점수또한 높다고 볼 수 있다.


##대학등급별로 실험경험 유무에 대한 비율차가 있는가?
data %>%
  group_by(`University Rating`,Research) %>%
  tally() %>%
  group_by(`University Rating`) %>%
  mutate(rate = n/sum(n)) %>%
  filter(Research==1) %>%
  ggplot(aes(x=`University Rating`,y=rate,fill=`University Rating`))+geom_histogram(stat='identity')
#대학등급이 높을수록 실험경험이 있을 확률이 높다.


##################Analysis##################
set.seed(201711517)
n <- nrow(data)
idx <- 1:n
train_idx <- sample(idx,n*.7)
training <- data[train_idx,]
validation <- data[-train_idx,]
nrow(training)
nrow(validation)

####LM
lm_1 <- lm(`Chance of Admit`~.,data=training)
summary(lm_1) #모델은 적합하나 유의하지않은 변수들이 많음.
              #stepwise기법을 사용하기로 결정.

library(MASS)
step_mod <- stepAIC(lm_1,direction = 'both')
summary(step_mod) #대학등급 변수만 제거됨을 알 수 있다.
                  #R-Square 값은 0.7956으로 약 80%의 적합도를 보여준다.

y_lm_hat <- predict(step_mod,newdata = validation)
cor(y_lm_hat,validation$`Chance of Admit`)^2 #R-Square 값이 0.8003078으로 약 80%의 적합도를 보여줌
library(Metrics)
rmse(validation$`Chance of Admit`,y_lm_hat) #약 0.06214값을 지님.

library(car)
vif(step_mod) #vif계수가 10을 넘지않으므로 다중공선성을 의심하지 않아도 된다고 생각.

LIN <- c(cor(y_lm_hat,validation$`Chance of Admit`)^2 ,rmse(validation$`Chance of Admit`,y_lm_hat))



#####LASSO & RIDGE
library(glmnet)

xx <- model.matrix(`Chance of Admit`~.-1,data=data)
train_x <- xx[train_idx,]
valid_x <- xx[-train_idx,]
y <- training$`Chance of Admit`

#generaalized linear model
glm_1 <- glmnet(train_x,y)
glm_1

cv_1 <- cv.glmnet(train_x,y)
cv_1 #lambda.min : 그래프의 최솟값, 교차검증의 오차를 최소로하는 람다값
     #lambda.1se : 위의 람다값중 1표준오차만큼 크지만 훨씬 단순한 모델을 만드는 람다값

par(mfrow=c(1,2))
plot(glm_1,main='glmnet') #lambda가 증가함에 따라 계수
plot(cv_1,main='cv.glmnet')

length(which(coef(cv_1,s=cv_1$lambda.min)>0))
length(which(coef(cv_1,s=cv_1$lambda.1se)>0)) #1개를 더 줄여준다.

cv1 <- cv.glmnet(train_x,y,alpha=1) #LASSO : 불필요한 변수의 계수를 0으로 하여 변수의 갯수를 줄이는 효과가 있음. -> 많은변수를 다룰때 사용함.
cv.5 <- cv.glmnet(train_x,y,alpha=.5) #ELASTIC NET : 라쏘 & 릿지 모델의 하이브리드 모형
cv0 <- cv.glmnet(train_x,y,alpha=0) #RIDGE : 변수선택의 의미는 없지만 각 계수들을 줄여준다. -> 다중공선성을 방지.

par(mfrow=c(2,2))
plot(cv1,main='LASSO')
plot(cv.5,main='ELASTIC NET')
plot(cv0,main='RIDGE')

plot(log(cv1$lambda),cv1$cvm,col='red',xlab='log(lambda)',ylab=cv1$name)
points(log(cv.5$lambda),cv.5$cvm,col='blue')
points(log(cv0$lambda),cv0$cvm,col='green')
legend('topleft',
       legend=c('alpha=1.0','alpha=0.5','alpha=0'),
       col=c('red','blue','green'),
       pch=19)

library(knitr)
print_glmnet_coefs <- function(cvfit, s="lambda.min") {
  ind <- which(coef(cvfit, s=s) != 0)
  df <- data.frame(
    feature=rownames(coef(cvfit, s=s))[ind],
    coeficient=coef(cvfit, s=s)[ind]
  )
  kable(df)
}


print_glmnet_coefs(cv1)
print_glmnet_coefs(cv.5)
print_glmnet_coefs(cv0)

#라쏘와 릿지방법 중 어떤 방법이 가장 좋은가?
y_hat_LM <- predict(cv1,s='lambda.min',newx = valid_x)
y_hat_L1 <- predict(cv1,s='lambda.1se',newx = valid_x)
y_hat_RM <- predict(cv0,s='lambda.min',newx = valid_x)
y_hat_R1 <- predict(cv0,s='lambda.1se',newx = valid_x)

#R-Square & RMSE
LM <- c(cor(y_hat_LM,validation$`Chance of Admit`)^2 ,rmse(validation$`Chance of Admit`,y_hat_LM))
L1 <- c(cor(y_hat_L1,validation$`Chance of Admit`)^2 ,rmse(validation$`Chance of Admit`,y_hat_L1))
RM <- c(cor(y_hat_RM,validation$`Chance of Admit`)^2 ,rmse(validation$`Chance of Admit`,y_hat_RM))
R1 <- c(cor(y_hat_R1,validation$`Chance of Admit`)^2,rmse(validation$`Chance of Admit`,y_hat_R1))
mat_R <- rbind(LM,L1,RM,R1)
colnames(mat_R) <- c('R-Square','RMSE')
kable(mat_R)
#라쏘방법과 표준오차를 lambda.min으로 한 모델이 RMSE가 가장 작고 R-square또한 가장 높으므로 가장 좋은 모델임을 알 수 있다.
y_hat_fin <- predict(cv1,s='lambda.min',newx = valid_x)
LM




#Elastic net 의 최적 alpha값 구하기.
alpha <- seq(0.01,0.99,by=0.01)
A <- rep(NA,length(alpha))
B <- rep(NA,length(alpha))
for(i in 1:length(alpha)) { 
  a <- alpha[i]
  cv.A <- cv.glmnet(train_x,y,alpha=a)
  y_hat_A <- predict(cv.A,s='lambda.min',newx = valid_x)
  y_hat_B <- predict(cv.A,s='lambda.1se',newx = valid_x)
  A[i] <- rmse(validation$`Chance of Admit`,y_hat_A)
  B[i] <- rmse(validation$`Chance of Admit`,y_hat_B)
}

alpha[which.min(A)] #표준오차를 lambda.min로 잡았을 때의 최적의 alpha값
A[which.min(A)] #최적의 alpha값을 넣어 만든 모델의 rmse

alpha[which.min(B)] #표준오차를 lambda.1se로 잡았을 때의 최적의 alpha값
B[which.min(B)] #최적의 alpha값을 넣어 만든 모델의 rmse

#A와 B의 최적의 alpha값을 넣어 만든 모델의 rmse를 비교해보면 표준오차를 lambda.min으로 하는 모델이 가장 좋다고 볼 수 있다.
EL <- A[which.min(A)]

#라쏘릿지방법과 엘라스틱넷 RMSE 비교.
LM[2] ; EL
#엘라스틱넷을 사용한 모델의 RMSE가 더 낮으므로 엘라스틱넷이 더 좋다고 볼 수 있다.
alpha[which.min(A)] #ALPHA가 0.81이고 labda.min일때 가장 좋은 모델이다.
EL #0.06236

cv.fin <- cv.glmnet(train_x,y,alpha=alpha[which.min(A)]) #ELASTIC NET : 라쏘 & 릿지 모델의 하이브리드 모형
print_glmnet_coefs(cv.fin)



####RandomForest
library(randomForest)
rf_train <- as.data.frame(training)
rf_mod1 <- randomForest(`Chance of Admit`~.,rf_train)
#Error in eval(predvars, data, env) : object 'GRE Score' not found

library(rpart)
rf_mod1 <- rpart(`Chance of Admit`~.,training)
y_hat_rf <- predict(rf_mod1,validation)
RF <- c(cor(y_hat_rf,validation$`Chance of Admit`)^2 ,rmse(validation$`Chance of Admit`,y_hat_rf))
RF

#선형회귀에서의 가장 좋게 만들어진 모형을 가져와 랜포 돌려보기
rf_mod2 <- rpart(`Chance of Admit`~`GRE Score`+`TOEFL Score`+SOP+LOR+CGPA+Research, data= training)
y_hat_rf_2 <- predict(rf_mod2,validation[,-3])
RF_2 <- c(cor(y_hat_rf_2,validation$`Chance of Admit`)^2 ,rmse(validation$`Chance of Admit`,y_hat_rf_2)) 
RF_2 #RF 와 RF_2가 동일하게 나옴 -> 왜?? -> 있으나마나한 변수였다 생각.

RF_2[2] ; EL
#엘라스틱넷이 RMSE관점에서 더 좋다고 볼 수 있다.



####GBM
library(gbm)
gbm_mod <- gbm(`Chance of Admit`~., training, distribution = 'gaussian', n.trees=1000, shrinkage=0.01, interaction.depth=4 )
summary(gbm_mod)
#CGPA변수가 가장 중요한 변수로 나온다.
#`GRE Score` 은 두번째로 중요한 변수.
plot(gbm_mod,i='CGPA') #그래프로 보아 이 변수는 종속변수에대해 양의 상관관계를 지닌다.

#최적의 나무 갯수 확인하기
n.trees <- seq(from=100 ,to=1000, by=50) 
predmatrix <- predict(gbm_mod,validation,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#MSE 계산하기
test.error<-with(validation,apply( (predmatrix-`Chance of Admit`)^2,2,mean))
head(sqrt(test.error)) #RMSE계산하기.

#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")
abline(h = min(test.error),col="red") #에러가 가장작은 부분 보기.
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)
#나무의 갯수가 350일때 가장 적은 에러를 가진다고 볼 수 있다.
y_hat_gbm <- predict(gbm_mod,validation,n.trees=600)
GBM <- rmse(validation$`Chance of Admit`,y_hat_gbm)

EL ; GBM 
#엘라스틱넷을 사용한 모델이 가장 좋음.
