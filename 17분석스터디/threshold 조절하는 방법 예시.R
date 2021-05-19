library(ISLR)
library(ROCR)
library(e1071)
library(MASS)
data(College)
RNGkind(sample.kind = "Rounding")
set.seed(1234)
str(College)
summary(College)
tran <- sample(dim(College)[1], floor(dim(College)[1]*0.7))

# 이 밑에 tran.X
RNGkind(sample.kind = "Rounding")
set.seed(12345)
N.lab <- sample(rep(seq(10), length=sum(College$Private[tran]=="No")))
Y.lab <- sample(rep(seq(10), length=sum(College$Private[tran]=="Yes")))
gr <- rep(0, length(tran))
gr[College$Private[tran]=="No"] <- N.lab
gr[College$Private[tran]=="Yes"] <- Y.lab
tran.X <- College[tran,]


## 사용할 모델은 각자 알아서 정해서 밑에 포문에 넣기만 하면 됨. 
## threshold 바꿔가면서 CV 방법 사용하기.
## 파라미터도 조절한다면 threshold for문 -> 파라미터 조절 for문 -> CV for문 순서.
K <- 10 # CV 개수 
alpha <- seq(0,1, by=0.01) # threshold 정하기
alpha.error <- matrix(NA, nrow=length(alpha), ncol=4) # alpha갯수가 행, 사용할 분석기법갯수가 열
cv.error <- matrix(NA, nrow=K, ncol=4) # CV 개수가 행, 사용할 분석기법 갯수가 열
for (i in 1:length(alpha)) { # 알파값에 따라서 CV를 각각 시행해야 하므로 알파부터 for문 사용
  thre=alpha[i] # alpha를 threshold로 지정
  for (j in 1:K) { # CV method
    cv.tran <- which(gr!=j) ; cv.test <- which(gr==j) # train, validation split -> test를 미리 빼서 train과 validation으로 CV 해서 test 적합할 예정.
    cv.y.test <- tran.X$Private[cv.test] # test Y
    
    cv.LR <- predict(glm(Private~., data=tran.X, subset=cv.tran, family='binomial'), # method1 : Logistic regression
                     tran.X[cv.test,], type='response')
    cv.LR_yhat <- ifelse(cv.LR>=thre, 'Yes', 'No') # thereshold에 따라 Yes, No 구분
    
    cv.LDA <- predict(lda(Private~., data=tran.X, subset=cv.tran), tran.X[cv.test,]) # method2 : LDA
    cv.LDA_yhat <- ifelse(cv.LDA$posterior[,2]>=thre, 'Yes', 'No') # threshold 적용 -> posterior는 predict값에서 확률을 뽑음, [,2]는 Yes일 확률
    
    cv.QDA <- predict(qda(Private~., data=tran.X, subset=cv.tran), tran.X[cv.test,]) # method3 : QDA
    cv.QDA_yhat <- ifelse(cv.QDA$posterior[,2]>=thre, 'Yes', 'No') # threshold 적용
    
    cv.NB <- predict(naiveBayes(Private~., data=tran.X, subset=cv.tran), tran.X[cv.test,], type='raw') # method4 : NaiveBayes
    cv.NB_yhat <- ifelse(cv.NB[,2]>=thre, 'Yes', 'No') # threshold 적용
    
    cv.error[j,] <- c(mean(cv.LR_yhat!=cv.y.test), mean(cv.LDA_yhat!=cv.y.test), # 오분류율 구하기
                      mean(cv.QDA_yhat!=cv.y.test), mean(cv.NB_yhat!=cv.y.test))
  }
  alpha.error[i,] <- apply(cv.error, 2, mean) # CV 오분류율 구하기.
}

op.alpha <- apply(alpha.error, 2, function(x) mean(alpha[which(x==min(x))]) ) 
# 각 모델 별 CV 오분류율을 가장 작게 만드는 alpha값 찾기 -> 사용 모델 개수x1 벡터로 나옴. -> 내가한거는 LR ,LDA, QDA, NB순으로 최적의 알파가 4x1임.
# 그냥 min이 아니라 function 쓴 이유는 CV 오분류율이 똑같은 alpha값이 존재하기 때문에 이 alpha값의 평균으로 최적의 알파를 설정.

# 최적의 알파값 적용 후 test set에 대한 오분류율 구하기
op.LR <- predict(glm(Private~., data=College, subset=tran, family='binomial'), College[-tran,], type='response') 
op.LR_yhat <- ifelse(op.LR>=op.alpha[1], 'Yes', 'No') 

op.LDA <- predict(lda(Private~., data=College, subset=tran), College[-tran,])
op.LDA_yhat <- ifelse(op.LDA$posterior[,2]>=op.alpha[2], 'Yes', 'No')

op.QDA <- predict(qda(Private~., data=College, subset=tran), College[-tran,])
op.QDA_yhat <- ifelse(op.QDA$posterior[,2]>=op.alpha[3], 'Yes', 'No')

op.NB <- predict(naiveBayes(Private~., data=College, subset=tran), College[-tran,], type='raw')
op.NB_yhat <- ifelse(op.NB[,2]>=op.alpha[4], 'Yes', 'No')

y.test <- College$Private[-tran]

(Q3 <- data.frame(op.MCR=c(mean(op.LR_yhat!=y.test), mean(op.LDA_yhat!=y.test),
                           mean(op.QDA_yhat!=y.test), mean(op.NB_yhat!=y.test)),
                  op.alpha=op.alpha, row.names=c('Logistic', 'LDA', 'QDA', 'NaiveBayes')))