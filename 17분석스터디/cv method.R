# 5 fold cross-validation (logistic)
library(ISLR)
data(Default)
?Default

X <- Default # variables
y_obs <- ifelse(Default[,1]=='Yes',1,0) # Y
summary(X) 
dim(X) # 10000 X 4

library(glmnet)
library(ROCR)
K <- 5
idx <- sample(1:5, nrow(X), replace=T)
table(idx)
cv_error=NA
for (i in 1:K) {
  train_idx <- which(idx!=i)
  test_idx <- which(idx==i)
  
  train_x <- X[train_idx,]
  test_x <- X[test_idx,] ; test_y <- y[test_idx]
  model <- glm(default~.,data=train_x, family=binomial)
  yhat <- predict(model, test_x)
  pred <- prediction(yhat, test_y)
  cv_error[i] <- 1- performance(pred,'auc')@y.values[[1]]
}
cv_error

final_error <- mean(cv_error)




# In glmnet
X_mat <- data.matrix(X[,-1])
cv.glmnet(X_mat, y_obs, alpha=1, foldid = idx)