setwd("C:\\Users\\NT500R\\Desktop\\datascience\\data")
data <- read.csv('lol_diamond_10min.csv', header=T)
library(dplyr)
###### Variables ######
# blueWins : The target column. 1 if the blue team has won, 0 otherwise. -> Factor
# blueWardsPlaced : Number of warding totems placed by the blue team on the map.
# blueWardsDestroyed : Number of enemy warding totems the blue team has destroyed.
# blueFirstBlood : First kill of the game. 1 if the blue team did the first kill, 0 otherwise. -> Factor
# blueKills : Number of enemies killed by the blue team.
# blueDeaths : Number of deaths (blue team).
# blueAssists : Number of kill assists (blue team).
# blueEliteMonsters : Number of elite monsters killed by the blue team (Dragons and Heralds).
# blueDragons : Number of dragons killed by the blue team.
# blueHeralds : Number of heralds killed by the blue team.
# blueTowersDestroyed : Number of structures destroyed by the blue team (towers...).
# blueTotalGold : Blue team total gold.
# blueAvgLevel : Blue team average champion level.
# blueTotalExperience : Blue team total experience.
# blueTotalMinionsKilled : Blue team total minions killed (CS).
# blueTotalJungleMinionsKilled : Blue team total jungle monsters killed.
# blueGoldDiff : Blue team gold difference compared to the enemy team.
# blueExperienceDiff : Blue team experience difference compared to the enemy team.
# blueCSPerMin : Blue team CS (minions) per minute.
# blueGoldPerMin : Blue team gold per minute.
# redWardsPlaced : Number of warding totems placed by the red team on the map.
# redWardsDestroyed : Number of enemy warding totems the red team has destroyed.
# redKills : Number of enemies killed by the red team.
# redDeaths : Number of deaths (red team).
# redAssists : Number of kill assists (red team).
# redEliteMonsters : Number of elite monsters killed by the red team (Dragons and Heralds).
# redDragons : Number of dragons killed by the red team.
# redHeralds : Number of heralds killed by the red team.
# redTowersDestroyed : Number of structures destroyed by the red team (towers...).
# redTotalGold : Red team total gold.
# redAvgLevel : Red team average champion level.
# redTotalExperience : Red team total experience.
# redTotalMinionsKilled : Red team total minions killed (CS).
# redTotalJungleMinionsKilled : Red team total jungle monsters killed.
# redCSPerMin : Red team CS (minions) per minute.
# redGoldPerMin : Red team gold per minute.


###### EDA ######
glimpse(data) # We can find that all variables are numeric type. -> Some variables must change to Factor type.
# Change to Factor type : blueWins, blueFirstBlood, blueDragons, blueHeralds, redDragons, redHeralds
# Remove variables : redFirstBlood, redKills, redDeaths, redEliteMonsters, redCSPerMin, redGoldPerMin,
#                    blueEliteMonsters, redGoldDiff, redExperienceDiff, blueCSPerMin, blueGoldPerMin
remove_idx <- which(colnames(data) %in% c('gameId','redFirstBlood', 'redKills', 'redDeaths', 'redEliteMonsters', 
                      'redCSPerMin', 'redGoldPerMin','blueEliteMonsters', 'redGoldDiff', 
                      'redExperienceDiff', 'blueCSPerMin', 'blueGoldPerMin'))
data_base <- data[,-remove_idx]

data_base$blueWins <- as.factor(as.character(data_base$blueWins))
data_base$blueFirstBlood <- as.factor(as.character(data_base$blueFirstBlood))
data_base$blueDragons <- as.factor(as.character(data_base$blueDragons))
data_base$blueHeralds <- as.factor(as.character(data_base$blueHeralds))
data_base$redDragons <- as.factor(as.character(data_base$redDragons))
data_base$redHeralds <- as.factor(as.character(data_base$redHeralds))

colnames(data_base) <- c('blueWins', 'bWP', 'bWD', 'bFB', 'bK','bD', 'bA', 'bDr', 
                         'bH', 'bTD', 'bTG', 'bAL', 'bTE', 'bTCS', 'bTJMK', 'bGD',
                         'bED', 'rWP', 'rWD', 'rA', 'rDr', 'rH', 'rTD', 'rTG', 
                         'rAL', 'rTE', 'rTCS', 'rTJMK')
## Change variables name ##
# b : blue
# r : red
# WP : WardsPlaced
# WD : wardsDestroyed
# FB : FirstBlood
# K : Kills
# D : Deaths
# A : Assist
# Dr : Dragons
# H : Heralds
# TD : TowersDestroyed
# TG : TotalGold
# AL : AvgLevel
# TE : TotalExperience
# TCS : TotalMinionsKilled
# TJMK : TotalJungleMinionsKilled
# GD : GoldDiff
# ED : ExperienceDiff
# WSD : Wards Score Diff
library(ggplot2)
glimpse(data_base)
summary(data_base)
data_base[data_base$bWP==250,]

data <- data_base

boxplot(data$bWP) # When bWP and rWP > 100 -> outlier -> remove

data[data$bWP>200,] %>%
  ggplot(aes(x=blueWins, fill=blueWins)) +geom_bar()

data2 <- data[-which(data$bWP>100 | data$rWP>100),]
summary(data2)

### Plot y~x ###
# bWP
data2 %>%
  ggplot(aes(x=bWP, fill=blueWins))+geom_density(alpha=.3) + ggtitle("Y ~ blue Wards Placed") +xlim(30,100)

data2 %>%
  ggplot(aes(x=bWP, fill=blueWins))+geom_histogram(bins=50) + ggtitle("Y ~ blue Wards Placed")

# bWD
data2 %>%
  ggplot(aes(x=bWD, fill=blueWins))+geom_density(alpha=.3) + ggtitle("Y ~ blue Wards Distroyed") +xlim(10,40)

# bWSD : (bWP+bWD) - (rWP+rWD)
data2$bWSD <- data2$bWP+data2$bWD - data2$rWP-data2$rWD

summary(data2$bWSD)
data2 %>%
  ggplot(aes(x=bWSD, fill=blueWins))+geom_density(alpha=.3) + ggtitle("Y ~ Wards Score Diff")


# bFB 
prop.table(table(data2$blueWins, data2$bFB),1)[2,1:2]
data2 %>%
  group_by(bFB, blueWins) %>%
  tally() %>%
  group_by(blueWins) %>%
  mutate(prob_FB = n/sum(n)) %>%
  filter(blueWins==1) %>%
  ggplot(aes(x=bFB, y=prob_FB,fill=bFB))+geom_bar(stat='identity')+ ggtitle("Prob. of blue First Blood ablut blueWins")+
  geom_label(aes(label=round(prob_FB, 5)), fontface='bold')


# bK
data2 %>%
  ggplot(aes(x=blueWins, y=bK, fill=blueWins))+geom_boxplot()

# bD
data2 %>%
  ggplot(aes(x=blueWins, y=bD, fill=blueWins))+geom_boxplot()

# bA 
data2 %>%
  ggplot(aes(x=blueWins, y=bA, fill=blueWins))+geom_boxplot()


# bDr 
summary(data2$bDr)

prop.table(table(data2$blueWins, data2$bDr),2)[2,1:2]
data2 %>%
  group_by(bDr, blueWins) %>%
  tally() %>%
  group_by(blueWins) %>%
  mutate(prob_Dr = n/sum(n)) %>%
  filter(blueWins==1) %>%
  ggplot(aes(x=bDr, y=prob_Dr,fill=bDr))+geom_bar(stat='identity')+ ggtitle("Prob. of blue Dragons about blueWins")+
  geom_label(aes(label=round(prob_Dr, 5)), fontface='bold')

# bH
summary(data2$bH)
prop.table(table(data2$blueWins, data2$bH),1)[2,1:2]
data2 %>%
  group_by(bH, blueWins) %>%
  tally() %>%
  group_by(blueWins) %>%
  mutate(prob_H = n/sum(n)) %>%
  filter(blueWins==1) %>%
  ggplot(aes(x=bH, y=prob_H,fill=bH))+geom_bar(stat='identity')+ ggtitle("Prob. of blue Heralds about blueWins")+
  geom_label(aes(label=round(prob_H, 5)), fontface='bold')

data3 <- data2[-which(data2$bDr==data2$bH | data2$rDr==data2$rH),]
t.test(as.numeric(as.character(data3[data3$blueWins==1,'bH'])), alternative='greater', mu=0.5)

# bTD
summary(data2$bTD)
data2 %>%
  ggplot(aes(x=bTD, fill=blueWins))+geom_density(alpha=.4)

data2 %>%
  ggplot(aes(x=bTD, fill=blueWins))+geom_histogram(alpha=.4)

# bTG
summary(data2$bTG)
data2 %>%
  ggplot(aes(x=bTG, fill=blueWins))+geom_density(alpha=.4)

# bAL
summary(data2$bAL)
data2 %>%
  ggplot(aes(x=bAL, fill=blueWins))+geom_density(alpha=.4)

# bTE
summary(data2$bTE)
data2 %>%
  ggplot(aes(x=bTE, fill=blueWins))+geom_density(alpha=.4)

# bTCS
summary(data2$bTCS)
data2 %>%
  ggplot(aes(x=bTCS, fill=blueWins))+geom_density(alpha=.4)

# bTJMK
data2 %>% 
  ggplot(aes(bTJMK,fill= blueWins)) + geom_density(alpha=.5)

# bGD
data2 %>% 
  ggplot(aes(bGD,fill= blueWins)) + geom_density(alpha=.5)

# bED
data2 %>% 
  ggplot(aes(bED,fill= blueWins)) + geom_density(alpha=.5)

# rA
data2 %>% 
  ggplot(aes(rA,fill= blueWins)) + geom_density(alpha=.5)

# rTD
data2 %>% ggplot(aes(rTD,fill= blueWins)) + geom_density(alpha=.5)

# rTG
data2 %>% ggplot(aes(rTG,fill= blueWins)) + geom_density(alpha=.5)

# rAL
data2 %>% 
  ggplot(aes(rAL,fill= blueWins)) + geom_density(alpha=.5)

# rTE
data2 %>% 
  ggplot(aes(rTE,fill= blueWins)) + geom_density(alpha=.5)

# rTCS
data2 %>% 
  ggplot(aes(rTCS,fill= blueWins)) + geom_density(alpha=.5)

# rTJMK
data2 %>% ggplot(aes(rTJMK,fill= blueWins)) + geom_density(alpha=.5)



########################### Modeling ################################

library(dplyr)
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)
library(ROCR)
library(caret)

set.seed(0920)
n <- nrow(data2)
idx <- 1:n
training_idx <- sample(idx, n*0.6)
idx <- setdiff(idx, training_idx)
valid_idx <- sample(idx, n*0.2)
test_idx <- setdiff(idx,valid_idx)
length(training_idx)
length(valid_idx)
length(test_idx)

train <- data2[training_idx,]
valid <- data2[valid_idx,]
test <- data2[test_idx,]

glimpse(train)
####### Logistic Regression ######

# Full
lol_lr <- glm(blueWins~.-bWSD-bGD-bED, data=train, family=binomial)
summary(lol_lr)

y_obs <- ifelse(valid$blueWins=='1', 1, 0)
yhat_lr <- predict(lol_lr, newdata=valid, type='response')

pred_lr <- prediction(yhat_lr, y_obs)
perf_lr <- performance(pred_lr, measure='tpr', x.measure='fpr')
plot(perf_lr, col='black', main='ROC Curve for Logistic')
abline(0,1)
performance(pred_lr, 'auc')@y.values[[1]]

# bWSD in
lol_lr_2 <- glm(blueWins~.-bWP-bWD-rWP-rWD-bGD-bED, data=train, family=binomial)
summary(lol_lr_2)

yhat_lr_2 <- predict(lol_lr_2, newdata=valid, type='response')

pred_lr_2 <- prediction(yhat_lr_2, y_obs)
perf_lr_2 <- performance(pred_lr_2, measure='tpr', x.measure='fpr')
plot(perf_lr_2, col='black', main='ROC Curve for Logistic')
abline(0,1)
performance(pred_lr_2, 'auc')@y.values[[1]]

# bGD in
lol_lr_3 <- glm(blueWins~.-bWSD-bED-bTG-rTG, data=train, family=binomial)
summary(lol_lr_3)

yhat_lr_3 <- predict(lol_lr_3, newdata=valid, type='response')

pred_lr_3 <- prediction(yhat_lr_3, y_obs)
perf_lr_3 <- performance(pred_lr_3, measure='tpr', x.measure='fpr')
plot(perf_lr_3, col='black', main='ROC Curve for Logistic')
abline(0,1)
performance(pred_lr_3, 'auc')@y.values[[1]]

# bED in
lol_lr_4 <- glm(blueWins~.-bWSD-bGD-bTE-rTE, data=train, family=binomial)
summary(lol_lr_4)

yhat_lr_4 <- predict(lol_lr_4, newdata=valid, type='response')

pred_lr_4 <- prediction(yhat_lr_4, y_obs)
perf_lr_4 <- performance(pred_lr_4, measure='tpr', x.measure='fpr')
plot(perf_lr_4, col='black', main='ROC Curve for Logistic')
abline(0,1)
performance(pred_lr_4, 'auc')@y.values[[1]]

# bWSD & bGD in
lol_lr_5 <- glm(blueWins~.-bWP-bWD-rWP-rWD-bTG-rTG-bED, data=train, family=binomial)
summary(lol_lr_5)

yhat_lr_5 <- predict(lol_lr_5, newdata=valid, type='response')

pred_lr_5 <- prediction(yhat_lr_5, y_obs)
perf_lr_5 <- performance(pred_lr_5, measure='tpr', x.measure='fpr')
plot(perf_lr_5, col='black', main='ROC Curve for Logistic')
abline(0,1)
performance(pred_lr_5, 'auc')@y.values[[1]]

# bWSD & bED in
lol_lr_6 <- glm(blueWins~.-bWP-bWD-rWP-rWD-bGD-bTE-rTE, data=train, family=binomial)
summary(lol_lr_6)

yhat_lr_6 <- predict(lol_lr_6, newdata=valid, type='response')

pred_lr_6 <- prediction(yhat_lr_6, y_obs)
perf_lr_6 <- performance(pred_lr_6, measure='tpr', x.measure='fpr')
plot(perf_lr_6, col='black', main='ROC Curve for Logistic')
abline(0,1)
performance(pred_lr_6, 'auc')@y.values[[1]]

# bWSD & bED & bGD in
lol_lr_7 <- glm(blueWins~.-bWP-bWD-rWP-rWD-bTG-rTG-bTE-rTE, data=train, family=binomial)
summary(lol_lr_7)

yhat_lr_7 <- predict(lol_lr_7, newdata=valid, type='response')

pred_lr_7 <- prediction(yhat_lr_7, y_obs)
perf_lr_7 <- performance(pred_lr_7, measure='tpr', x.measure='fpr')
plot(perf_lr_7, col='black', main='ROC Curve for Logistic')
abline(0,1)
performance(pred_lr_7, 'auc')@y.values[[1]]

# stepAIC in lol_lr_7
lol_lr_step <- stepAIC(lol_lr_7, direction='both')
summary(lol_lr_step)

yhat_lr_step <- predict(lol_lr_step, newdata=valid, type='response')

pred_lr_step <- prediction(yhat_lr_step, y_obs)
perf_lr_step <- performance(pred_lr_step, measure='tpr', x.measure='fpr')
plot(perf_lr_step, col='black', main='ROC Curve for Logistic')
abline(0,1)
performance(pred_lr_step, 'auc')@y.values[[1]] # Best model in Logistic Regression 



####### Regularization method #######
xx <- model.matrix(blueWins~.-1, data2)
x <- xx[training_idx,]
y <- ifelse(train$blueWins=='1', 1, 0)

## Lasso regression model
set.seed(0920)
lol_lasso <-  cv.glmnet(x, y, family='binomial')
plot(lol_lasso) # lambda.1se : 4 variables select
                # lambda.min : 12 variables select
coef(lol_lasso, s='lambda.1se')
coef(lol_lasso, s='lambda.min')


yhat_las_1se <- predict(lol_lasso, s='lambda.1se', newx=xx[valid_idx,], type='response')[,1]
yhat_las_min <- predict(lol_lasso, s='lambda.min', newx=xx[valid_idx,], type='response')[,1]

pred_las_1se <- prediction(yhat_las_1se, y_obs)
pred_las_min <- prediction(yhat_las_min, y_obs)

perf_las_1se <- performance(pred_las_1se, measure='tpr', x.measure='fpr')
perf_las_min <- performance(pred_las_min, measure='tpr', x.measure='fpr')

plot(perf_las_1se, col='black', main='ROC Curve')
plot(perf_las_min, col='blue', add=T)
abline(0,1)
legend('bottomright', inset=.1, legend=c('1se','min'), col=c('black', 'blue'), lty=1, lwd=2)
performance(pred_las_1se, 'auc')@y.values[[1]]
performance(pred_las_min, 'auc')@y.values[[1]] # Best model in Lasso Regression using lambda.min



## Ridge Regression model
set.seed(0920)
lol_ridge <-  cv.glmnet(x, y, family='binomial', alpha=0)
plot(lol_ridge) # lambda.1se : 4 variables select
# lambda.min : 12 variables select

yhat_rid_1se <- predict(lol_ridge, s='lambda.1se', newx=xx[valid_idx,], type='response')[,1]
yhat_rid_min <- predict(lol_ridge, s='lambda.min', newx=xx[valid_idx,], type='response')[,1]

pred_rid_1se <- prediction(yhat_rid_1se, y_obs)
pred_rid_min <- prediction(yhat_rid_min, y_obs)

perf_rid_1se <- performance(pred_rid_1se, measure='tpr', x.measure='fpr')
perf_rid_min <- performance(pred_rid_min, measure='tpr', x.measure='fpr')

plot(perf_rid_1se, col='black', main='ROC Curve')
plot(perf_rid_min, col='blue', add=T)
abline(0,1)
legend('bottomright', inset=.1, legend=c('1se','min'), col=c('black', 'blue'), lty=1, lwd=2)
performance(pred_rid_1se, 'auc')@y.values[[1]]
performance(pred_rid_min, 'auc')@y.values[[1]] # Best model in Ridge Regression using lambda.min



## Elasticnet Regression model
alpha = seq(0.2 , 0.3, by=0.01)
auc_elastic_1se = NA
auc_elastic_min = NA
best_elastic = matrix(NA, 2, 2)
colnames(best_elastic) <- c('alpha', 'AUC')
rownames(best_elastic) <- c('lambda.1se', 'lambda.min')

set.seed(0920)
for (i in 1:length(alpha)) {
  lol_El <- cv.glmnet(x, y, family='binomial', alpha=alpha[i])
  
  yhat_El_1se <- predict(lol_El, s='lambda.1se', newx=xx[valid_idx,], type='response')[,1]
  yhat_El_min <- predict(lol_El, s='lambda.min', newx=xx[valid_idx,], type='response')[,1]
  
  pred_El_1se <- prediction(yhat_El_1se, y_obs)
  pred_El_min <- prediction(yhat_El_min, y_obs)
  
  auc_elastic_1se[i] <- performance(pred_El_1se, 'auc')@y.values[[1]]
  auc_elastic_min[i] <- performance(pred_El_min, 'auc')@y.values[[1]]
}

best_elastic[1,1] <- alpha[which.max(auc_elastic_1se)]
best_elastic[2,1] <- alpha[which.max(auc_elastic_min)]
best_elastic[1,2] <- max(auc_elastic_1se)
best_elastic[2,2] <- max(auc_elastic_min)

best_elastic # Best model in Elasticnet Regression using alpha=0.22 and lambda.1se



####### RandomForest #######
set.seed(0920)

ntree = seq(500, 1000, by=100)
auc_rf = NA
best_rf = matrix(NA, 1, 2)
colnames(best_rf) <- c('ntree', 'AUC')
for (i in 1:length(ntree)) {
  lol_rf <- randomForest(blueWins~., train, ntree=ntree[i])
  
  yhat_rf <- predict(lol_rf, newdata=valid, type='prob')[,'1']

  pred_rf <- prediction(yhat_rf, y_obs)

  auc_rf[i] <- performance(pred_rf, 'auc')@y.values[[1]]
}
best_rf[,1] <- ntree[which.max(auc_rf)]
best_rf[,2] <- max(auc_rf)

best_rf # Best model in RandomForest using ntree=600



####### Boosting #######
set.seed(0920)
train_gbm <- train %>% mutate(blueWins=ifelse(blueWins=='1', 1, 0))
shrinkage = seq(0.01, 0.1, by=0.01)
auc_gbm = NA
best_gbm = matrix(NA, 1, 2)
colnames(best_gbm) <- c('shrinkage', 'AUC')
for (i in 1:length(shrinkage)) {
  lol_gbm <- gbm(blueWins~., data=train_gbm, distribution = 'bernoulli', 
                 n.tree=5000, cv.folds=3, verbose = T, shrinkage=shrinkage[i])
  best_iter <- gbm.perf(lol_gbm, method='cv')
  
  yhat_gbm <- predict(lol_gbm, n.trees=best_iter, newdata=valid, type='response')
  
  pred_gbm <- prediction(yhat_gbm, y_obs)
  
  auc_gbm[i] <- performance(pred_gbm, 'auc')@y.values[[1]]
}
best_gbm[,1] <- shrinkage[which.max(auc_gbm)]
best_gbm[,2] <- max(auc_gbm)

best_gbm # Best model in gbm using shrinkage=0.05


########### Test set fit ############
# Logistic Regression : Using stepAIC
y_obs_test <- ifelse(test$blueWins=='1', 1, 0)
test_yhat_lr_step <- predict(lol_lr_step, newdata=test, type='response')

test_pred_lr_step <- prediction(test_yhat_lr_step, y_obs_test)

test_perf_lr_step <- performance(test_pred_lr_step, measure='tpr', x.measure='fpr')

# Lasso Regression : Using lambda.min
test_yhat_las <- predict(lol_lasso, s='lambda.min', newx=xx[test_idx,], type='response')[,1]

test_pred_las <- prediction(test_yhat_las, y_obs_test)

test_perf_las <- performance(test_pred_las, measure='tpr', x.measure='fpr')

# Ridgge Regression : Using lambda.min
test_yhat_rid <- predict(lol_ridge, s='lambda.min', newx=xx[test_idx,], type='response')[,1]

test_pred_rid <- prediction(test_yhat_rid, y_obs_test)

test_perf_rid <- performance(test_pred_rid, measure='tpr', x.measure='fpr')

# Elasicnet Regression : Using alpha=0.22 and lambda.1se
set.seed(0920)
lol_El_best <- cv.glmnet(x, y,  family='binomial', alpha=0.22)
test_yhat_El <- predict(lol_El_best, s='lambda.1se', newx=xx[test_idx,], type='response')[,1]

test_pred_El <- prediction(test_yhat_El, y_obs_test)

test_perf_El <- performance(test_pred_El, measure='tpr', x.measure='fpr')

# RandomForest model : Using ntree=600
set.seed(0920)
lol_rf_best <- randomForest(blueWins~., train, ntree=600)
varImpPlot(lol_rf_best)
plot(lol_rf_best)
test_yhat_rf <- predict(lol_rf_best, newdata=test, type='prob')[,'1']

test_pred_rf <- prediction(test_yhat_rf, y_obs_test)

test_perf_rf <- performance(test_pred_rf, measure='tpr', x.measure='fpr')

# gbm : Using shrinkage=0.05
set.seed(0920)
lol_gbm_best <- gbm(blueWins~., data=train_gbm, distribution = 'bernoulli', 
               n.tree=5000, cv.folds=3, verbose = T, shrinkage=0.05)
best_iter <- gbm.perf(lol_gbm_best, method='cv')

test_yhat_gbm <- predict(lol_gbm_best, n.trees=best_iter, newdata=test, type='response')

test_pred_gbm <- prediction(test_yhat_gbm, y_obs_test)

test_perf_gbm <- performance(test_pred_gbm, measure='tpr', x.measure='fpr')

##### ROC Curve #####
plot(test_perf_lr_step, col='black', main='ROC Curve')
plot(test_perf_las, col='blue',add=T)
plot(test_perf_rid, add=T, col='red')
plot(test_perf_El, add=T, col='cyan')
plot(test_perf_rf, add=T, col='orange')
plot(test_perf_gbm, add=T, col='yellow')
abline(0,1)
legend('bottomright', inset=.1, legend=c('Log. Reg.', 'Lasso', 'Ridge', 'Elasticnet', 'RF', 'GBM'),
       col=c('black','blue','red','cyan', 'orange', 'yellow'), lty=1,lwd=2)


###### AUC  Comparison ######
data.frame(method = c('Log. Reg.', 'Lasso', 'Ridge', 'Elasticnet', 'RandomForest', 'GBM'),
           AUC = round(c(performance(test_pred_lr_step, 'auc')@y.values[[1]],
                          performance(test_pred_las, 'auc')@y.values[[1]],
                          performance(test_pred_rid, 'auc')@y.values[[1]],
                          performance(test_pred_El, 'auc')@y.values[[1]],
                          performance(test_pred_rf, 'auc')@y.values[[1]],
                          performance(test_pred_gbm, 'auc')@y.values[[1]]), 6))

# Elasticnet Regression is the best model. 
# But, We want to calculate variables importance.
# So, We can't use Regularization model.
# Exccept Regularization model, Best model is Logistic Regression model.
(best_model_varimp <- caret::varImp(lol_lr_step))
obs <- order(best_model_varimp, decreasing=T)
Imp_var <- data.frame(var_Imp = best_model_varimp[[1]][obs])
rownames(Imp_var) <- rownames(best_model_varimp)[obs]

Imp_var %>%
  ggplot(aes(x=var_Imp))+geom_point(stat='identity')


(best_model_varimp <- caret::varImp(lol_lr_step))
obs <- order(best_model_varimp, decreasing=T)
ordering <- data.frame(var_name = rownames(best_model_varimp)[obs],
                       varImp = best_model_varimp[[1]][obs])
ordering %>% ggplot(aes(var_name,varImp)) + geom_point(stat='identity')