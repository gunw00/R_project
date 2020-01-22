rm(list=ls())
library(ggplot2)
library(dplyr)

setwd('C:/Users/NT500R/Desktop/AIR이론 18년도 2학기')
adult <- read.csv('adult.data',header=FALSE,strip.white = T)
names(adult) <- c('age','workclass','fnlwgt','education','education_num','marital_status','occupation','relationship','race','sex','capital_gain','capital_loss','hours_per_week','native_country','wage')

summary(adult)
glimpse(adult)
apply(adult, 2, function(x){sum(x=='?')})
adult <- adult[-which(adult[,2]=='?'),]
adult <- adult[-which(adult[,7]=='?'),]
adult <- adult[-which(adult[,14]=='?'),]
glimpse(adult)
summary(adult)

adult %>%
  ggplot(aes(x=wage, y=age,fill=wage))+geom_boxplot()

adult %>%
  ggplot(aes(x=age,fill=wage))+geom_density(alpha=.5)

adult %>%
  ggplot(aes(x=sex,fill=wage))+ geom_bar(position='dodge')

adult %>%
  summarize(fm=mean(fnlwgt),
            gm=mean(capital_gain),
            lm=mean(capital_loss))

adult %>%
  select(fnlwgt,capital_gain,capital_loss) %>%
  mutate(log_fnl = log(fnlwgt)) %>%
  head(10)

adult %>%
  group_by(wage,sex,race) %>%
  tally() %>%
  group_by(sex,race) %>%
  mutate(rate_mid = n/sum(n)) %>%
  filter(wage=='>50K') %>%
  ggplot(aes(x=race,y=rate_mid,group=sex,color=sex))+geom_point()+geom_line()

adult %>%
  ggplot(aes(x=workclass,y=age,fill=wage,color=wage))+geom_boxplot(alpha=0.7)+coord_flip()

# 고용형태와 급여 별 나이의 박스 플롯으로 각 직업별 나이의 분포모형과 이상치들을 확인 할 수 있다.
# 전체적으로 나이가 많으면 급여를 많이 받는다고 볼 수 있다.

adult %>%
  group_by(wage,workclass,relationship) %>%
  tally() %>%
  group_by(workclass,relationship) %>%
  mutate(rate_mid = n/sum(n)) %>%
  filter(wage=='>50K') %>%
  ggplot(aes(x=relationship,y=rate_mid,group=workclass,color=workclass))+geom_point()+geom_line()

# 고용형태와 가족관계별 급여가 50K가 넘는 사람의 비율을 볼 수 있다
# 가족관계별 급여 차이가 있다고 볼 수 있다.