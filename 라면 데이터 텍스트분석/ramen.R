library(dplyr)
library(ggplot2)
library(data.table)
library(knitr)
setwd('C:/Users/NT500R/Desktop/AIR 19년도 1학기/ramen-ratings')
data <- read.csv('ramen-ratings.csv',header=T)
################변수설명###################
#Review : 상품에 달린 리뷰의 갯수
#Brand : 상품의 브랜드회사
#Variety : 상품의 특징
#Style : 상품의 형태(팩,컵,접시,그릇)
#Country : 상품생산된 나라
#Stars : 상품의 평점
#Top.Ten : 년도별 탑텐 선정.

colnames(data)[1] <- c('Review')

###################EDA#####################
summary(data)
glimpse(data)
data$Stars <- as.numeric(as.character(data$Stars))

#NA
length(which(is.na(data$Variety)))
length(which(is.na(data$Review)))
length(which(is.na(data$Brand)))
length(which(is.na(data$Style)))
length(which(is.na(data$Country)))
length(which(is.na(data$Stars))) #3개의 NA데이터 발견.
length(which(is.na(data$Top.Ten))) 
#Warning message:
#In is.na(data$Top.ten) :
#is.na() applied to non-(list or vector) of type 'NULL'
#Top.ten 데이터들은 NA가 아닌 NULL형태. 즉, 변환 필요

##Stars 변수 NA 처리
data <- data[-which(is.na(data$Stars)==TRUE),]
length(which(is.na(data$Stars)==TRUE)) #NA데이터 처리확인.
data$Stars <- as.numeric(as.character(data$Stars))


##Top.Ten data
table(data$Top.Ten)
#12년도 8등, 13년도 5등,7등,8등, 
#14년도 2등,3등, 15년도 2등,3등,5등, 
#16년도 2,3,4,6등 NA
#년도 별 수상한 등수가 겹치거나 다르게 수상하여 등수에 NA가 생긴거라 예상
#즉 이 변수를 수상을 했고 안했고로 나눠서 처리.
#'\n'이라는 데이터를 해석 할 수 없으므로 제거하기로 결정.
data<- data[-which(data$Top.Ten=='\n'),]
table(data$Top.Ten)#제거 확인.
#변수를 수상여부로 새로운 변수 만들기
data$award <- ifelse(data$Top.Ten=='',0,1)
data$award <- as.factor(data$award) #Binary data로 지정.


##Review & Stars
data %>%
  ggplot(aes(x=Review,y=Stars))+geom_point()+geom_jitter()
#조금의 선형성이 보이므로 리뷰의 갯수가 많으면 점수도 높을 가능성이 있다고 볼 수 있다.


##Brand
unique(data$Brand)
#355개의 브랜드로 나뉘어져 있음.
sort(table(data$Brand),decreasing=T)
#특정 브랜드가 수상을 많이 했는가?
data_award <- data[which(data$award==1),]
sort(table(data_award$Brand),decreasing = T)
#제품을 많이 내는 브랜드라고 해서 수상을 한것이 아님을 알 수 있다.

#수상한 브랜드의 시각화.
A<-data %>%
  group_by(Brand,award) %>%
  filter(award==1) %>%
  tally() %>%
  mutate(win=n) %>%
  tally()

A<-as.data.frame(A)
glimpse(A)
A$nn <- as.numeric(A$nn)

A %>%
  ggplot(aes(x=Brand,y=nn,fill=nn))+geom_bar(stat='identity') +coord_flip()
#상을 가장 많이 수상한 회사는 prima taste가 가장 많이 수상하였다.



##Style
table(data$Style)
#''로 된 데이터는 삭제, Can은 데이터 수가 1 이므로 의미없다고 생각.
#bar로 된 라멘은 없으므로 오기로 판단하여 삭제.
#box또한 컵과 팩의 중의적 의미를 담고 있으므로 삭제하기로 결정.
data <- data[-which(data$Style=='' | data$Style=='Box' | data$Style=='Bar' | data$Style=='Can'),]

#Style & Stars
data %>%
  ggplot(aes(x=Style,y=Stars,fill=Style))+geom_boxplot()
#형태에 따른 평균차가 없어 보임.


##country
table(data$Country)
#나라 변수는 38개의 나라로 된 레벨로 범주화 되어있음.
#나라별 데이터가 겹치기도 하고 ex) U.S.A & U.S 데이터의 수도 각기 달라서
#대륙별로 데이터를 합치기로 결정.

data$Continent[data$Country=="Australia"]<-"Australia/Oceania"
data$Continent[data$Country=="Bangladesh"]<-"Asia"
data$Continent[data$Country=="Brazil"]<-"South America"
data$Continent[data$Country=="Cambodia"]<-"Asia"
data$Continent[data$Country=="Canada"]<-"North America"
data$Continent[data$Country=="China"]<-"Asia"
data$Continent[data$Country=="Colombia"]<-"South America"
data$Continent[data$Country=="Dubai"]<-"Asia"
data$Continent[data$Country=="Estonia"]<-"Europe"
data$Continent[data$Country=="Fiji"]<-"Australia/Oceania"
data$Continent[data$Country=="Finland"]<-"Europe"
data$Continent[data$Country=="Germany"]<-"Europe"
data$Continent[data$Country=="Ghana"]<-"Africa"
data$Continent[data$Country=="Holland"]<-"Europe"
data$Continent[data$Country=="Hong Kong"]<-"Asia"
data$Continent[data$Country=="Hungary"]<-"Europe"
data$Continent[data$Country=="India"]<-"Asia"
data$Continent[data$Country=="Indonesia"]<-"Asia"
data$Continent[data$Country=="Japan"]<-"Asia"
data$Continent[data$Country=="Malaysia"]<-"Asia"
data$Continent[data$Country=="Mexico"]<-"North America"
data$Continent[data$Country=="Myanmar"]<-"Asia"
data$Continent[data$Country=="Nepal"]<-"Asia"
data$Continent[data$Country=="Netherlands"]<-"Europe"
data$Continent[data$Country=="Nigeria"]<-"Africa"
data$Continent[data$Country=="Pakistan"]<-"Asia"
data$Continent[data$Country=="Philippines"]<-"Asia"
data$Continent[data$Country=="Poland"]<-"Europe"
data$Continent[data$Country=="Sarawak"]<-"Asia"
data$Continent[data$Country=="Singapore"]<-"Asia"
data$Continent[data$Country=="South Korea"]<-"Asia"
data$Continent[data$Country=="Sweden"]<-"Europe"
data$Continent[data$Country=="Taiwan"]<-"Asia"
data$Continent[data$Country=="Thailand"]<-"Asia"
data$Continent[data$Country=="UK"]<-"Europe"
data$Continent[data$Country=="United States"]<-"North America"
data$Continent[data$Country=="USA"]<-"North America"
data$Continent[data$Country=="Vietnam"]<-"Asia"

data$Continent <- as.factor(data$Continent)
table(data$Continent)
#Africa level 3개의 데이터를 가짐.
#South America도 11개 정도 밖에 데이터가 얼마 없음.

data[which(data$Continent=='Africa'|data$Continent=='South America'),]
#데이터의 갯수도 더 작고 수상한 희귀한 데이터도 아니므로 제거하기로 결정.
data <- data[-which(data$Continent=='Africa'|data$Continent=='South America'),]
table(data$Continent)
#제거 확인.

#대륙별 평점의 차이를 보기위한 상자그림
data %>%
  ggplot(aes(x=Continent,y=Stars,fill=Continent))+geom_boxplot()
#아시아가 가장 평점이 높다고 볼 수 있다.






##Variety
word <- data$Variety
word <- as.character(word)
#text mining 
#install.packages('tm')
#install.packages('e1071')
#install.packages('caret') # for a random stratified split
#install.packages('quanteda')
#install.packages('irlba')
#install.packages('stringr')
#install.packages('KoNLP')
library(KoNLP)#한국어 텍스트마이닝.
library(stringr)
library(e1071)
library(caret)
library(quanteda)
library(irlba)
library(randomForest)
library(tm)
library(wordcloud)




# Tokenize SMS text messages.
words <- tokens(word, what = 'word',
                        remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE, remove_hyphens = TRUE)
#구두점, 숫자, 심볼, 하이폰 제거 후 토큰화.

words <- tokens_tolower(words)#소문자로 변환.


#쓸모없는 단어들 지우기
words <- tokens_select(words,stopwords('en'),selection = 'remove',padding = F)
words <- tokens_select(words,stopwords('po'),selection = 'remove',padding = F)
words <- tokens_select(words,stopwords('SMART'),selection = 'remove',padding = F)
words <- tokens_select(words,c('flavor','flavour','noodles','noodle','instant','ramen','bowl','style','udon',
                               'yum','with','cup','mi','demae','penang','soup','souce')
                       ,selection = 'remove',padding = F)

#단어를 추출한 후 데이터프레임으로 변환
#변수명 변환 후 각 단어 빈도표 생

wordcount <- table(unlist(words))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

#가장 많이 나온 단어 20개 선정.
top_20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top_20

#베르미첼리(Vermicelli)는 전통적인 형태의 이탈리아 파스타의 일종으로 스파게티보다 얇다.


pal <- brewer.pal(8,"Dark2")  # Dark2 색상 목록에서 8개 색상 추출

wordcloud(words = df_word$word,  # 단어
          freq = df_word$freq,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .1,          # 회전 단어 비율
          scale = c(4, 0.3),     # 단어 크기 범위
          colors = pal)          # 색깔 목록

#전체 데이터에서의 워드클라우드와 수상한 팀의 특징 워드클라우드.
word_2 <- data_award$Variety
word_2 <- as.character(word_2)

# Tokenize SMS text messages.
words_2 <- tokens(word_2, what = 'word',
                remove_numbers = TRUE, remove_punct = TRUE,
                remove_symbols = TRUE, remove_hyphens = TRUE)
#구두점, 숫자, 심볼, 하이폰 제거 후 토큰화.

words_2 <- tokens_tolower(words_2)#소문자로 변환.


#쓸모없는 단어들 지우기
words_2 <- tokens_select(words_2,stopwords('en'),selection = 'remove',padding = F)
words_2 <- tokens_select(words_2,stopwords('po'),selection = 'remove',padding = F)
words_2 <- tokens_select(words_2,stopwords('SMART'),selection = 'remove',padding = F)
words_2 <- tokens_select(words_2,c('flavor','flavour','noodles','noodle','instant','ramen','bowl','style','udon',
                               'yum','with','cup','mi','demae','penang','soup','souce','la')
                       ,selection = 'remove',padding = F)

#단어를 추출한 후 데이터프레임으로 변환
#변수명 변환 후 각 단어 빈도표 생

wordcount_2 <- table(unlist(words_2))
df_word_2 <- as.data.frame(wordcount_2, stringsAsFactors = F)
df_word_2 <- rename(df_word_2,
                  word = Var1,
                  freq = Freq)

#가장 많이 나온 단어 20개 선정.
top_20 <- df_word_2 %>%
  arrange(desc(freq)) %>%
  head(20)
top_20
# mian : 밀로 만든 중국면
# laksa : 동남 아시아의 페라 나칸 (Peranakan) 요리 로 유명한 매운 누들 스프 


pal_2 <- brewer.pal(8,"Dark2")[5:9]  # Dark2 색상 목록에서 8개 색상 추출

wordcloud(words = df_word_2$word,  # 단어
          freq = df_word_2$freq,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .1,          # 회전 단어 비율
          scale = c(4, 0.01),     # 단어 크기 범위
          colors = pal)          # 색깔 목록

#전체로 만든 워드클라우드와 수상한 데이터의 워드클라우드에서의 차이가 별로 없다고 볼 수 있다.


######################Analysis######################
set.seed(201711517)
training_idx <- createDataPartition(data$award, times = 1, p = 0.7, list = F)
train_data <- data[training_idx,]
test_data <- data[-training_idx,]

prop.table(table(data$award))
prop.table(table(test_data$award))
prop.table(table(test_data$award))
#award간 비율 동일하게 잘 뽑혔는지 확인.


summary(data)
glimpse(data)
##LM model
#Variety는 독립변수로 넣징않고, Top.Ten데이터는 award로 대체.
#Country데이터와 Contient데이터는 둘 중 하나만 넣어보기로.
lm_model <- lm(Stars~.-Variety-Top.Ten-Continent,data=train_data)
summary(lm_model)
#모델은 적합하다고 나옴,
#Brand 데이터 대부분이 적합하지 않다고 판단. 
#Adj-R^2값이 0.3919로 매우 좋지 않은 모델임을 알 수 있다.

lm_mod_2 <- lm(Stars~.-Variety-Top.Ten-Country,data=train_data)
summary(lm_mod_2)
#country 대신 contient변수를 넣으니 변화 미미.(0.3813으로 더안좋아짐.)



lm_mod_3 <- lm(Stars~.-Variety-Top.Ten-Country-Brand,data=train_data)
summary(lm_mod_3)

##적합도가 매우 떨어져서 도시변수의 빈도수가 많은 도시들만 뽑아서 새 데이터 만들기.
#wordcloud
wordcloud(data$Country,random.order = F,colors = pal[5:9])
#japan, korea, usa, taiwan의 빈도수가 매우 많음.

#
data_2 <- data %>%
  filter(Country=='Japan'|Country=='USA'|Country=='Taiwan'|Country=='South Korea')

training_idx_2 <- createDataPartition(data_2$award, times = 1, p = 0.7, list = F)
train_data_2 <- data_2[training_idx_2,]
test_data_2 <- data_2[-training_idx_2,]

prop.table(table(data_2$award))
prop.table(table(test_data_2$award))
prop.table(table(test_data_2$award))

lm_model <- lm(Stars~.-Variety-Top.Ten-Continent,data=train_data_2)
summary(lm_model)

lm_mod_2 <- lm(Stars~.-Variety-Top.Ten-Continent-Brand,data=train_data_2)
summary(lm_mod_2)

library(MASS)#stepwise
step_mod <- stepAIC(lm_model,direction = 'both')
summary(step_mod)
#0.3553으로 0.5가 채 안되는 좋지않은 모델.


library(car)
library(randomForest)
rf_mod <- randomForest(Stars~.-Variety-Top.Ten-Continent,train_data )
#카테고리가 너무 많아서 에러뜸.
rf_mod <- randomForest(Stars~.-Variety-Top.Ten-Continent-Brand,train_data )
plot(rf_mod)

#변수 중요도 확인.
varImpPlot(rf_mod)

##이분할적 데이터가 아니므로 confusion matrix 못만들고 roc curve또한 못그림.
##데이터의 적합도를 높이기 위해서는 이상치제거, 주성분분석 등을 사용해야함.

#이상치 제거하기
data %>%
  ggplot(aes(y=Stars))+geom_boxplot()

#상자그림 통계량.
boxplot(data$Stars)$stats

data_3 <- data %>%
  filter(Stars>=1.75)

training_idx_3 <- createDataPartition(data_3$award, times = 1, p = 0.7, list = F)
train_data_3 <- data_3[training_idx_3,]
test_data_3 <- data_3[-training_idx_3,]

prop.table(table(data_3$award))
prop.table(table(test_data_3$award))
prop.table(table(test_data_3$award))

lm_model <- lm(Stars~.-Variety-Top.Ten-Continent,data=train_data_3)
summary(lm_model)

step_mod <- stepAIC(lm_model,direction = 'both')
summary(step_mod)