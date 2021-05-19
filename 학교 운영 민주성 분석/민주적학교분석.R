setwd('C:\\Users\\NT500R\\Desktop\\선생님 분석')

library(readxl)
data <- read_excel(path = 'data.xlsx', sheet = 'Sheet1', col_names = TRUE)
str(data)

colnames(data) <- c('Q11','Q12','Q13','Q14','Q15','Q16','Q21','Q22','Q23','Q24','Q25','Q26','Q27','지역','직급','성별','경력','학교형태','규모')
str(data)

data$지역 <- as.factor(data$지역)
data$직급 <- as.factor(data$직급)
data$성별 <- as.factor(data$성별)
data$경력 <- as.factor(data$경력)
data$학교형태 <- as.factor(data$학교형태)
data$규모 <- as.factor(data$규모)
str(data)

library(ggplot2)
library(dplyr)

na_data <- union(which(is.na(data$Q11)), #설문지 내용 중 NA 인 데이터 처리를 위한 작업.
c(which(is.na(data$Q12)),
which(is.na(data$Q13)),
which(is.na(data$Q14)),
which(is.na(data$Q15)),
which(is.na(data$Q16)),
which(is.na(data$Q21)),
which(is.na(data$Q22)),
which(is.na(data$Q23)),
which(is.na(data$Q24)),
which(is.na(data$Q25)),
which(is.na(data$Q26)),
which(is.na(data$Q27))))
na_data #12개의 NA발견

data1 <- data[-c(na_data),] #NA데이터를 뺀 데이터셋 만듦.

str(data1) #12개의 NA데이터 빠진것으로 유추.

data1$Q1 <- data1$Q11+data1$Q12+data1$Q13+data1$Q14+data1$Q15+data1$Q16 #1번 문항 합계
data1$Q2 <- data1$Q21+data1$Q22+data1$Q23+data1$Q24+data1$Q25+data1$Q26+data1$Q27 #2번 문항 합계
data1$Q3 <- data1$Q1 + data1$Q2 #모든문항 합계
str(data1)

sum(is.na(data1$지역))
which(is.na(data1$직급)) #1개 -> 교사 위주로 점수를 볼 것 이므로 직급을 적지않은 데이터는 삭제.
which(is.na(data1$경력)) #2개 -> 경력에 따른 점수는 의미 x
which(is.na(data1$학교형태)) #5개 -> 학교형태에 따른 점수를 보는것이 관건 -> 학교형태를 모르는 데이터는 삭제하기로 결정

na_re <- union(which(is.na(data1$직급)), c(which(is.na(data1$학교형태))))
data2 <- data1[-c(na_re),]
str(data2)


###########분석#############
data2 %>%
  ggplot(aes(x=`성별`,fill=`성별`))+geom_bar()+
  geom_text(aes( label = ..count..), stat= "count",position=position_stack(.5))


data2 %>%
  ggplot(aes(x=`지역`,fill=`지역`))+geom_bar()+
  geom_text(aes( label = ..count..), stat= "count",position=position_stack(.5))

data3 <- data2[,c('지역','Q1','Q2','Q3')]

data_w <- data3[which(data3[,'지역']=='서부권'),c(2,3,4)]
data_e <- data3[-which(data3[,'지역']=='서부권'),c(2,3,4)]

m_w <- apply(data_w,2,mean)
m_e <- apply(data_e,2,mean)

m <-  as.data.frame(rbind(m_w,m_e))
rownames(m) <- c('서부권','동부권')
m$locate <- c('서부권','동부권')

m %>%
  ggplot(aes(x=locate,y=Q1,fill=locate))+geom_bar(stat='identity')+
  geom_text(aes( label = Q1),position=position_stack(.7))+ggtitle('Q1에 대한 지역별 점수 평균')
  

m %>%
  ggplot(aes(x=locate,y=Q2,fill=locate))+geom_bar(stat='identity')+
  geom_text(aes( label = Q2),position=position_stack(.7))+ggtitle('Q2에 대한 지역별 점수 평균')

m %>%
  ggplot(aes(x=locate,y=Q3,fill=locate))+geom_bar(stat='identity')+
  geom_text(aes( label = Q3),position=position_stack(.7))+ggtitle('전체에 대한 지역별 점수 평균')


data4 <- data2[,c(14:22)]


school <- data4 %>%
  group_by(학교형태) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3))

school %>%
  ggplot(aes(x=학교형태,y=mQ1,fill=학교형태))+geom_bar(stat='identity')+
  geom_text(aes( label = mQ1),position=position_stack(.7))+ggtitle('Q1에 대한 학교형태별 점수 평균')

school %>%
  ggplot(aes(x=학교형태,y=mQ2,fill=학교형태))+geom_bar(stat='identity')+
  geom_text(aes( label = mQ2),position=position_stack(.7))+ggtitle('Q2에 대한 학교형태별 점수 평균')

school %>%
  ggplot(aes(x=학교형태,y=mQ3,fill=학교형태))+geom_bar(stat='identity')+
  geom_text(aes( label = mQ3),position=position_stack(.7))+ggtitle('전체에 대한 학교형태별 점수 평균')




X <- data4 %>%
  group_by(학교형태,규모) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3))

X %>%
  ggplot(aes(x=학교형태,y=mQ1))+geom_bar(aes(fill=규모),stat='identity',position=position_dodge())+ggtitle('Q1에 대한 학교형태별 점수 평균')


X %>%
  ggplot(aes(x=학교형태,y=mQ2))+geom_bar(aes(fill=규모),stat='identity',position=position_dodge())+ggtitle('Q2에 대한 학교형태별 점수 평균')



X %>%
  ggplot(aes(x=학교형태,y=mQ3))+geom_bar(aes(fill=규모),stat='identity',position=position_dodge())+ggtitle('전체에 대한 학교형태별 점수 평균')



A <- data4 %>%
  group_by(직급,성별) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3))

A %>%
  ggplot(aes(x=직급,y=mQ3,fill=성별))+
  geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.7))+
  ggtitle('직급과 성별에 따른 전체평균점수')

teach <- data4[which(data4$직급=='교사'),]

teach %>%
  group_by(학교형태,규모) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=학교형태,y=mQ3,fill=규모))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('교사응답 중 학교형태와 규모에 따른 전체평균점수')


teach %>%
  group_by(학교형태) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=학교형태,y=mQ3,fill=학교형태))+geom_bar(stat='identity')+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('교사응답 중 학교형태에 따른 전체평균점수')

teach %>%
  group_by(규모) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=규모,y=mQ3,fill=규모))+geom_bar(stat='identity')+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('교사응답 중 규모에 따른 전체평균점수')


teach %>%
  group_by(경력,성별) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=경력,y=mQ3,fill=성별))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('교사응답 중 경력과 성별에 따른 전체평균점수')


teach %>%
  group_by(경력,학교형태) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=경력,y=mQ3,fill=학교형태))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('교사응답 중 경력과 학교형태에 따른 전체평균점수')




data4 %>%
  group_by(경력) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=경력,y=mQ3,fill=경력))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('교사응답 중 경력에 따른 전체평균점수')



data4 %>%
  group_by(경력) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=경력,y=mQ1,fill=경력))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ1,1)),position=position_dodge(.9))+
  ggtitle('교사응답 중 경력에 따른 Q1평균점수')



data4 %>%
  group_by(경력) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=경력,y=mQ2,fill=경력))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ2,1)),position=position_dodge(.9))+
  ggtitle('교사응답 중 경력에 따른 Q2평균점수')



##################################################################################
data4 %>%
  group_by(지역) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=지역,y=mQ1,fill=지역))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ1,1)),position=position_dodge(.9))+
  ggtitle('지역에 따른 민주적 학교문화 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='지역', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))



data4 %>%
  group_by(지역) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=지역,y=mQ2,fill=지역))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ2,1)),position=position_dodge(.9))+
  ggtitle('지역에 따른 민주적 학교구조 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='지역', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4 %>%
  group_by(지역) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=지역,y=mQ3,fill=지역))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('지역에 따른 전체평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='지역', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4 %>%
  group_by(직급) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=직급,y=mQ1,fill=직급))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ1,1)),position=position_dodge(.9))+
  ggtitle('직급에 따른 민주적 학교구조 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='직급', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))


data4 %>%
  group_by(직급) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=직급,y=mQ2,fill=직급))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ2,1)),position=position_dodge(.9))+
  ggtitle('직급에 따른 Q2평균점수')


data4 %>%
  group_by(직급) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=직급,y=mQ3,fill=직급))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('직급에 따른 전체평균점수')


data4 %>%
  group_by(성별) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=성별,y=mQ1,fill=성별))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ1,1)),position=position_dodge(.9))+
  ggtitle('성별에 따른 민주적 학교문화 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='성별', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4 %>%
  group_by(성별) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=성별,y=mQ2,fill=성별))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ2,1)),position=position_dodge(.9))+
  ggtitle('성별에 따른 민주적 학교문화구조 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='성별', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4 %>%
  group_by(성별) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=성별,y=mQ3,fill=성별))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('성별에 따른 전체평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='성별', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4 %>%
  group_by(경력) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=경력,y=mQ3,fill=경력))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('경력에 따른 전체평균점수')

length(which(is.na(data4$경력)))
data4_1 <- data4[-which(is.na(data4$경력)),]




data4_1 %>%
  group_by(경력) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=경력,y=mQ1,fill=경력))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ1,1)),position=position_dodge(.9))+
  ggtitle('경력에 따른 민주적 학교문화 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='경력', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4_1 %>%
  group_by(경력) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=경력,y=mQ2,fill=경력))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ2,1)),position=position_dodge(.9))+
  ggtitle('경력에 따른 민주적 학교구조 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='경력', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4_1 %>%
  group_by(경력) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=경력,y=mQ3,fill=경력))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('경력에 따른 전체평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='경력', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))


data4 %>%
  group_by(성별) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=성별,y=mQ1,fill=성별))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ1,1)),position=position_dodge(.9))+
  ggtitle('성별에 따른 Q1평균점수')

data4 %>%
  group_by(성별) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=성별,y=mQ2,fill=성별))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ2,1)),position=position_dodge(.9))+
  ggtitle('성별에 따른 Q2평균점수')

data4 %>%
  group_by(성별) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=성별,y=mQ3,fill=성별))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('성별에 따른 전체 평균점수')

data4 %>%
  group_by(학교형태) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=학교형태,y=mQ1,fill=학교형태))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ1,1)),position=position_dodge(.9))+
  ggtitle('학교형태에 따른 민주적 학교문화 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='학교형태', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4 %>%
  group_by(학교형태) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=학교형태,y=mQ2,fill=학교형태))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ2,1)),position=position_dodge(.9))+
  ggtitle('학교형태에 따른 민주적 학교구조 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='학교형태', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))


data4 %>%
  group_by(학교형태) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=학교형태,y=mQ3,fill=학교형태))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('학교형태에 따른 전체평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='학교형태', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))




length(which(is.na(data4$규모)))
data4_2 <- data4[-which(is.na(data4$규모)),]

data4_2 %>%
  group_by(규모) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=규모,y=mQ1,fill=규모))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ1,1)),position=position_dodge(.9))+
  ggtitle('규모에 따른 민주적 학교문화 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='규모', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4_2 %>%
  group_by(규모) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=규모,y=mQ2,fill=규모))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ2,1)),position=position_dodge(.9))+
  ggtitle('규모에 따른 민주적 학교구조 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='규모', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))


data4_2 %>%
  group_by(규모) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=규모,y=mQ3,fill=규모))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('규모에 따른  평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='규모', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))


############test###############
t <- data4 %>%
  group_by(지역) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3))

t.test(Q3~지역,data=data4)
t.test(Q3~성별,data=data4)


summary(aov(Q3~경력,data=data4_1))

summary(aov(Q3~규모,data=data4_2))

install.packages("DescTools")
library(DescTools)
PostHocTest( aov(Q3~규모,data=data4_2), method="lsd", conf.level="0.95")





data5 <- data4

data5$직급 <- ifelse(data5$직급=='보직(부장)교사','교사',
                ifelse(data5$직급=='교감','관리직',
                  ifelse(data5$직급=='교장','관리직','행정직원')))

data5$직급 <- as.factor(data5$직급)

summary(data5$직급)


data5 %>%
  group_by(직급) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=직급,y=mQ1,fill=직급))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ1,1)),position=position_dodge(.9))+
  ggtitle('직급에 따른 민주적 학교문화 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='직급', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data5 %>%
  group_by(직급) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=직급,y=mQ2,fill=직급))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ2,1)),position=position_dodge(.9))+
  ggtitle('직급에 따른 민주적 학교구조 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='직급', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data5 %>%
  group_by(직급) %>%
  summarise(mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3)) %>%
  ggplot(aes(x=직급,y=mQ3,fill=직급))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('직급에 따른 전체평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='직급', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

summary(aov(Q3~직급,data=data5))

summary(aov(Q1~학교형태,data=data5))

summary(aov(Q2~학교형태,data=data5))

##########################################################

A <- data2 %>%
  group_by(학교형태) %>%
  summarise(mQ11 = mean(Q11), mQ12 = mean(Q12), mQ13 = mean(Q13), mQ14 = mean(Q14), mQ15 = mean(Q15), mQ16 = mean(Q16),
            mQ21 = mean(Q21), mQ22 = mean(Q22), mQ23 = mean(Q23), mQ24 = mean(Q24), mQ25 = mean(Q25), mQ26 = mean(Q26), mQ27 = mean(Q27),
            mQ1 = mean(Q1), mQ2 = mean(Q2), mQ3 = mean(Q3))


A %>% ggplot(aes(x=학교형태,y=mQ1,fill=학교형태))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ1,1)),position=position_dodge(.9))+
  ggtitle('학교형태에 따른 Q1평균점수')
  
A %>% ggplot(aes(x=학교형태,y=mQ2,fill=학교형태))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ2,1)),position=position_dodge(.9))+
  ggtitle('학교형태에 따른 Q2평균점수')

A %>% ggplot(aes(x=학교형태,y=mQ3,fill=학교형태))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ3,1)),position=position_dodge(.9))+
  ggtitle('학교형태에 따른 전체 평균점수')

summary(aov(Q3~학교형태,data=data4))

A %>% ggplot(aes(x=학교형태,y=mQ11,fill=학교형태))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ11,1)),position=position_dodge(.9))+
  ggtitle('학교형태에 따른 Q1.1평균점수')

A %>% ggplot(aes(x=학교형태,y=mQ27,fill=학교형태))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ27,1)),position=position_dodge(.9))+
  ggtitle('학교형태에 따른 \n교직원의 민주적인 업무 처리 만족도 평균점수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='학교형태', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

A %>% ggplot(aes(x=학교형태,y=mQ27,fill=학교형태))+geom_bar(stat='identity',position = position_dodge())+
  geom_text(aes( label = round(mQ27,1)),position=position_dodge(.9))+
  ggtitle('학교형태에 따른 Q2.7평균점수')

######################################################################################################################
data #601개 데이터

data1 #원래 자료에서 Q1, Q2문항에 답을 하지않은 자료 12개를 제거함. 601개 -> 589개 자료

data2 #학교형태란에 답을 안한 자료 6개를 제거. 589개 -> 583개

data4 %>%
  group_by(성별) %>%
  tally() %>%
  ggplot(aes(x=성별, y=n, fill=성별))+geom_bar(stat='identity')+
  geom_text(aes(label=n))+
  ggtitle('성별 범주 별 인원 수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='성별', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))
  
data4 %>%
  group_by(지역) %>%
  tally() %>%
  ggplot(aes(x=지역, y=n, fill=지역))+geom_bar(stat='identity')+
  geom_text(aes(label=n))+
  ggtitle('지역 범주 별 인원 수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='지역', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4 %>%
  group_by(직급) %>%
  tally() %>%
  ggplot(aes(x=직급, y=n, fill=직급))+geom_bar(stat='identity')+
  geom_text(aes(label=n))+
  ggtitle('직급 범주 별 인원 수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='직급', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))

data4 %>%
  group_by(경력) %>%
  tally() %>%
  ggplot(aes(x=경력, y=n, fill=경력))+geom_bar(stat='identity')+
  geom_text(aes(label=n))+
  ggtitle('경력 범주 별 인원 수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='경력', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))


data4 %>%
  group_by(규모) %>%
  tally() %>%
  ggplot(aes(x=규모, y=n, fill=규모))+geom_bar(stat='identity')+
  geom_text(aes(label=n))+
  ggtitle('규모 범주 별 인원 수')+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))+
  labs(x='규모', y='')+
  theme(axis.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))









