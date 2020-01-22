#성별에 따른 임금차이 히스토그램
adult %>%
  group_by(sex,wage)%>%
  ggplot(aes(x=sex,fill=wage))+
  geom_histogram(stat='count',position='dodge')

#인종에 따른 임금차이 히스토그램
adult %>%
  group_by(race,wage)%>%
  ggplot(aes(x=race,fill=wage))+
  geom_histogram(stat='count',position='dodge')

#인종과 성별에 따른 중산층의 비율 히스토그램
adult %>%
  mutate(mid=ifelse(wage=='>50K',1,0))%>%
  group_by(sex,race,mid)%>%
  tally()%>%
  group_by(sex,race)%>%
  mutate(prob_mid=n/sum(n))%>%
  filter(mid==1)%>%
  ggplot(aes(x=race,y=prob_mid,colour=sex,group=sex))+
  geom_point()+geom_line()


############iris#####################
data <- tbl_df(iris)
class(data)
glimpse(data)

data %>%
  head(10)
library(gapminder)

adult %>% sample_frac(0.01)

