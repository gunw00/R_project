# 장석건 
training %>% ggplot(aes(workclass, fill=wage)) + geom_bar()
training %>% ggplot(aes(capital_gain-capital_loss, fill=wage)) +geom_density(alpha =1) + xlim(-5000, 5000) 
training %>% ggplot(aes(hours_per_week,fill=wage)) + geom_density(alpha=.5)

# 차현우
training%>%ggplot(aes(hours_per_week,fill=wage))+geom_bar()
training%>%ggplot(aes(occupation,fill=wage))+geom_bar()+coord_flip()
training%>%ggplot(aes(marital_status,fill=wage))+geom_bar()+coord_flip()
training%>%ggplot(aes(race,fill=wage))+geom_bar()

# 배경득
training %>% ggplot(aes(sex,fill=wage))+geom_bar()
training %>% ggplot(aes(race,fill=sex))+geom_bar()
training %>% ggplot(aes(age,fill=sex))+geom_bar()

training %>%
  group_by(wage, sex) %>%
  tally() %>%
  mutate(tol_n = n/sum(n)) %>%
  ggplot(aes(wage, tol_n, fill=sex))+geom_histogram(stat='identity')+ylim(0,1)

# 김성원
training %>% ggplot(aes(sex, fill=wage)) + geom_bar()
training %>% ggplot(aes(age, fill=wage)) + geom_density(alpha=.6)
training %>% ggplot(aes(hours_per_week, fill=sex)) + geom_density(alpha=.6)
training %>% ggplot(aes(hours_per_week, fill=wage)) + geom_density(alpha=.6)
training %>% ggplot(aes(workclass, fill=wage)) + geom_bar()