########### 장석건 #############

## 1.1
adult %>% ggplot(aes(age)) + geom_density()
adult %>% ggplot(aes(workclass, fill=workclass)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(fnlwgt)) + geom_density() + xlim(10000, 70000)
adult %>% ggplot(aes(education, fill= education)) + geom_bar() + coord_flip ()
adult %>% ggplot(aes(education_num <- as.factor(education_num), fill = education_num)) + geom_bar() + xlab("education_num")
#adult %>% ggplot(aes(education_num , fill= education_num )) + geom_bar() 
adult %>% ggplot(aes(marital_status, fill= marital_status)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(occupation , fill= occupation )) + geom_bar() + coord_flip()
adult %>% ggplot(aes(relationship , fill= relationship )) + geom_bar()
adult %>% ggplot(aes(sex , fill= sex )) + geom_bar() 
adult %>% ggplot(aes(capital_gain)) + geom_histogram()
adult %>% ggplot(aes(capital_loss)) + geom_histogram()
adult %>% ggplot(aes(hours_per_week)) + geom_density()
adult %>% ggplot(aes(native_country)) + geom_bar() + coord_flip() 
adult %>% ggplot(aes(wage)) + geom_bar()

## 1.2
adult %>% ggplot(aes(fnlwgt, fill=workclass)) + geom_histogram() + xlim(10000, 70000)
adult %>% mutate(edu_level = ifelse(education_num >=7, "high_level", "low_level")) %>% ggplot(aes(age, fill=wage)) + 
  geom_density(alpha = .5) + facet_grid(edu_level ~ sex, scales = 'free_y')
adult %>% mutate(country = ifelse(native_country =="United-States", "U.S.", "Others"), race=ifelse(race=="White", "White","Ohters")) %>%
  ggplot(aes(country, fill=wage)) + geom_bar() + facet_grid(sex ~ race, scales = 'free_y')


table(adult$workclass)
adult %>% filter(workclass !="?", workclass !="Never-worked", workclass !="Without-pay") %>% ggplot(aes(workclass, fill= wage)) + geom_bar()
# adult %>% filter(workclass != c("?", "Never-worked", "Without-pay")) %>% ggplot(aes(workclass, fill= wage)) + geom_bar()
adult %>% ggplot(aes(education, fill= wage)) + geom_bar() + coord_flip ()
summary(adult$hours_per_week)
table(adult$native_country)
adult %>% filter(native_country %in% c('Vietnam', 'Germany')) %>% ggplot(aes(hours_per_week, fill=wage)) +
  geom_density(alpha =.5) + facet_wrap(~ native_country)
adult %>% filter(race %in% c('Black', 'White')) %>% ggplot(aes(hours_per_week, fill=wage)) +
  geom_density(alpha =.5) + facet_wrap(~ race) + xlim(0, 70)

adult %>% mutate(workclass = workclass - "?") %>% ggplot(aes(workclass)) + geom_bar()

## 1.3
pairs(adult)

plot(adult2$education, adult2$education_num)
adult2 <- adult
adult2 <- adult2 %>% mutate(education=reorder(education, education_num)) 
levels(adult2$education)




########### 차현우 ############

## 1.1
adult%>%ggplot(aes(age))+geom_bar() #age 변수


adult%>%ggplot(aes(workclass))+geom_bar()
adult%>%ggplot(aes(fnlwgt))+geom_bar() 

adult%>%ggplot(aes(fnlwgt),xlim=c(0,500000))+geom_bar()


adult$education<-as.factor(adult$education)
adult%>%ggplot(aes(education))+geom_bar()+coord_flip()


table(adult$education)
round(prop.table(table(adult$education))*100,1)
pie(table(adult$education))
round(prop.table(table(adult$education))*100,1)


adult%>%ggplot(aes(education_num))+geom_bar()

adult$marital_status<-as.factor(adult$marital_status)
adult%>%ggplot(aes(marital_status))+geom_bar()+coord_flip()


adult%>%ggplot(aes(occupation))+geom_bar()+coord_flip()
adult%>%ggplot(aes(relationship))+geom_bar()+coord_flip()
adult%>%ggplot(aes(race))+geom_bar()
adult%>%ggplot(aes(sex))+geom_bar()+coord_flip()
adult%>%ggplot(aes(native_country))+geom_bar()+coord_flip()
adult%>%ggplot(aes(native_country)+xlim(c(0,50))+geom_bar()+coord_flip())
adult%>%ggplot(aes(wage))+geom_bar()


## 1.2
adult%>%ggplot(aes(age,fill=wage))+geom_density(alpha=.5)
adult%>%ggplot(aes(workclass,wage))+geom_jitter(alpha=.1,col='blue')
adult%>%ggplot(aes(fnlwgt,fill=wage))+geom_density(alpha=.5)
adult%>%ggplot(aes(education,wage))+geom_jitter()
adult%>%ggplot(aes(education,fill=wage))+geom_density()+coord_flip()
adult%>%ggplot(aes(occupation,wage))+geom_jitter()+coord_flip()
adult%>%ggplot(aes(relationship,wage))+geom_jitter()
adult%>%ggplot(aes(race,wage))+geom_jitter()
adult%>%ggplot(aes(sex,wage))+geom_jitter()
adult%>%ggplot(aes(capital_gain,fill=wage))+geom_density()
adult%>%ggplot(aes(capital_loss,fill=wage))+geom_density()
adult%>%ggplot(aes(hours_per_week,fill=wage))+geom_density()
adult%>%ggplot(aes(native_country,wage))+geom_jitter()+coord_flip()


## 1.3
adult%>% filter(native_country%in% c('Japan','India')) %>% ggplot(aes(age,fill=wage)) + geom_density(alpha=0.5)+
  ylim(0,0.1)+ facet_grid(native_country~sex,scales = 'free_y')


table(adult$native_country)
Asia<- adult%>%filter(native_country=='India'|native_country=='Laos'|native_country=='China')
Europe<-adult%>%filter(native_country=='England'|native_country=='France'|native_country=='Hungary')
America<-adult%>%filter(native_country=='Canada'|native_country=='Mexico'|native_country=='Cuba')

Asia %>% ggplot(aes(age,fill=wage)) + geom_density(alpha=0.5)+
  ylim(0,0.1)+ facet_grid(native_country~sex,scales = 'free_y')

Europe %>% ggplot(aes(age,fill=wage)) + geom_density(alpha=0.5)+
  ylim(0,0.1)+ facet_grid(native_country~sex,scales = 'free_y')

America %>% ggplot(aes(age,fill=wage)) + geom_density(alpha=0.5)+
  ylim(0,0.1)+ facet_grid(native_country~sex,scales = 'free_y')

Asia$native_country<-NULL
Europe$native_country<-NULL
America$native_country<-NULL

Asia<-Asia%>%mutate(country='asia')
Europe<-Europe%>%mutate(country='europe')
America<-America%>%mutate(country='america')

World1<-rbind(Asia,Europe,America)
World<-World1
World%>%ggplot(aes(age,fill=wage))+geom_density(alpha=0.5)+
  ylim(0,0.1)+facet_grid(country~sex,scales = 'free_y')

adult<-adult%>%mutate(income=ifelse(capital_gain-capital_loss>=0,'surplus','deficit'))
table(adult$income)
adult3<-sample_n(adult,1000)
adult3%>%ggplot(aes(sex,income))+geom_jitter()
adult3%>%ggplot(aes(income,fill=wage))+geom_bar()

adult3$education<-NULL
adult3$capital_gain<-NULL
adult3$capital_loss<-NULL
glimpse(adult3)



########### 배경득 #############
##1.2

#범주 범주
adult %>% ggplot(aes(workclass, fill=education)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=marital_status)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=occupationalStatus)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=relationship)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=race)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=sex)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=hours_per_week)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=native_country)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=wage)) + geom_bar()

adult %>% ggplot(aes(education, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(education, fill=marital_status)) + geom_bar()
adult %>% ggplot(aes(education, fill=occupation)) + geom_bar()
adult %>% ggplot(aes(education, fill=relationship)) + geom_bar()
adult %>% ggplot(aes(education, fill=race)) + geom_bar()
adult %>% ggplot(aes(education, fill=sex)) + geom_bar()
adult %>% ggplot(aes(education, fill=hours_per_week)) + geom_bar()
adult %>% ggplot(aes(education, fill=native_country)) + geom_bar()
adult %>% ggplot(aes(education, fill=wage)) + geom_bar()

adult %>% ggplot(aes(marital_status, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=education)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=occupation)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=relationship)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=race)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=sex)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=hours_per_week)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=native_country)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=wage)) + geom_bar()

adult %>% ggplot(aes(occupation, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(occupation, fill=education)) + geom_bar()
adult %>% ggplot(aes(occupation, fill=marital_status)) + geom_bar()
adult %>% ggplot(aes(occupation, fill=relationship)) + geom_bar()
adult %>% ggplot(aes(occupation, fill=race)) + geom_bar()
adult %>% ggplot(aes(occupation, fill=sex)) + geom_bar()
adult %>% ggplot(aes(occupation, fill=hours_per_week)) + geom_bar()
adult %>% ggplot(aes(occupation, fill=native_country)) + geom_bar()
adult %>% ggplot(aes(occupation, fill=wage)) + geom_bar()

adult %>% ggplot(aes(relationship, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(relationship, fill=education)) + geom_bar()
adult %>% ggplot(aes(relationship, fill=marital_status)) + geom_bar()
adult %>% ggplot(aes(relationship, fill=occupation)) + geom_bar()
adult %>% ggplot(aes(relationship, fill=race)) + geom_bar()
adult %>% ggplot(aes(relationship, fill=sex)) + geom_bar()
adult %>% ggplot(aes(relationship, fill=hours_per_week)) + geom_bar()
adult %>% ggplot(aes(relationship, fill=native_country)) + geom_bar()
adult %>% ggplot(aes(relationship, fill=wage)) + geom_bar()

adult %>% ggplot(aes(race, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(race, fill=education)) + geom_bar()
adult %>% ggplot(aes(race, fill=marital_status)) + geom_bar()
adult %>% ggplot(aes(race, fill=occupation)) + geom_bar()
adult %>% ggplot(aes(race, fill=relationship)) + geom_bar()
adult %>% ggplot(aes(race, fill=sex)) + geom_bar()
adult %>% ggplot(aes(race, fill=hours_per_week)) + geom_bar()
adult %>% ggplot(aes(race, fill=native_country)) + geom_bar()
adult %>% ggplot(aes(race, fill=wage)) + geom_bar()

adult %>% ggplot(aes(sex, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(sex, fill=education)) + geom_bar()
adult %>% ggplot(aes(sex, fill=marital_status)) + geom_bar()
adult %>% ggplot(aes(sex, fill=occupation)) + geom_bar()
adult %>% ggplot(aes(sex, fill=relationship)) + geom_bar()
adult %>% ggplot(aes(sex, fill=race)) + geom_bar()
adult %>% ggplot(aes(sex, fill=hours_per_week)) + geom_bar()
adult %>% ggplot(aes(sex, fill=native_country)) + geom_bar()
adult %>% ggplot(aes(sex, fill=wage)) + geom_bar()

adult %>% ggplot(aes(hours_per_week, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(hours_per_week, fill=education)) + geom_bar()
adult %>% ggplot(aes(hours_per_week, fill=marital_status)) + geom_bar()
adult %>% ggplot(aes(hours_per_week, fill=occupation)) + geom_bar()
adult %>% ggplot(aes(hours_per_week, fill=relationship)) + geom_bar()
adult %>% ggplot(aes(hours_per_week, fill=race)) + geom_bar()
adult %>% ggplot(aes(hours_per_week, fill=sex)) + geom_bar()
adult %>% ggplot(aes(hours_per_week, fill=native_country)) + geom_bar()
adult %>% ggplot(aes(hours_per_week, fill=wage)) + geom_bar()

adult %>% ggplot(aes(native_country, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(native_country, fill=education)) + geom_bar()
adult %>% ggplot(aes(native_country, fill=marital_status)) + geom_bar()
adult %>% ggplot(aes(native_country, fill=occupation)) + geom_bar()
adult %>% ggplot(aes(native_country, fill=relationship)) + geom_bar()
adult %>% ggplot(aes(native_country, fill=race)) + geom_bar()
adult %>% ggplot(aes(native_country, fill=sex)) + geom_bar()
adult %>% ggplot(aes(native_country, fill=hours_per_week)) + geom_bar()
adult %>% ggplot(aes(native_country, fill=wage)) + geom_bar()

adult %>% ggplot(aes(wage, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(wage, fill=education)) + geom_bar()
adult %>% ggplot(aes(wage, fill=marital_status)) + geom_bar()
adult %>% ggplot(aes(wage, fill=occupation)) + geom_bar()
adult %>% ggplot(aes(wage, fill=relationship)) + geom_bar()
adult %>% ggplot(aes(wage, fill=race)) + geom_bar()
adult %>% ggplot(aes(wage, fill=sex)) + geom_bar()
adult %>% ggplot(aes(wage, fill=hours_per_week)) + geom_bar()
adult %>% ggplot(aes(wage, fill=native_country)) + geom_bar()

#수량 범주
adult %>% ggplot(aes(age, workclass)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(age, education)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(age, marital_status)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(age, occupation)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(age, relationship)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(age, race)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(age, sex)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(age, hours_per_week)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(age, native_country)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(age, wage)) + geom_boxplot(alpha=.5) + coord_flip()

adult %>% ggplot(aes(fnlwgt, workclass)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(fnlwgt, education)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(fnlwgt, marital_status)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(fnlwgt, occupation)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(fnlwgt, relationship)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(fnlwgt, race)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(fnlwgt, sex)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(fnlwgt, hours_per_week)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(fnlwgt, native_country)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(fnlwgt, wage)) + geom_boxplot(alpha=.5) + coord_flip()

adult %>% ggplot(aes(education_num, workclass)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(education_num, education)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(education_num, marital_status)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(education_num, occupation)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(education_num, relationship)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(education_num, race)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(education_num, sex)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(education_num, hours_per_week)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(education_num, native_country)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(education_num, wage)) + geom_boxplot(alpha=.5) + coord_flip()

adult %>% ggplot(aes(capital_gain, workclass)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_gain, education)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_gain, marital_status)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_gain, occupation)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_gain, relationship)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_gain, race)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_gain, sex)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_gain, hours_per_week)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_gain, native_country)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_gain, wage)) + geom_boxplot(alpha=.5) + coord_flip()

adult %>% ggplot(aes(capital_loss, workclass)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_loss, education)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_loss, marital_status)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_loss, occupation)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_loss, relationship)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_loss, race)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_loss, sex)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_loss, hours_per_week)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_loss, native_country)) + geom_boxplot(alpha=.5) + coord_flip()
adult %>% ggplot(aes(capital_loss, wage)) + geom_boxplot(alpha=.5) + coord_flip()

## 1.3
adult$age

adult$age2 <- as.factor(ifelse(adult$age  < 24, "젊음" ,ifelse(adult$age  < 50, "늙음", "틀딱딱")))
adult2 <- dplyr::select(adult, age2, sex)
adult$edu1 <- adult2 %>% filter(sex == 'Male', age2 == '젊음')
adult$kaka <- adult$edu %>% filter(sex == 'Male', age2 == '젊음')
adult$edu %>% filter(sex == 'Male', age2 == '늙음')
adult$edu %>% filter(sex == 'Male', age2 == '틀딱딱')
adult$edu %>% filter(sex == 'Female', age2 == '젊음')
adult$edu %>% filter(sex == 'Female', age2 == '늙음')
adult$edu %>% filter(sex == 'Female', age2 == '틀딱딱')
adult$edu %>% filter(sex == 'Female', age2 == '틀딱딱') %>% levels=(c=("A"))
adult %>% ggplot(aes(sex, fill=age2)) + geom_bar()





############# 김성원 ##############
adult %>% ggplot(aes(x=age)) + geom_freqpoly()
adult %>% ggplot(aes(workclass)) + geom_bar()
adult %>% ggplot(aes(x=fnlwgt)) + geom_freqpoly()
adult %>% ggplot(aes(education)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(x=education_num)) + geom_freqpoly()
adult %>% ggplot(aes(marital_status)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(occupation)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(relationship)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(race)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(sex)) + geom_bar()
adult %>% ggplot(aes(x=capital_gain)) + geom_freqpoly()
adult %>% ggplot(aes(x=capital_loss)) + geom_freqpoly()
adult %>% ggplot(aes(hours_per_week)) + geom_freqpoly()
adult %>% ggplot(aes(native_country)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(wage)) + geom_bar()



adult %>% ggplot(aes(age, fill=wage)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=wage)) + geom_bar()
adult %>% ggplot(aes(wage, fnlwgt)) + geom_boxplot()
adult %>% ggplot(aes(education, fill=wage)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(education_num, fill=wage)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=wage)) + geom_bar()
adult %>% ggplot(aes(occupation, fill= wage)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(relationship, fill=wage)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(race, fill=wage)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(sex, fill=wage)) + geom_bar()
adult %>% ggplot(aes(wage, capital_gain)) + geom_boxplot()
adult %>% ggplot(aes(wage, capital_loss)) + geom_boxplot()
adult %>% ggplot(aes(wage, hours_per_week)) + geom_boxplot()
adult %>% ggplot(aes(native_country, fill=wage)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(wage, fill=native_country)) + geom_bar()
adult %>%
  group_by(wage, sex) %>%
  tally() %>%
  mutate(tol_n = n/sum(n)) %>%
  ggplot(aes(wage, tol_n, fill=sex))+geom_histogram(stat='identity')+ylim(0,1)

adult %>% ggplot(aes((capital_gain-capital_loss), fill=wage)) + geom_density(alpha=1) + 
  xlim(-3000,3000)

adult %>% filter(native_country %in% c('Taiwan', 'Vietnam', 'Thailand', 'South', 'Japan')) %>%
  ggplot(aes(age, fill=wage)) + geom_bar() + facet_grid(native_country ~ sex)





adult %>% ggplot(aes(age, sex)) + geom_jitter()

pairs(~ age + fnlwgt + education_num + (capital_gain-capital_loss) + 
        hours_per_week, data=adult)
pairs(~ age + fnlwgt + education_num + capital_gain + capital_loss +
        hours_per_week, data=adult)

adult %>% ggplot(aes(workclass, age)) + geom_boxplot()
adult %>% ggplot(aes(education, age)) + geom_boxplot()
adult %>% ggplot(aes(marital_status, age)) + geom_boxplot()
adult %>% ggplot(aes(occupation, age)) + geom_boxplot()
adult %>% ggplot(aes(relationship, age)) + geom_boxplot()
adult %>% ggplot(aes(race, age)) + geom_boxplot()
adult %>% ggplot(aes(native_country, age)) + geom_boxplot() + coord_flip()

adult %>% ggplot(aes(workclass, age)) + geom_boxplot()
adult %>% ggplot(aes(workclass, fnlwgt)) + geom_boxplot()
adult %>% ggplot(aes(education, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(workclass, education_num)) + geom_boxplot()
adult %>% ggplot(aes(workclass, fill = marital_status)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=occupation)) + geom_bar()
adult %>% ggplot(aes(occupation, fill=workclass)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(relationship, fill=workclass)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=relationship)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(workclass, fill= race)) + geom_bar()
adult %>% ggplot(aes(workclass, fill=sex)) + geom_bar()
adult %>% ggplot(aes(workclass, capital_gain)) + geom_boxplot()
adult %>% ggplot(aes(workclass, capital_loss)) + geom_boxplot()
adult %>% ggplot(aes(workclass, hours_per_week)) + geom_boxplot()
adult %>% ggplot(aes(native_country, fill=workclass)) + geom_bar() + coord_flip()


adult %>% ggplot(aes(education, fnlwgt)) + geom_boxplot()
adult %>% ggplot(aes(marital_status, fnlwgt)) + geom_boxplot()
adult %>% ggplot(aes(occupation, fnlwgt)) + geom_boxplot() + coord_flip()
adult %>% ggplot(aes(relationship, fnlwgt)) + geom_boxplot()
adult %>% ggplot(aes(race, fnlwgt)) + geom_boxplot()
adult %>% ggplot(aes(sex, fnlwgt)) + geom_boxplot()
adult %>% ggplot(aes(native_country, fnlwgt)) + geom_boxplot() + coord_flip()


adult %>% ggplot(aes(education, fill=education_num)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(marital_status, fill=education)) + geom_bar()
adult %>% ggplot(aes(occupation, fill = education)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(education, fill=relationship)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(education, fill = race)) + geom_bar()
adult %>% ggplot(aes(education, fill=sex)) + geom_bar()
adult %>% ggplot(aes(education, capital_gain)) + geom_boxplot()
adult %>% ggplot(aes(education, capital_loss)) + geom_boxplot()
adult %>% ggplot(aes(education, hours_per_week)) + geom_boxplot()
adult %>% ggplot(aes(native_country, fill=education)) + geom_bar() + coord_flip()


adult %>% ggplot(aes(marital_status, education_num)) + geom_boxplot()
adult %>% ggplot(aes(occupation, education_num)) + geom_boxplot() + coord_flip()
adult %>% ggplot(aes(relationship, education_num)) + geom_boxplot()
adult %>% ggplot(aes(race, education_num)) + geom_boxplot()
adult %>% ggplot(aes(sex, education_num)) + geom_boxplot()
adult %>% ggplot(aes(native_country, education_num)) + geom_boxplot() + coord_flip()


adult %>% ggplot(aes(occupation, fill=marital_status)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(marital_status, fill = relationship)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=race)) + geom_bar()
adult %>% ggplot(aes(marital_status, fill=sex)) + geom_bar()
adult %>% ggplot(aes(marital_status, capital_gain)) + geom_boxplot()
adult %>% ggplot(aes(marital_status, capital_loss)) + geom_boxplot()
adult %>% ggplot(aes(marital_status, hours_per_week)) + geom_boxplot()
adult %>% ggplot(aes(native_country, fill=marital_status)) + geom_bar() + coord_flip()


adult %>% ggplot(aes(occupation, fill=relationship)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(occupation, fill = race)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(occupation, fill=sex)) + geom_bar() + coord_flip()
adult %>% ggplot(aes(occupation, capital_gain)) + geom_boxplot() + coord_flip()
adult %>% ggplot(aes(occupation, capital_loss)) + geom_boxplot() + coord_flip()
adult %>% ggplot(aes(occupation, hours_per_week)) + geom_boxplot() + coord_flip()
adult %>% ggplot(aes(native_country, fill = occupation)) + geom_bar() + coord_flip()


adult %>% ggplot(aes(relationship, fill=race)) + geom_bar()
adult %>% ggplot(aes(relationship, fill = sex)) + geom_bar()
adult %>% ggplot(aes(relationship, capital_gain)) + geom_boxplot()
adult %>% ggplot(aes(relationship, capital_loss)) + geom_boxplot()
adult %>% ggplot(aes(relationship, hours_per_week)) + geom_boxplot()
adult %>% ggplot(aes(native_country, fill = relationship)) + geom_bar() + coord_flip()


adult %>% ggplot(aes(race, fill=sex)) + geom_bar()
adult %>% ggplot(aes(race, capital_gain)) + geom_boxplot()
adult %>% ggplot(aes(race, capital_loss)) + geom_boxplot()
adult %>% ggplot(aes(race, hours_per_week)) + geom_boxplot()
adult %>% ggplot(aes(native_country,fill=race)) + geom_bar() + coord_flip()


adult %>% ggplot(aes(sex, capital_gain)) + geom_boxplot()
adult %>% ggplot(aes(sex, capital_loss)) + geom_boxplot()
adult %>% ggplot(aes(sex, hours_per_week)) + geom_boxplot()
adult %>% ggplot(aes(native_country, fill=sex)) + geom_bar()+ coord_flip()


adult %>% ggplot(aes(native_country, capital_gain)) + geom_boxplot() + coord_flip()
adult %>% ggplot(aes(native_country, capital_loss)) + geom_boxplot() + coord_flip()
adult %>% ggplot(aes(native_country, hours_per_week)) + geom_boxplot() + coord_flip()