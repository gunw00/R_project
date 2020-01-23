setwd('C:\\Users\\NT500R\\Desktop\\AIR 19년도 여름방학')

library(readxl)
data <- read_excel(path = '인상 조사 설문지답변.xlsx', sheet = '설문지 응답 시트1', col_names = TRUE)
#install.packages('wordcloud')
#install.packages('KoNLP')
library(wordcloud)
library(RColorBrewer)
library(KoNLP)


male_data <- data[,6]
female_data <- data[,7]

head(male_data)
head(female_data)

dt1 <- sapply(male_data, extractNoun, USE.NAMES = F) #sapply는 list형태로 리턴됨.
dt1_us <- unlist(dt1) #분석을 위해 vector형으로 바꿔줌.
head(dt1_us)
wordcount <- table(dt1_us)
wordcount <- sort(wordcount,decreasing = T)
head(wordcount,100)


####전처리####
dt1_us <- gsub('[~!@#$%&*()_+=?<>]','',dt1_us)
dt1_us <- gsub("\\[","",dt1_us)
dt1_us <- gsub('[ㄱ-ㅎ]','',dt1_us)
dt1_us <- gsub('(ㅜ|ㅠ)','',dt1_us)
dt1_us <- gsub("\\d+","",dt1_us)
head(dt1_us)

dt1_us <- Filter(function(x){nchar(x)>=2}, dt1_us) #두글자 이상 단어만 뽑기.
wordcount <- table(dt1_us)
wordcount <- sort(wordcount,decreasing = T)
head(wordcount,100) #100개를 살펴본 결과 제거 안된 단어는 직접 제거하기


dt1_us <- gsub("성격","",dt1_us)
dt1_us <- gsub("같습니","",dt1_us)
dt1_us <- gsub("스타일","",dt1_us)
dt1_us <- gsub("여자","",dt1_us)
dt1_us <- gsub("친구","",dt1_us)
dt1_us <- gsub("매일","",dt1_us)
dt1_us <- gsub("사람","",dt1_us)
dt1_us <- gsub("생각","",dt1_us)
dt1_us <- gsub("허허허","",dt1_us)
dt1_us <- gsub(".눈썹이","",dt1_us)
dt1_us <- gsub(".인상이","",dt1_us)
dt1_us <- gsub(".코가","",dt1_us)
dt1_us <- gsub(".키","",dt1_us)
dt1_us <- gsub("과제하면","",dt1_us)
dt1_us <- gsub("대한","",dt1_us)
dt1_us <- gsub("도와달라고","",dt1_us)
dt1_us <- gsub("들이","",dt1_us)
dt1_us <- gsub("민국","",dt1_us)
dt1_us <- gsub("반응","",dt1_us)


dt1_us <- Filter(function(x){nchar(x)>=2}, dt1_us) #두글자 이상 단어만 뽑기.
wordcount <- table(dt1_us)
wordcount <- sort(wordcount,decreasing = T)
wordcount <- head(wordcount,100) #100개를 살펴본 결과 제거 안된 단어는 직접 제거하기




display.brewer.all()
color <- brewer.pal(12, "Set3")
windowsFonts(font=windowsFont("a한글사랑L"))

wordcloud(names(wordcount), wordcount, scale=c(5,0.5),random.order = FALSE, random.color = TRUE, colors = color, family = "font")
