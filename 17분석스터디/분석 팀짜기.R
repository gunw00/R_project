set.seed(200923)
data <- c("반" ,'배' ,'장', '곽', '차' , '김')
g1 <- sample(data, 3, replace=F)
g2 <- setdiff(data, g1)

g1 ; g2
