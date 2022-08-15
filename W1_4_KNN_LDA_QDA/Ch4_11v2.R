library(ISLR)
library(ggplot2)
library(MASS)
library(class)

auto <- read.csv("Auto.csv", na.strings="?") ## reading csv
auto <- na.omit(auto)

auto$mpg01 <- matrix(0, nrow(auto),1)
auto$mpg01[auto$mpg>median(auto$mpg)]=1

split <- runif(nrow(auto),0,1)>0.5
train <- subset(auto, split)
test <- subset(auto, !split)

lda <- lda(mpg01~horsepower+weight+acceleration, data=train)
lda.prob <- predict(lda, test)
ldaerrormatrix <- table(lda.prob$class, test$mpg01)
lda.accuracy <- (ldaerrormatrix["0","0"]+ldaerrormatrix["1","1"])/
  (ldaerrormatrix["0","0"]+ldaerrormatrix["0","1"]+ldaerrormatrix["1","0"]+
     ldaerrormatrix["1","1"])
lda.error <- 100*(1-lda.accuracy)
ldaerrormatrix
lda.error

qda <- qda(mpg01~horsepower+weight+acceleration, data=train)
qda.prob <- predict(qda, test)
qdaerrormatrix <- table(qda.prob$class, test$mpg01)
qda.accuracy <- (qdaerrormatrix["0","0"]+qdaerrormatrix["1","1"])/
  (qdaerrormatrix["0","0"]+qdaerrormatrix["0","1"]+qdaerrormatrix["1","0"]+
     qdaerrormatrix["1","1"])
qda.error <- 100*(1-qda.accuracy)
qdaerrormatrix
qda.error

train.X = as.matrix(cbind(train$weight,train$horsepower,train$acceleration))
test.X = as.matrix(cbind(test$weight,test$horsepower,test$acceleration))
knn.pred = knn(train.X,test.X,train$mpg01,k=1)
knnerrormatrix <- table(knn.pred,test$mpg01)
knn.accuracy <- (knnerrormatrix["0","0"]+knnerrormatrix["1","1"])/
  (knnerrormatrix["0","0"]+knnerrormatrix["0","1"]+knnerrormatrix["1","0"]+
     knnerrormatrix["1","1"])
knn.error <- 100*(1-knn.accuracy)
knnerrormatrix
knn.error