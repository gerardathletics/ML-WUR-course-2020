library(MASS)
library(ISLR)
library(ggplot2)
library(class)

bs <- Boston


bs$crim01 <- matrix(0,nrow(bs),1)
bs$crim01[bs$crim>median(bs$crim)] <- 1

split <- runif(nrow(bs),0,1)>0.5
train <- subset(bs, split)
test <- subset(bs, !split)

crimglm <- glm(crim01~dis+indus+age+nox+rad+ptratio, data=train, family="binomial")
crim.prob <- predict(crimglm, test, type="response")
crim.pred <- rep(0,nrow(test))
crim.pred[crim.prob>0.5] = 1
errormatrix <- table(crim.pred, test$crim01)
accuracy <- (errormatrix["0","0"]+errormatrix["1","1"])/(errormatrix["0","0"]+errormatrix["0","1"]+errormatrix["1","0"]+errormatrix["1","1"])
error <- 100*(1-accuracy)
error

#tax, age,indus,nox, rad, ptratio
crim.lda <- lda(crim01~dis+indus+age+nox+rad+ptratio, data=train)
crim.prob <- predict(crim.lda, test, type="response")
ldaerrormatrix <- table(crim.prob$class, test$crim01)
lda.accuracy <- (ldaerrormatrix["0","0"]+ldaerrormatrix["1","1"])/(ldaerrormatrix["0","0"]+ldaerrormatrix["0","1"]+ldaerrormatrix["1","0"]+ldaerrormatrix["1","1"])
lda.error <- 100*(1-lda.accuracy)
lda.error

train.X = as.matrix(cbind(train$dis,train$indus,train$age,train$nox,train$rad,train$ptratio))
test.X = as.matrix(cbind(test$dis,test$indus,test$age,test$nox,test$rad,test$ptratio))
knn.pred = knn(train.X,test.X,train$crim01,k=1)
knnerrormatrix <- table(knn.pred,test$crim01)
knn.accuracy <- (knnerrormatrix["0","0"]+knnerrormatrix["1","1"])/
  (knnerrormatrix["0","0"]+knnerrormatrix["0","1"]+knnerrormatrix["1","0"]+
     knnerrormatrix["1","1"])
knn.error <- 100*(1-knn.accuracy)
knn.error