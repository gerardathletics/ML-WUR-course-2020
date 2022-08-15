library(ISLR)
library(ggplot2)
library(MASS)
library(class)
wk <- Weekly

train <- subset(wk, wk$Year >= 1990 & wk$Year < 2009)
test <- subset(wk, wk$Year > 2008)

wk.lda <- lda(Direction~Lag2, train)
wklda.prob <- predict(wk.lda, test, type="response")
wklda.pred <- wklda.prob$class
ldaerrormatrix <- table(wklda.pred, test$Direction) 
lda.accuracy <- (ldaerrormatrix["Down","Down"]+ldaerrormatrix["Up","Up"])/(ldaerrormatrix["Down","Down"]+ldaerrormatrix["Down","Up"]+ldaerrormatrix["Up","Down"]+ldaerrormatrix["Up","Up"])
lda.error <- 100*(1-lda.accuracy)
ldaerrormatrix
lda.error

wk.qda <- qda(Direction~Lag2, train)
wkqda.prob <- predict(wk.qda, test, type="response")
wkqda.pred <- wkqda.prob$class
qdaerrormatrix <- table(wkqda.pred, test$Direction) 
qda.accuracy <- (qdaerrormatrix["Down","Down"]+qdaerrormatrix["Up","Up"])/(qdaerrormatrix["Down","Down"]+qdaerrormatrix["Down","Up"]+qdaerrormatrix["Up","Down"]+qdaerrormatrix["Up","Up"])
qda.error <- 100*(1-qda.accuracy)
qdaerrormatrix
qda.error

train.X <- as.matrix(train$Lag2)
test.X <- as.matrix(test$Lag2)
labels <- train$Direction
knn.pred <- knn(train.X,test.X,labels,k=1)
knnerrormatrix <- table(knn.pred,test$Direction)
knn.accuracy <- (knnerrormatrix["Down","Down"]+knnerrormatrix["Up","Up"])/(knnerrormatrix["Down","Down"]+knnerrormatrix["Down","Up"]+knnerrormatrix["Up","Down"]+knnerrormatrix["Up","Up"])
knn.error <- 100*(1-knn.accuracy)
knnerrormatrix
knn.error

#logistic and LDA have best performance in this case, the error is lower
