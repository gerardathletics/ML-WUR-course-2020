library(MASS)

bs <- Boston

bs$crim01 <- matrix(0,nrow(bs),1)
bs$crim01[bs$crim>median(bs$crim)] <- 1

pairs(bs)
cor(bs)

split <- runif(nrow(bs),0,1)>0.5
train <- subset(bs, split)
test <- subset(bs, !split)

#tax, age,indus,nox, rad, ptratio
crimglm <- glm(crim01~dis+indus+age+nox+rad+ptratio, data=train, family="binomial")
crim.prob <- predict(crimglm, test, type="response")
crim.pred <- rep(0,nrow(test))
crim.pred[crim.prob>0.5] = 1
errormatrix <- table(crim.pred, test$crim01)
accuracy <- (errormatrix["0","0"]+errormatrix["1","1"])/(errormatrix["0","0"]+errormatrix["0","1"]+errormatrix["1","0"]+errormatrix["1","1"])
error <- 100*(1-accuracy)
error