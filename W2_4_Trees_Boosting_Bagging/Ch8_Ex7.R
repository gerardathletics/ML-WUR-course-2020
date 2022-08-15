library(MASS)
library(randomForest)

set.seed(1101)
bs=Boston

train = sample(dim(bs)[1], dim(bs)[1]/2)
X.train = bs[train,-14]
X.test = bs[-train,-14]
Y.train = bs[train, 14]
Y.test = bs[-train, 14]

p = dim(bs)[2]-1
p.2 = p/2
p.sq = sqrt(p)

rf.bs.p = randomForest(X.train, Y.train, xtest=X.test, ytest = Y.test, 
                       mtry=p, ntree=500)
rf.bs.p.2 = randomForest(X.train, Y.train, xtest=X.test, ytest = Y.test, 
                         mtry=p.2, ntree=500)
rf.bs.p.sq = randomForest(X.train, Y.train, xtest=X.test, ytest = Y.test, 
                          mtry=p.sq, ntree=500)

plot(1:500, rf.bs.p$test$mse, col='green', type='l', xlab='Number of Trees', 
     ylab='Test MSE',
     ylim=c(10,19))
lines(1:500, rf.bs.p.2$test$mse, col='red', type='l')
lines(1:500, rf.bs.p.sq$test$mse, col='blue', type='l')
legend('topright', c('m=p', 'm=p/2', 'm=sqrt(p)'), col=c('green', 'red', 'blue'), 
       cex=1, lty=1)

