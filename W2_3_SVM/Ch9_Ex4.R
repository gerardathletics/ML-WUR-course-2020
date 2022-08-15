library(e1071)

# Generate Gaussian data, slightly shift one class and plot it

set.seed (1)
x = matrix(rnorm(100*2),ncol=2)
x[1:50,] = x[1:50,]+2
x[51:75,] = x[51:75,]-2
y = c(rep(1,75),rep(2,25))
dat = data.frame(x=x,y=as.factor(y))
plot(x,col=y)
table(predict=ypred, truth=testdat$y)

# Split into training and test set, train an RBF SVM

train = sample(100,50)

# Radial and Poly VM ------------------------------
svmfit = svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1e5)
svmfitpol = svm(y~., data=dat[train,], kernel="polynomial", gamma=1, cost=1e5)
plot(svmfitpol, dat[train,])
plot(svmfit, dat[train,])

# We can also optimize gamma using CV
tune.out = tune(svm,y~.,data=dat[train,],kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
plot(tune.out$best.model,dat[train,])

# And test the best model
table(true=dat[-train,"y"], pred=predict(tune.out$best.model, newx=dat[-train ,]))

# Linear VM -------------------------------------- 
svmfitlin = svm(y~., data=dat[train,], kernel="linear", gamma=1, scale=F)
plot(svmfitlin, dat[train,])

# Tune cost using 10-fold CV
set.seed(1)
tune.out = tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)

# Best model is stored as well
bestmod = tune.out$best.model
summary(bestmod)

plot(svmfitlin, dat[train,])
