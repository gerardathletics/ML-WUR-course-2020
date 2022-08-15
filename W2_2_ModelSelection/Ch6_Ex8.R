library(leaps)
library(glmnet)

## PART A to E ----------------------------------------------
set.seed(1)
y = rnorm(100)
x = rnorm(100)

b0=2
b1=2
b2=2
b3=2

#creating some data 
y = b0 + b1*x + b2*x^2 + b3*x^3 +rnorm(100)

Data = data.frame(x,y)
attach(Data)

bestsubset = regsubsets(y~poly(x,10,raw=T),data=Data)
bss = summary(bestsubset)

par(mfrow=c(1,3))
plot(bss$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
grid()
maxadjr2 = which.max(bss$adjr2)
points(maxadjr2,bss$adjr2[maxadjr2], col="red",cex=2,pch=20)

plot(bss$cp,xlab="Number of Variables",ylab="Cp",type='l')
grid()
mincp = which.min(bss$cp)
points(mincp,bss$cp[mincp],col="red",cex=2,pch=20)

minbic = which.min(bss$bic)
plot(bss$bic,xlab="Number of Variables",ylab="BIC",type='l')
grid()
points(minbic,bss$bic[minbic],col="red",cex=2,pch=20)

## FORWARD METHOD ##--------------------------------
bssForward = regsubsets(y~poly(x,10,raw=T),data=Data, method="forward")
bssF = summary(bssForward)

par(mfrow=c(1,3))
plot(bssF$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
grid()
maxadjr2F = which.max(bssF$adjr2)
points(maxadjr2F,bssF$adjr2[maxadjr2F], col="red",cex=2,pch=20)

plot(bssF$cp,xlab="Number of Variables",ylab="Cp",type='l')
grid()
mincpF = which.min(bss$cp)
points(mincpF,bssF$cp[mincpF],col="red",cex=2,pch=20)

minbicF = which.min(bss$bic)
plot(bssF$bic,xlab="Number of Variables",ylab="BIC",type='l')
grid()
points(minbicF,bssF$bic[minbicF],col="red",cex=2,pch=20)

##BACKWARD METHOD ---------------------------
bssBackward = regsubsets(y~poly(x,10,raw=T),data=Data, method="backward")
bssB = summary(bssBackward)

par(mfrow=c(1,3))
plot(bssB$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
grid()
maxadjr2B = which.max(bssB$adjr2)
points(maxadjr2B,bssB$adjr2[maxadjr2B], col="red",cex=2,pch=20)

plot(bssB$cp,xlab="Number of Variables",ylab="Cp",type='l')
grid()
mincpB = which.min(bss$cp)
points(mincpB,bssB$cp[mincpB],col="red",cex=2,pch=20)

minbicB = which.min(bss$bic)
plot(bssB$bic,xlab="Number of Variables",ylab="BIC",type='l')
grid()
points(minbicB,bssB$bic[minbicB],col="red",cex=2,pch=20)

#comparing models coeficients
coef(bestsubset,7)
coef(bssForward,7)
coef(bssBackward,7)

## LASSO MODEL ------------------------------------
grid = 10^seq(10,-2,length=100)
x = model.matrix(y~poly(x,10,raw=T), Data)[,-1]
y = Data$y
y.test=y[test]

train = sample(1:nrow(Data), nrow(Data)/2)
test = (-train)

lasso = glmnet(x[train,], y[train],alpha=1,lambda=grid)

par(mfrow=c(1,2))
plot(lasso)

set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

bestlam = cv.out$lambda.min
lasso.pred = predict(lasso,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out = glmnet(x,y,alpha=1,lambda=grid)
lasso.coef = predict(out,type="coefficients",s=bestlam)[1:11,]
lasso.coef
lasso.coef[lasso.coef!=0]

## PART F ---------------------------------------------
set.seed(1)
y = rnorm(100)
x = rnorm(100)

b0=2
b1=2
b2=2
b3=2

#creating some data 
y = b0 + b3*x^7 +rnorm(100)

Data = data.frame(x,y)
attach(Data)

bestsubset = regsubsets(y~poly(x,10,raw=T),data=Data)
bss = summary(bestsubset)

par(mfrow=c(1,3))
plot(bss$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
grid()
maxadjr2 = which.max(bss$adjr2)
points(maxadjr2,bss$adjr2[maxadjr2], col="red",cex=2,pch=20)

plot(bss$cp,xlab="Number of Variables",ylab="Cp",type='l')
grid()
mincp = which.min(bss$cp)
points(mincp,bss$cp[mincp],col="red",cex=2,pch=20)

minbic = which.min(bss$bic)
plot(bss$bic,xlab="Number of Variables",ylab="BIC",type='l')
grid()
points(minbic,bss$bic[minbic],col="red",cex=2,pch=20)

## FORWARD METHOD part F ##--------------------------------
bssForward = regsubsets(y~poly(x,10,raw=T),data=Data, method="forward")
bssF = summary(bssForward)

par(mfrow=c(1,3))
plot(bssF$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
grid()
maxadjr2F = which.max(bssF$adjr2)
points(maxadjr2F,bssF$adjr2[maxadjr2F], col="red",cex=2,pch=20)

plot(bssF$cp,xlab="Number of Variables",ylab="Cp",type='l')
grid()
mincpF = which.min(bss$cp)
points(mincpF,bssF$cp[mincpF],col="red",cex=2,pch=20)

minbicF = which.min(bss$bic)
plot(bssF$bic,xlab="Number of Variables",ylab="BIC",type='l')
grid()
points(minbicF,bssF$bic[minbicF],col="red",cex=2,pch=20)

## BACKWARD METHOD part F ---------------------------
bssBackward = regsubsets(y~poly(x,10,raw=T),data=Data, method="backward")
bssB = summary(bssBackward)

par(mfrow=c(1,3))
plot(bssB$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
grid()
maxadjr2B = which.max(bssB$adjr2)
points(maxadjr2B,bssB$adjr2[maxadjr2B], col="red",cex=2,pch=20)

plot(bssB$cp,xlab="Number of Variables",ylab="Cp",type='l')
grid()
mincpB = which.min(bss$cp)
points(mincpB,bssB$cp[mincpB],col="red",cex=2,pch=20)

minbicB = which.min(bss$bic)
plot(bssB$bic,xlab="Number of Variables",ylab="BIC",type='l')
grid()
points(minbicB,bssB$bic[minbicB],col="red",cex=2,pch=20)

#comparing models coeficients
coef(bestsubset,7)
coef(bssForward,7)
coef(bssBackward,7)

## LASSO MODEL part F ------------------------------------
grid = 10^seq(10,-2,length=100)
x = model.matrix(y~poly(x,10,raw=T), Data)[,-1]
y = Data$y
y.test=y[test]

train = sample(1:nrow(Data), nrow(Data)/2)
test = (-train)

lasso = glmnet(x[train,], y[train],alpha=1,lambda=grid)

par(mfrow=c(1,2))
plot(lasso)

set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

bestlam = cv.out$lambda.min
lasso.pred = predict(lasso,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out = glmnet(x,y,alpha=1,lambda=grid)
lasso.coef = predict(out,type="coefficients",s=bestlam)[1:11,]
lasso.coef
lasso.coef[lasso.coef!=0]
