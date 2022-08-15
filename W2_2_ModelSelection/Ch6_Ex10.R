## Part a ----------------------------------------
set.seed(1)

p = 20
n = 1000
 
# Generate Noise
X = matrix(rnorm(n*p), n, p)
colnames(X) = paste('X', 1:20, sep='')

beta = rnorm(20); beta[3] = 0; beta[5]; beta[7] = 0; beta[11] = 0; beta[13] = 0; 
beta[17] = 0; beta[19] = 0

names(beta) = paste('X', 1:20, sep='')

Y = X %*% beta + rnorm(n)
summary(Y)


## Part b ----------------------------------------
train = sample(1:n, 0.1*n)
Xtrn = X[train,]; Xtst = X[-train,]
Ytrn = Y[train]; Ytst = Y[-train]

## Part c- e ----------------------------------------
rf = regsubsets(x=Xtrn, y=Ytrn, nvmax=20)
rfs = summary(rf)

par(mfrow=c(2,2))
plot(rfs$rss,type="l",xlab="#variables",ylab="RSS")

plot(rfs$cp,type="l",xlab="#variables",ylab="Cp")
points(which.min(rfs$cp),rfs$cp[which.min(rfs$cp)],col="red",cex=2,pch=20)

plot(rfs$bic,type="l",xlab="#variables",ylab="BIC")
points(which.min(rfs$bic),rfs$bic[which.min(rfs$bic)],col="red",cex=2,pch=20)

plot(rfs$adjr2,type="l",xlab="#variables",ylab="adjusted RSq")
points(which.max(rfs$adjr2),rfs$adjr2[which.max(rfs$adjr2)],col="red",cex=2,pch=20)

dftrn =data.frame(Ytrn,Xtrn)
dftst =data.frame(Ytst,Xtst)
trnmat =model.matrix(Ytrn~.,data=dftrn)
tstmat =model.matrix(Ytst~.,data=dftst)
trn_mse =c(); tst_mse =c()
for(k in 1:20) {
  coefk =coef(rf,id=k)
  pred =trnmat[,names(coefk)]%*%coefk 
  trn_mse[k] =mean((Ytrn-pred)^2)
  pred =tstmat[,names(coefk)]%*%coefk
  tst_mse[k] =mean((Ytst-pred)^2)
  }

par(mfrow=c(1,1))
plot(trn_mse,type="l",col="blue")
lines(tst_mse,type="l",col="red")

## Part e-f ----------------------------------------------------------------
which.min(trn_mse)
which.min(tst_mse)

coef(rf, id=which.min(tst_mse))
beta[-which(beta==0)]

# part G -------------------------------------------------------------------
diff = c()
for (k in 1:20) {
  coefk = coef(rf, id=k)
  diff[k] = sqrt(sum((coefk -c(0,beta[names(coefk[-1])]))^2))
}
plot(1:20, diff, type='l', col='blue')