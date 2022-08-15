library(e1071)

# Creating data set
n = 100; r = 2; s = 0.3;

ra = 0.125*pi +runif(n/2)*1.25*pi;
xa =matrix(c(r*sin(ra),r*cos(ra))+s*rnorm(n),n/2,2)

rb = 0.375*pi -runif(n/2)*1.25*pi;
xb =matrix(c(r*sin(rb),r*cos(rb))+s*rnorm(n)+rep(-0.75*r,n),n/2,2)

x =rbind(xa,xb)
lab =c(rep(1,n/2),rep(2,n/2))
dat =data.frame(x=x, y=as.factor(lab))
plot(x,col=lab)


train = sample(nrow(dat), nrow(dat)/2)

tune.out = tune(svm, y~., data=dat[train,], kernel='linear')
svm.fit = tune.out$best.model
summary(svm.fit)
plot(svm.fit, dat)


pred = predict(svm.fit, dat[-train,])
table(pred, lab[-train])
1-mean(pred==lab[-train])

tune.out =tune(svm,y~.,data=dat[train,],kernel="polynomial",
               gamma=1,coef0=1,ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                                           degree=c(2,3,4,5)))
svm.fit = tune.out$best.model
summary(svm.fit)
plot(svm.fit, dat)   

pred = predict(svm.fit, dat[-train,])
table(pred, lab[-train])
1-mean(pred==lab[-train])


tune.out = tune(svm,y~.,data=dat[train,],kernel="radial",
                gamma=1,coef0=1,ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                                            degree=c(2,3,4,5)))
svm.fit = tune.out$best.model
summary(svm.fit)
plot(svm.fit, dat)   

pred = predict(svm.fit, dat[-train,])
table(pred, lab[-train])
1-mean(pred==lab[-train])
