library(ggplot2)

set.seed(1)
# Generate data
x1 = runif(500)-0.5
x2 = runif(500)-0.5

#linear function
y = as.factor(1*(x1^2 - x2^2 > 0))
dat = data.frame(x1, x2, y)

plot(x1, x2, col=y)


# Perform Logistic Regression fitting
glm.fit = glm(y~x1+x2, data=dat, family='binomial')
glm.pred <- predict(glm.fit, dat, type="response")
plot(x1, x2, col=(glm.pred>0.5)+1)

# e, f

x1s = x1^2; x2s = x2^2

glm.fit = glm(y~x1s+x2s, data=dat, family='binomial')
glm.pred = predict(glm.fit, dat, type='response')
plot(x1, x2, col=(glm.pred>0.5)+1)
plot(x1s, x2s, col=(glm.pred>0.5)+1)

# g
svm.fit = svm(y~x1+x2, data=dat, kernel='linear', cost=1)
svm.pred = predict(svm.fit, dat)
plot(svm.fit,dat)

# h 
svm.fit = svm(y~x1+x2, data=dat, kernel='polynomial', cost=1, degree=2, gamma=1, coef0=1)
svm.pred = predict(svm.fit, dat)
plot(svm.fit, dat)

tune.out = tune(svm, y~x1+x2, data=dat, kernel='radial',
                ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100), gamma=c(0.25,0.5,1,2,4)))
svm.fit = tune.out$best.model
summary(svm.fit)
plot(svm.fit, dat)
