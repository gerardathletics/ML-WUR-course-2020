# 4.6.3 LDA

# Create a training set (until 2005)
library(ISLR)
library(MASS)
train=(Smarket$Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Smarket$Direction[!train]

# Train an LDA classifier and plot it
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)

# Use it to predict the test data (after 2005)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)  
lda.class=lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005) 

# Predictions are just thresholded posteriors
sum(lda.pred$posterior[,1]>=.5) 
sum(lda.pred$posterior[,1]<.5) 

# The posterior is the probability that the market will decrease
lda.pred$posterior[1:20,1]
lda.class[1:20]

# How often are we very certain about that?
sum(lda.pred$posterior[,1]>.9)

# 4.6.4 QDA

# Train a QDA classifier
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit

# Predict for years after 2005
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

# 4.6.5 K-NN

# Create training and test set as matrices
library(class)
train.X = cbind(Smarket$Lag1,Smarket$Lag2)[train,]
test.X = cbind(Smarket$Lag1,Smarket$Lag2)[!train,]
train.Direction=Smarket$Direction[train]

# Train and test a 1-NN classifier
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)

# Train and test a 3-NN classifier
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
