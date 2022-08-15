library(ISLR)
library(tree)

# Data
set.seed(1013)
oj = OJ

train = sample(dim(oj)[1], 800)

oj.train = oj[train,]
oj.test = oj[-train,]

#Computes tree
oj.tree = tree(Purchase~., data=oj.train)
summary(oj.tree)
oj.tree
plot(oj.tree)
text(oj.tree, pretty=0)

# Predicts the response on the test data and produces a confusion matrix
oj.pred = predict(oj.tree, oj.test, type='class')
table(oj.test$Purchase, oj.pred)

#Calculates the optimal tree size and plots it
oj.cv = cv.tree(oj.tree, FUN= prune.tree)
plot(oj.cv$size, oj.cv$dev, type='b', xlab='Tree Size', ylab='Deviance')

summary(oj.cv)

# Creating prune tree with the lowest cross-validation (6)
oj.pruned = prune.tree(oj.tree, best=6)
summary(oj.pruned)

# Getting the training error to compare
pred.unpruned = predict(oj.tree, oj.test, type='class') #its the same as oj.pred
misclass.unpruned = sum(oj.test$Purchase != pred.unpruned)
misclass.unpruned/length(pred.unpruned)

pred.pruned = predict(oj.pruned, oj.test, type='class')
misclass.pruned = sum(oj.test$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)