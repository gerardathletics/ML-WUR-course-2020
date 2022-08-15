library(kernlab)
library(nnet)
data(spam)

spam$lab = class.ind(spam$type)
nn.fit = nnet(lab~.-type,spam,size=1,softmax=T)
summary(nn.fit)

set.seed(1)
train.idx = sample(nrow(spam),nrow(spam)/2)
spam.train = spam[train.idx,]
spam.test = spam[-train.idx,]
nn.fit2 = nnet(lab~.-type,spam.train,size=1,softmax=T)

nn.pred = predict(nn.fit, spam.train)
nn.lab = rep("nonspam", nrow(spam.train))
nn.lab[nn.pred[,1]<0.5] = "spam"
table(nn.lab,spam.train$type)
1-mean(nn.lab==spam.train$type)

nn.pred = predict(nn.fit, spam.test)
nn.lab = rep("nonspam", nrow(spam.train))
nn.lab[nn.pred[,1]<0.5] = "spam"
table(nn.lab,spam.test$type)
1-mean(nn.lab==spam.test$type)
