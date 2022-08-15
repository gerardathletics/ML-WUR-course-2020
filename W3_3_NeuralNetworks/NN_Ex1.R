library(nnet)

cheese = read.csv('cheese.csv')

nn.fit = nnet(taste~Acetic+H2S+Lactic, cheese, size=0, skip=T, linout=T)
summary(nn.fit)

nn.fit2 = nnet(taste~Acetic+H2S+Lactic, cheese, size=1, lineout=T)
summary(nn.fit2)

lm.fit = lm(taste~Acetic+H2S+Lactic, cheese)
anova(lm.fit)
