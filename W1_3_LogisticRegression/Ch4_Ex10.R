library(ISLR)
library(ggplot2)
wk <- Weekly

plot(wk$Year, wk$Volume)
plot(wk$Year, wk$Lag1)
plot(wk$Year, wk$Lag2)
plot(wk$Year, wk$Lag3)
plot(wk$Year, wk$Lag4)
plot(wk$Year, wk$Lag5)
pairs(wk)

ggplot(wk, aes(x=Year,Lag1,color=wk$Direction))+geom_point()+geom_smooth()

wkglm <- glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=wk, family="binomial")
summary(wkglm)

wkprob <- predict(wkglm, wk, type="response") #calculation probablitiy prediction
wkpred <- rep("Down", nrow(wk)) #creating set values with value down
wkpred[wkprob > 0.5] <- "Up" #substitute value down with up if prob is higher than 0.5
table(wkpred, wk$Direction) #creating confusion matrix

train <- subset(wk, wk$Year >= 1990 & wk$Year < 2009)
test <- subset(wk, wk$Year > 2008)
wkglm2 <- glm(Direction~Lag2, train, family="binomial")
wkprob2 <- predict(wkglm2, test, type="response") #calculation probablitiy prediction
wkpred2 <- rep("Down", nrow(test)) #creating set values with value down
wkpred2[wkprob2 > 0.5] <- "Up" #substitute value down with up if prob is higher than 0.5
table(wkpred2, test$Direction) #creating confusion matrix
1 - (9+56)/(9+56+34+5) #calculating the error the logistic regression makes 37.5%