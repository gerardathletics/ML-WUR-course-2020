auto <- read.csv("Auto.csv", na.strings="?") ## reading csv
auto <- na.omit(auto)

pairs(auto) ## scatterplot of all the variables

#auto <- auto[-9] ## taking out the names column because it's not numerical
cor(subset(auto, select=-name)) 

lmauto <- lm(mpg~. -name, data=auto)
summary(lmauto)

par(mfrow=c(2,2))
plot(lmauto)

par(mfrow=c(1,1))
plot(predict(lmauto),rstudent(lmauto))

lmauto2 =lm(mpg~cylinders*displacement+displacement*weight) #use highest values in correlation matrix
summary(lmauto2)

lmauto3 <- lm(mpg~sqrt(weight)+log(acceleration)+horsepower+I(horsepower^2), data=auto) #after I we write the formula
summary(lmauto3)
par(mfrow=c(2,2))
plot(lmauto3)

lmauto4 <-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=auto)
summary(lmauto4)
plot(lmauto4)