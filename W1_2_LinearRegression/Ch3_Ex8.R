
auto <- read.csv("Auto.csv", na.strings="?") ## reading csv
auto <- na.omit(auto)


lrauto <- lm(mpg~horsepower, data=auto) ## lm(y~x(response,predictor), data)
summary(lrauto)
# a - i. p-value is very low so we can think that they are related
# ii. The R2 was about 0.605, meaning 60.5% of the variance in mpg is explained by horsepower
# iii. negative
predict(lrauto, data.frame(horsepower=98), interval = "confidence")
predict(lrauto, data.frame(horsepower=98), interval = "prediction")
# iiii. See above

plot(auto$horsepower, auto$mpg)
abline(lrauto)

par(mfrow=c(2,2))
plot(lrauto)


