
auto <- read.csv("Auto.csv", na.strings="?") ## reading csv
auto <- na.omit(auto)
fix(auto)

range(auto$mpg)
summary(auto)

auto <- auto[-c(10:84),] #deleting rows from 10 to 85

pairs(~mpg + acceleration + horsepower + weight, auto)