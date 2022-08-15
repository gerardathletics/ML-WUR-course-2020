library(MASS)
Boston

pairs(~crim + age + black + tax, Boston)


table(Boston$chas)

# f
summary(Boston$ptratio)
min(Boston$medv)

# g
which.min(Boston$medv)
Boston[399,]

which(Boston$rm > 7)
which(Boston$rm > 8)

