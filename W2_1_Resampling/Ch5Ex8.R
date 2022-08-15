set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

plot(x,y)

library(boot)
Data = data.frame(x, y)

set.seed(15)
## Computing LOOCV errors
# i.
glm.fit = glm(y ~ x)
cverror_1 = cv.glm(Data, glm.fit)$delta 

# ii.
glm.fit = glm(y ~ poly(x, 2))
cverror_2 = cv.glm(Data, glm.fit)$delta 

# iii.
glm.fit = glm(y ~ poly(x, 3))
cverror_3 = cv.glm(Data, glm.fit)$delta 

# iv.
glm.fit = glm(y ~ poly(x, 4))
cverror_4 = cv.glm(Data, glm.fit)$delta 

set.seed(30)
## Computing LOOCV errors
# i.
glm.fit = glm(y ~ x)
cverror_1r = cv.glm(Data, glm.fit)$delta 

# ii.
glm.fit = glm(y ~ poly(x, 2))
cverror_2r = cv.glm(Data, glm.fit)$delta 

# iii.
glm.fit = glm(y ~ poly(x, 3))
cverror_3r = cv.glm(Data, glm.fit)$delta 

# iv.
glm.fit = glm(y ~ poly(x, 4))
cverror_4r = cv.glm(Data, glm.fit)$delta 
## Results are exact same, because LOOCV will be the same since it evaluates n folds of a single observation.
## The quadratic polynomial had the lowest LOOCV test error rate. This was expected because it matches the true form of Y
summary(glm.fit)
# p-values show statistical significance of linear and quadratic terms, which agrees with the CV results.
