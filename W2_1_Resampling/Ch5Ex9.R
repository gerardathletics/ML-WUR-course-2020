library(MASS)
library(boot)

bs = Boston

set.seed(1)
attach(Boston) 

#Estimate for the population mean
medv.mean = mean(medv)
#Estimate for the std error
#We do it computing: sample std / sqrt of number of observations
medv.err = sd(medv/sqrt(length(medv)))
#Estimate sdt error using bootstrap
boot.fn = function(data, index) return(mean(data[index]))
bstrap = boot(medv, boot.fn, 1000)
#result is similar to the calculated before

#Calculating 95% confidence interval for the mean of medv
t.test(medv)
#Comparing the 2 confidence intervals
c(bstrap$t0-2 * 0.4119, bstrap$t0+2 * 0.4119)

#Calculating the median of medv
medv.med = median(medv)

#Estimating the standard error of medv.med using bootstrap
boot.fn = function(data,index) return(median(data[index]))
boot(medv, boot.fn, 1000)
#Median of 21.2 with SE of 0.380. Small standard error relative to median value.

#Estimate for the tenth percentile of medv in Bs suburbs
medv.tenth = quantile(medv, c(0.1))

#Using bootstrap to estimate the std error of medv.tenth
boot.fn = function(data, index) return(quantile(data[index], c(0.1)))
boot(medv, boot.fn, 1000)
#Tenth-percentile of 12.75 with SE of 0.511. Small standard error relative to tenth-percentile value.
