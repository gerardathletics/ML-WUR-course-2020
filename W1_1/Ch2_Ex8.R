

college <- read.csv("College.csv") ## reading csv
fix(college)
rownames(college)=college[,1] ## seting college names as rownames
fix(college)
college=college[,-1] ## removing numbers column because now college names are row names
fix(college)

summary(college)
pairs(college[,1:10])

outstate <- college$Outstate
private <- college$Private

plot(y=outstate, x=private)

elite <- rep("No", nrow(college))
elite[college$Top10perc > 50] <- "Yes"
elite <- as.factor(elite)
college <- data.frame(college, elite)
summary(elite)
plot(x=elite, y=outstate)

par(mfrow=c(2,2))
hist(college$Accept, breaks=4)
hist(college$Top25perc, breaks=8)
hist(college$PhD, breaks=5)
hist(college$Personal, breaks=5)
