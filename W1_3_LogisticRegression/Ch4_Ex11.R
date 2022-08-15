auto <- read.csv("Auto.csv", na.strings="?") ## reading csv
auto <- na.omit(auto)

auto$mpg01 <- matrix(0, nrow(auto),1)
auto$mpg01[auto$mpg>median(auto$mpg)]=1
colors <- matrix("gold", nrow(auto),1)
colors[auto$mpg01==1] <- "black"

pairs(auto, col=colors)

split <- runif(nrow(auto),0,1)>0.5
train <- subset(auto, split)
test <- subset(auto, !split)

mpgglm <- glm(mpg01~horsepower+weight+acceleration, data=train, family="binomial")
mpg.prob <- predict(mpgglm, test, type="response")
mpg.pred <- rep(0,nrow(test))
mpg.pred[mpg.prob>0.5] = 1
table(mpg.pred, test$mpg01)