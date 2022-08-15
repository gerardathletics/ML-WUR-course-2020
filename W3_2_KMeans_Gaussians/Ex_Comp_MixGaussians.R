library(mixtools)

par(mfrow=c(1,1))
hist(faithful$waiting)

par(mfrow=c(1,2))
mog.fit = normalmixEM(faithful$waiting, verb=T)
plot(mog.fit, which=1)
plot(mog.fit, which=2)

mog.fit3 = normalmixEM(faithful$waiting, verb=T, k=3)
plot(mog.fit3, which=1)
plot(mog.fit3, which=2)

mog.fit4 = normalmixEM(faithful$waiting, verb=T, k=4)
plot(mog.fit4, which=1)
plot(mog.fit4, which=2)


km.out = kmeans(faithful$waiting, 2, nstart=20)
plot(faithful$waiting, col=(km.out$cluster+1), main='K-means Clustering Results with K=3', 
     xlab='', ylab='', pch=20, cex=2)


plot(faithful$waiting, col=(mog.fit3$mu+1), main='K-means Clustering Results with K=3', 
     xlab='', ylab='', pch=20, cex=2)

plot(faithful$waiting, mog.fit$posterior[,1], col=km.out$cluster)
