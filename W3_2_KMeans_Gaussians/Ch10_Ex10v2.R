set.seed(2)

n = 20
m = 50
data = rbind(matrix(rnorm(n*m, mean=5),n,m),
             matrix(rnorm(n*m, mean=10),n,m),
             matrix(rnorm(n*m, mean=15),n,m))

pr.out = prcomp(data)
par(mfrow=c(1,1))

plot(pr.out$x[,1:2], col=c(rep(1,20), rep(2,20), rep(3,20)))

summary(pr.out)$importance[2,]

par(mfrow=c(1,3))

km.out = kmeans(data, 3, nstart=20)
plot(data, col=(km.out$cluster+1), main='K-means Clustering Results with K=3', 
xlab='', ylab='', pch=20, cex=2)

km.out2 = kmeans(data, 2, nstart=20)
plot(data, col=(km.out2$cluster+1), main='K-means Clustering Results with K=2', 
     xlab='', ylab='', pch=20, cex=2)

km.out4 = kmeans(data, 4, nstart=20)
plot(data, col=(km.out4$cluster+1), main='K-means Clustering Results with K=4', 
     xlab='', ylab='', pch=20, cex=2)

#kmeans with the 2 first Principal components
km.outpc = kmeans(pr.out$x[,1:2], 3, nstart=20)
plot(data, col=(km.outpc$cluster+1), main='K-means Clustering Results with K=3', 
     xlab='', ylab='', pch=20, cex=2)

km.outpc2 = kmeans(pr.out$x[,1:2], 2, nstart=20)
plot(data, col=(km.outpc2$cluster+1), main='K-means Clustering Results with K=2', 
     xlab='', ylab='', pch=20, cex=2)

km.outpc4 = kmeans(pr.out$x[,1:2], 4, nstart=20)
plot(data, col=(km.outpc4$cluster+1), main='K-means Clustering Results with K=4', 
     xlab='', ylab='', pch=20, cex=2)

# with scaled data
scaled_data = scale(data)


km.outpc = kmeans(pr.out$x[,1:2], 3, nstart=20)
plot(scaled_data, col=(km.outpc$cluster+1), main='K-means Clustering Results with K=3', 
     xlab='', ylab='', pch=20, cex=2)

km.outpc2 = kmeans(pr.out$x[,1:2], 2, nstart=20)
plot(scaled_data, col=(km.outpc2$cluster+1), main='K-means Clustering Results with K=2', 
     xlab='', ylab='', pch=20, cex=2)

km.outpc4 = kmeans(pr.out$x[,1:2], 4, nstart=20)
plot(scaled_data, col=(km.outpc4$cluster+1), main='K-means Clustering Results with K=4', 
     xlab='', ylab='', pch=20, cex=2)