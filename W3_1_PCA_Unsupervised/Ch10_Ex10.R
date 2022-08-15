n = 20
m = 50
data = rbind(matrix(rnorm(n*m, mean=0),n,m),
      matrix(rnorm(n*m, mean=0.5),n,m),
      matrix(rnorm(n*m, mean=1),n,m))

pr.out = prcomp(data)$x
par(mfrow=c(1,1))

plot(pr.out[,1:2], col=c(rep(1,20), rep(2,20), rep(3,20)))
