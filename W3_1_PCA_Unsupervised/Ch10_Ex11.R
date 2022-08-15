data = read.csv('Ch10Ex11.csv', header=F)
dd = as.dist(1- cor(data))

methods = c('complete', 'centroid', 'average', 'single')
name = 'Hc'

par(mfrow=c(2,2))

for (m in methods) {
  plot(assign(paste(name, m, sep='_'),
  hclust(dist(dd), method=m)))
}

pr.out = prcomp(t(data))
summary(pr.out)

total_load = apply(pr.out$rotation, 1, sum)
indices = order(abs(total_load), decreasing=T)
total_load[indices[1:10]]


