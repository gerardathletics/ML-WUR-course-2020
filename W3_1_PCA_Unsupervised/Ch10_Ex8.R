data = USArrests

pr.out = prcomp(data, scale=T)
pr.out$sdev
pr.out$center

biplot(pr.out, scale=0)

pr.var = pr.out$sdev^2

pve = pr.var/sum(pr.var)
