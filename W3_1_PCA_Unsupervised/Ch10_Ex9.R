data = USArrests

# dist computes euclidean distance
# Computing hierarchical cluster
hc.complete = hclust(dist(data), method='complete')
plot(hc.complete)

clust3 = cutree(hc.complete, 3)

scaled_data = scale(data)
hc.compl.sc = hclust(dist(scaled_data), method='complete')
plot(hc.compl.sc)
