DF = read.csv("Ch10Ex11.csv",header=FALSE)
DF = t(DF) 

D = dist(DF)                  # "n x n matrix of Euclidean distance dissimilarities"
Dcor = as.dist( 1 - cor(t(DF)) ) # cor computes the correlation of *columns* so we need to take the transpose of DF
hclust.cor = hclust( Dcor, method="complete" )

# Question c) to compare genes in each cluster, one option is to use a t-test between the means of the gene response in each cluster: 
# (alternatively, if you would only compare the means and not take the variance into account, the picture would not change a lot)

clusterlabel=cutree( hclust.cor, k=2 ) 

n1 = apply( DF[ clusterlabel==1, ], 2, length ) # the number of samples (number of patients in each cluster)
n2 = apply( DF[ clusterlabel==2, ], 2, length )

m1 = apply( DF[ clusterlabel==1, ], 2, mean ) # the means across the 1000 genes in each cluster
m2 = apply( DF[ clusterlabel==2, ], 2, mean )

v1 = apply( DF[ clusterlabel==1, ], 2, var ) # the variances across the 1000 genes in each cluster
v2 = apply( DF[ clusterlabel==2, ], 2, var )

pooled_variance = sqrt( v1 / n1 + v2 / n2 )
#calculate t statistic for each gene
t_value = ( m1 - m2 ) / pooled_variance 

#order genes based on t_value,get the 10 with highest value (i.e. most discriminating between the two samples)
indices = order(abs(t_value), decreasing=T)
t_value[indices[1:10]]
#plot the expression of these 10 genes in the 40 samples; the 40 samples are ordered using the correlation based dendrogram
heatmap(DF[,indices[1:10]],Rowv=as.dendrogram(hclust.cor),scale="none")
