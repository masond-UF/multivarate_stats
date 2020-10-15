# 8 October 2020
# Multivariate Statistics Lab 6—Ordination 2
# David Mason
# Set up R session ####
library(raster)
library(cluster)
library(vegan)
library(pvclust)

birds<-read.csv("lab 6/Data/Caribbean_birds.csv", row=1, header=TRUE)
# Make distance matrix ####
distBirds<-vegdist(birds,"jaccard")
# Clustering algorithms ####
singleTree<-hclust(distBirds, method = "single")
completeTree<-hclust(distBirds, method = "complete")
centroidTree<-hclust(distBirds, method = "centroid")
medianTree<-hclust(distBirds, method = "median")
averageTree<-hclust(distBirds, method = "average")
wardTree<-hclust(distBirds, method = "ward.D2")

# Plot dendrograms ####
plot(singleTree)
plot(completeTree)
plot(centroidTree)
plot(medianTree)
plot(averageTree)
plot(wardTree)

par(mfrow=c(2,3))
plot(singleTree)
plot(completeTree)
plot(centroidTree)
plot(medianTree)
plot(averageTree)
plot(wardTree)
# Evaluating the cluster solution ####
# Agglomerative coefficient
ag1<-coef.hclust(singleTree)
ag2<-coef.hclust(completeTree)
ag3<-NA
ag4<-NA
ag5<-coef.hclust(averageTree)
ag6<-coef.hclust(wardTree)

# centroid and median are non-parametric (ranks) ASK [negative numbers?]
# agglomerative = clustering 
# cophentic = correlation 
# bootstrapping = is it different from random (p value)

methods<-c("single","complete","centroid", "median", "average", "ward")
agc<-round(c(ag1,ag2,ag3,ag4,ag5,ag6),2)
agcTable<-data.frame(methods,agc)

# parametric has underlying distribution you calculate pvalues and CI from.
# nonparametric uses ranks instead with resampling. can't use the underlying 
# distribution as a null. so instead you resample to create a null data set.
# compare the test statistic to that distribution. 

# Cophenetic correlation coefficient ####
cc1<-cor(distBirds,cophenetic(singleTree))
cc2<-cor(distBirds,cophenetic(completeTree))
cc3<-cor(distBirds,cophenetic(centroidTree))
cc4<-cor(distBirds,cophenetic(medianTree))
cc5<-cor(distBirds,cophenetic(averageTree))
cc6<-cor(distBirds,cophenetic(wardTree))
cophCor<-round(c(cc1,cc2,cc3,cc4,cc5,cc6),2)

methods<-c("single","complete","centroid", "median", "average", "ward")
dendrogramTable<-data.frame(methods,cophCor,agc)
dendrogramTable

# a loop ###
#function telling the loop to read the input as text
e = function(expr) eval(parse(text=expr)) 

#sets up a variable to fill with the output of the loop
cc<-NULL 

#list of names for the loop
methodList <- c("singleTree", "completeTree", "centroidTree", "medianTree", "averageTree", "wardTree") 

# run the loop
for (i in methodList) {
  cc[i]<-round(cor(distBirds,cophenetic(e(i))),2)
}

cc
# Bootstrapping ####
jaccard <- function(x) {
  x <- t(as.matrix(x))
  res <- vegdist(x, method="jaccard")
  res <- as.dist(res)
  attr(res, "method") <- "jaccard"
  return(res)
}

boot1<-pvclust(t(birds), method.hclust="single",method.dist=jaccard,iseed=22, nboot=100)
boot2<-pvclust(t(birds), method.hclust="complete",method.dist=jaccard,iseed=22, nboot=100)
boot3<-pvclust(t(birds), method.hclust="average",method.dist=jaccard,iseed=22, nboot=100)

par(mfrow=c(2,3))
plot(boot1)
pvrect(boot1, alpha=0.95, pv="au")
plot(boot2)
pvrect(boot2, alpha=0.95, pv="au")
plot(boot3)
pvrect(boot3, alpha=0.95, pv="au")

plot(boot1)
pvrect(boot1, alpha=0.95, pv="bp")
plot(boot2)
pvrect(boot2, alpha=0.95, pv="bp")
plot(boot3)
pvrect(boot3, alpha=0.95, pv="bp")

# Polythetic Divisive Hierarchical Clustering (PDHC) ####
# Clustering algorithm
diTree <- diana(distBirds)

# Next, plot the dendrogram:

plot(diTree, which.plots = 2)
diTree$dc

# and calculate the cophenetic correlation coefficient:

d.coph <- cor(distBirds,cophenetic(diTree))


