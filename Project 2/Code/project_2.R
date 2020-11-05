# 20 October 2020
# Multivariate Statistics Project 2—Cluster analysis: 
# David Mason
# Set up workspace ####
library(vegan)
library(raster)
library(cluster)
library(mvnormtest)
library(MVN)
library(tidyverse)
library(ade4)
library(pvclust)

pitcher <- read.csv("Project 2/Data/Darlingtonia.csv", row=1, header=TRUE)
dune <- read.csv("Project 2/Data/dune_data.csv", row=1, header=TRUE)

# Explore data ####
mshapiro.test(t(pitcher))
mvn(pitcher, mvnTest = "mardia")

pitcher.hist <- ggplot(gather(pitcher), aes(value)) + 
  					 	  geom_histogram(bins = 10) + 
  					 	  facet_wrap(~key, scales = 'free_x',ncol = 1)

pitcher$tube_diam <- log1p(pitcher$tube_diam)
pitcher$keel_diam <- log1p(pitcher$keel_diam)
pitcher$wing2_length <- log1p(pitcher$wing2_length)
pitcher$hoodarea <- log1p(pitcher$hoodarea)
pitcher$wingarea <- log1p(pitcher$wingarea)
pitcher$tubearea <- log1p(pitcher$tubearea)

pitcher.tot<- apply(pitcher,2, sum)
cv(pitcher.tot)

pitcher.z <- scale(pitcher)

dune.tot<- apply(dune,2, sum)
cv(dune.tot)




####################  K-means clustering ####################  
# Scree plot method ####
# Set a vector for the loop to fill. 
wss <- rep(0, 10)

# Run a loop for 1 to 8 clusters:

# sets the number of times the loop will be run i.e., 
# the number of clusters in this case)
for (i in 1:10) 
  
	# run the kmeans function for each number of clusters (i) 
	# and extract the within sum of squares for each.
	wss[i] <- sum(kmeans(pitcher.z, centers = i, nstart = 25)$withinss)

	# Check out you vector of within group sum of squares for one to eight groups:
	wss 

	plot(1:10, wss, type = "b", xlab = "Number of groups", 
		 	ylab = "Within groups sum of squares") 
# Silhouette fit ####
sil <- rep(0,10)
	for (i in 2:9)
 	sil[i] <- summary(silhouette(kmeans(pitcher.z, centers = i, 
 						iter.max = 100, nstart = 25)$cluster, dist(pitcher.z)))$avg.width
	plot(2:10, sil[2:10], type = "b", xlab = "Number of groups", ylab = "average silhouette width ")
	
	
	# method dist = "binary"
	# PCA numb of observations
	# 1-total number

# PCA plot ####
pitcher.pc <- princomp(pitcher.z, cor=F)
summary(pitcher.pc)
pitcher.pc$loadings

pitcher.z.kop <- kmeans(pitcher.z, centers= 2, iter.max=10, nstart=25)

my.color.vector <- rep("green", times=nrow(pitcher.z))
my.color.vector[pitcher.z.kop$cluster==1] <- "blue"
my.color.vector[pitcher.z.kop$cluster==2] <- "green"

plot(pitcher.pc$scores[,1], pitcher.pc$scores[,2], 
		 ylim=range(pitcher.pc$scores[,1]),xlim=range(pitcher.pc$scores[,1]*1.25), 
		 xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(pitcher.pc$scores[,1], pitcher.pc$scores[,2], labels=rownames(pitcher.z),
		 cex=1.25, lwd=2, col=my.color.vector)

biplot(pitcher.pc, xlabs= rep("",87),xlim=range(-.55,.55))
text(pitcher.pc$scores[,1], pitcher.pc$scores[,2], labels=rownames(pitcher.z), 
		 cex=1.25, lwd=2, col=my.color.vector)
# MRPP ####
groups <- pitcher.z.kop$cluster
pitcher.dist <- vegdist(pitcher.z, method="euclidean")

set.seed(11)
pitcher.z.MRPP <- mrpp(pitcher.z, groups, permutations = 1000)
# ANOSIM ####
pitcher.j <- vegdist(pitcher, "bray") 
set.seed(11) 
pitcher.z.ANOSIM <- anosim(pitcher.j, groups, permutations = 1000)
pitcher.z.ANOSIM
##########  Polythetic Agglomerative Hierarchical Clustering ##########  
# Clustering algorithms ####
dune.j <- vegdist(dune, "jaccard") 

singleTree <- hclust(dune.j, method = "single")
completeTree <- hclust(dune.j, method = "complete")
centroidTree <- hclust(dune.j, method = "centroid")
medianTree <- hclust(dune.j, method = "median")
averageTree <- hclust(dune.j, method = "average")
wardTree <- hclust(dune.j, method = "ward.D2")

plot(averageTree)
# Cophenetic correlation coefficient ####
e <- function(expr) eval(parse(text=expr)) 
cc <- NULL 

#list of names for the loop
methodList <- c("singleTree", "completeTree", "centroidTree", "medianTree", "averageTree", "wardTree") 

# run the loop
for (i in methodList) {
  cc[i]<-round(cor(dune.j,cophenetic(e(i))),2)
}
cc
# Agglomerative coefficient ####
ag1 <- coef.hclust(singleTree)
ag2 <- coef.hclust(completeTree)
ag3 <- coef.hclust(averageTree)
ag4 <- coef.hclust(wardTree)

methods <- c("single","complete", "average", "ward")
agc <- round(c(ag1,ag2,ag3,ag4),2)
agcTable <- data.frame(methods,agc)
agcTable
# Bootstrapping ####
jaccard <- function(x) {
  x <- t(as.matrix(x))
  res <- vegdist(x, method="jaccard")
  res <- as.dist(res)
  attr(res, "method") <- "jaccard"
  return(res)
}

boot  <- pvclust(t(dune), method.hclust="average",
				 method.dist=jaccard,iseed=22, nboot=100)

plot(boot)
pvrect(boot, alpha=0.95, pv="au")

##########   Polythetic Divisive Hierarchical Clustering  ##########  
dune.j <- vegdist(dune, "jaccard") 
# Clustering algorithm ####
dune.diana <- diana(dune.j)
plot(dune.diana, which.plots = 2)
# Cophenetic correlation coefficient #####
dune.diana.coph <- cor(dune.j,cophenetic(dune.diana))
dune.diana.coph
# Calculate the divisive coefficient #####
dune.diana$dc 


