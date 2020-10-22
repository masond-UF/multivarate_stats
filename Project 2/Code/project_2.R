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

pitcher <- read.csv("Project 2/Data/Darlingtonia.csv", row=1, header=TRUE)
# Explore data ####
mshapiro.test(t(pitcher))
mvn(pitcher, mvnTest = "mardia")

pitcher_hist <- ggplot(gather(pitcher), aes(value)) + 
  					 	  geom_histogram(bins = 10) + 
  					 	  facet_wrap(~key, scales = 'free_x',ncol = 1)

pitcher_z <- scale(pitcher)
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
	wss[i] <- sum(kmeans(pitcher_z, centers = i, nstart = 25)$withinss)

	# Check out you vector of within group sum of squares for one to eight groups:
	wss 

	plot(1:10, wss, type = "b", xlab = "Number of groups", 
		 	ylab = "Within groups sum of squares") 
# Silhouette fit ####
sil <- rep(0,10)
	for (i in 2:10)
 	sil[i] <- summary(silhouette(kmeans(pitcher_z, centers = i, 
 						iter.max = 100, nstart = 25)$cluster, dist(pitcher_z)))$avg.width
	plot(2:10, sil[2:10], type = "b", xlab = "Number of groups", ylab = "average silhouette width ")
	
	
	# method dist = "binary"
	# PCA numb of observations
	# 1-total number
