# 1 October 2020
# Multivariate Statistics Lab 5—Cluster analysis 1
# David Mason
# Set up R session ####
snails <- read.csv("Lab 5/data/snail_data.csv", row=1, header=TRUE)[,1:3]

library(raster)
library(cluster)
library(mvnormtest)
library(MVN)
library(tidyverse)

# Assessing normaility ####
mshapiro.test(t(snails))

mvn(snails, mvnTest = "mardia")

ggplot(gather(snails), aes(value)) + 
  					 geom_histogram(bins = 10) + 
  					 facet_wrap(~key, scales = 'free_x',ncol = 1)


snails2 <- snails[,c(1,2)]
result <- mvn(snails2, mvnTest = "mardia", multivariatePlot = "persp")

snails3 <- snails[,c(1,3)]
result <- mvn(snails3, mvnTest = "mardia", multivariatePlot = "persp")
# Standardize? ####

snail.tot <- apply(snails,2, sum)

cv(snail.tot)
z_snails <- scale(snails)

# K means ####
wss <- rep(0, 8)

#Run a loop for 1 to 8 clusters:
for (i in 1:8){ # sets the number of times the loop will be run i.e., the number of clusters in this case)
  
wss[i] <- sum(kmeans(z_snails, centers = i,nstart=25)$withinss) # run the kmeans function for each number of clusters (i) and extract the within sum of squares for each.
}
# Check out you vector of within group sum of squares for one to eight groups:
wss 

plot(1:8, wss, type = "b", xlab = "Number of groups", 
		 ylab = "Within groups sum of squares") 

?silhouette

sil <- rep(0,8)
for (i in 2:8){
sil[i] <- summary(silhouette(kmeans(z_snails, centers=i, iter.max=100, nstart=25)$cluster, dist(z_snails)))$avg.width
}
plot(2:8, sil[2:8], type = "b", xlab = "Number of groups", ylab = "average silhouette width ")

snails.kop <- kmeans(z_snails, centers= 2, iter.max=10, nstart=25)
pairs(snails, panel=function(x,y,z) text(x,y,snails.kop$cluster))

# PCA ####
snail.pc <- princomp(z_snails, cor=F)
summary(snail.pc)

snail.pc$loadings

my.color.vector <- rep("green", times=nrow(z_snails))
my.color.vector[snails.kop$cluster==1] <- "blue"
my.color.vector[snails.kop$cluster==2] <- "green"

plot(snail.pc$scores[,1], snail.pc$scores[,2], ylim=range(snail.pc$scores[,1]),xlim=range(snail.pc$scores[,1]*1.25), xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(snail.pc$scores[,1], snail.pc$scores[,2], labels=rownames(snails), cex=1.25, lwd=2,
     col=my.color.vector)
?ordiplot()

library('vegan')

ordiplot(snail.pc)
