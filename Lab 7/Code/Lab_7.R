# 15 October 2020
# Multivariate Statistics Lab 7—Testing for groups: 
# MRPP, PerMANVOA, Analysis of similarity (ANOSIM)
# David Mason
# Set up workspace ####
library(raster)
library(cluster)
library(vegan)
library(ade4)

birds <- read.csv("Lab 7/Data/combined_birds.csv", row=1, header=TRUE)
snails <- read.csv("Lab 7/Data/snail_data.csv", row=1, header=TRUE)[,1:3]

# Multiple Response Permutation Procedure (MRPP) ####
# k-means clustering
snails <- scale(snails)
snails.kop <- kmeans(snails, centers= 2, iter.max=10, nstart=25)
groups <- snails.kop$cluster
# MRPP
?mrpp
distSnails <- vegdist(snails, method="euclidean")
set.seed(11)
snailMRPP <- mrpp(distSnails, groups, permutations = 1000)
# Plot the delta
hist(snailMRPP$boot.deltas, main = "Histogram of snail MRPP deltas" )
points(snailMRPP$delta,0, pch=19,col="red", bg="red", cex=2)
# PerMANOVA ####
# NMDS
jbirds <- vegdist(birds, "bray") 
nmdsBird <- metaMDS(jbirds,k=2, trace=T)

group = as.matrix(c(rep("Historical",6),rep("Current",6)))
ordiplot(nmdsBird,type="n",xlim=c(-.5,.5),ylim=c(-.5,.5))

orditorp(nmdsBird,display="sites",col=c(rep("green",6),rep("blue",6)),air=0.01,cex=1.25)
legend(-.55,.5, c("Historical","Current"), cex=0.8, 
col=c("green","blue"), pch=15:15)

# ADONIS
?adonis

set.seed(11) 
permBirds <- adonis(jbirds ~ group, permutations=1000)

hist(permBirds$f.perms, main = "Histogram of F statistics for Hawaiian Birds", xlim=c(0,12))
points(permBirds$aov.tab$F[1],0, pch=19,col="red", bg="red", cex=2)

# Analysis of Group Similarities (ANOSIM) ####

jbirds <- vegdist(birds, "bray") 

nmdsBird <- metaMDS(jbirds,k=2, trace=T)

# For plotting the NMDS, create the groups to assign different colors to each time period.
group=as.matrix(c(rep("Historical",6),rep("Current",6)))
ordiplot(nmdsBird,type="n",xlim=c(-.5,.5),ylim=c(-.5,.5))
## species scores not available
orditorp(nmdsBird,display="sites",col=c(rep("green",6),rep("blue",6)),air=0.01,cex=1.25)
legend(-.55,.5, c("Historical","Current"), cex=0.8, 
col=c("green","blue"), pch=15:15)

?anosim 
set.seed(11) 
birdAnosim <- anosim(jbirds, group, permutations = 1000)

hist(birdAnosim$perm, main = "Histogram of R statistics for Hawaiian Birds", xlim=c(-.5,1 ))
points(birdAnosim$statistic,0, pch=19,col="red", bg="red", cex=2)




