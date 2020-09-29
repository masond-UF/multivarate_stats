# # 24 September 2020
# Multivariate Statistics Lab 4—Ordination 2
# David Mason
# Set up R session ####
birds <- read.csv("Lab 4/Data/Current_Hawaiian_Birds.csv", row=1, header=TRUE)
birds2 <- read.csv("Lab 4/Data/combined_birds.csv", row=1, header=TRUE)
tree <- read.csv("Lab 4/Data/tree.csv", row=1, header=TRUE)

library(vegan)
library(ca)
# Principal components analysis ####
jbirds <- vegdist(birds, "bray")
cmd<-cmdscale(jbirds, k=5, eig=TRUE) 
cmd$points

eigenvalues<-cmd$eig[1:5]
propVar<-eigenvalues/sum(eigenvalues)
cumVar<-cumsum(propVar)
PCoA_Table<-cbind(eigenvalues,propVar,cumVar)
PCoA_Table

plot(eigenvalues)
lines(lowess(eigenvalues))

x<-cmd$points[,1]
y<-cmd$points[,2]
plot(x,y,xlab= "Coordinate 1", ylab="Coordinate 2", xlim=range(x)*1.2,ylim=range(y)*1.2, type="n")
text(x,y,labels=rownames(cmd$points), cex=.9)

?ordiplot

ordiplot(scores(cmd)[,c(1,2)], type="t",cex=1, main="Hawaiian Bird PCoA")
abline(h=0,lty=3)
abline(v=0,lty=3)

species<-wascores(cmd$points[,1:2],birds)
text(species,rownames(species),cex=.7, col="red")
# PCA by hand ####
jbirds<-vegdist(birds, "bray")
CORD<--1/2*jbirds^2
C<-as.matrix(CORD)
cs<-colMeans(C)
rs<-rowMeans(C)
C1<-sweep(C,MARGIN=2,cs,FUN="-")
C2<-sweep(C1,MARGIN=1,rs,FUN="-")
delta<-mean(C)+C2

EG<-eigen(delta)
eigenvalues2<-EG$values[1:5]

propVar2<-eigenvalues2/sum(eigenvalues2)
cumVar2<-cumsum(propVar2)
PCoA_Table2<-cbind(eigenvalues2,propVar2,cumVar2)
PCoA_Table2

points2<-sweep(EG$vectors[,1:5],MARGIN=2,sqrt(eigenvalues2), FUN="*")
points2

x<-points2[,1]
y<-points2[,2]

#The coordinates:
plot(x,y,xlab= "Coordinate 1", ylab="Coordinate 2", xlim=range(x)*1.2,ylim=range(y)*1.2, type="n")
text(x,y,labels=rownames(birds), cex=.9)

# Calculate weighted species scores:
scores1<-sweep(birds,MARGIN=1,x, FUN="*")
species1<-colSums(scores1)/colSums(birds)
scores2<-sweep(birds,MARGIN=1,y, FUN="*")
species2<-colSums(scores2)/colSums(birds)

# Add to the plot:
text(cbind(species1,species2),colnames(birds),cex=.7, col="red")
# Non-metric multidimensional scaling ####
jbirds2<-vegdist(birds2, "bray") 
nmdsBird<-metaMDS(jbirds2,k=2, trace=T)
stressplot(nmdsBird)

#Identify the time period as groups:

treat=as.matrix(c(rep("Historical",6),rep("Current",6)))

#Plot out the points (islands):

ordiplot(nmdsBird,type="n",xlim=c(-.5,.5),ylim=c(-.5,.5))
## species scores not available
orditorp(nmdsBird,display="sites",col=c(rep("green",6),rep("blue",6)),air=0.01,cex=1.25)
legend(-.55,.5, c("Historical","Current"), cex=0.8, 
col=c("green","blue"), pch=15:15)

#Add a convex hull around each group:

ordihull(nmdsBird, treat, display="si",lty=1, col="green", show.groups="Historical")
ordihull(nmdsBird, treat, display="si", lty=1, col="blue", show.groups="Current")
# Correspondence Analysis (CA) ####
caTree<- ca(tree)
print(caTree) # inertia = eigenvalues
plot(caTree, xlim = c(-.5, 1),ylim = c(-.5, .5)) 
# CA by hand ####

#Divide the data matrix caTree by the grand total of the matrix:
p <- as.matrix(tree/sum(tree))

#Cross tabulate row and column sums to be used in calculating 
# expected values for the Chi Square values:
rs <- as.vector(apply(p,1,sum))
cs <- as.vector(apply(p,2,sum))

#Calculate expected values for the Chi Square calculation:
cp <- rs %*% t(cs)

#Calculate Chi Square values and check them out:
Qbar <- as.matrix((p - cp) / sqrt(cp))

#Conduct singular value decomposition (svd):

Q.svd <- svd(Qbar)


#Scale eigenvectors for rows and columns by the square root of row and column sums respectively:
V <- diag(1/sqrt(cs)) %*% Q.svd$v 
Vhat <- diag(1/sqrt(rs)) %*% Q.svd$u 

#Calculate ordination coordinates for both rows and columns:

F <- diag(1/rs) %*% p %*% V
Fhat <- diag(1/cs) %*% t(p) %*% Vhat


#Plot row and column coordinates in ordination space.

plot(Fhat[,1:2], xlim = c(-.5, 1),ylim = c(-.5, .5) , type = "n",xlab = "Coordinate 1", ylab = "Coordinate 2", lwd = 2)
text(Fhat[,1:2], labels = colnames(tree), cex = 0.7)
text(F[,1:2], labels = rownames(tree), cex = 0.7)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
