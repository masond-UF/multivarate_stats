# 29 September 2020—Project 1
# David Mason
# Set-up workspace ####
library(mvnormtest)
library(MVN)
library(MVA)
library(psych)
library(Hmisc)
library(vegan)
library(StatMatch)
library(MASS)
library(tibble)
library(tidyverse)
# Question 1
# Check assumptions ####
darl <- read.csv("Project 1/Data/Darlingtonia.csv")[,2:11]

mshapiro.test(t(darl))
mvn(darl, mvnTest = "mardia")

# Fails the conservative tests. Lets look at histograms
darl_hist <- ggplot(gather(darl), aes(value)) + 
  					 geom_histogram(bins = 10) + 
  					 facet_wrap(~key, scales = 'free_x',ncol = 1)

# I don't think any of them look bad. Lets standardize to get 
# everything on the same scale

Z_darl <-scale(darl)
# Run PCA ####
darl.pc <- princomp(Z_darl, cor=F)
summary(darl.pc)
darl.pc$loadings

# Scree
plot(darl.pc, type="lines") # 2 axes

# Biplot 
biplot(darl.pc$scores,darl.pc$loading,xlab="PC 1, 49%", ylab="PC 2, 22%",
			 expand= 1)

# Question 2
# Set-up workspace ####
library(vegan)
library(ca)
# Check assumptions ####
caribb_spec <- read.csv("Project 1/Data/Atlantic_Caribbean.csv")[,5:385]
caribb_ocean <- read.csv("Project 1/Data/Atlantic_Caribbean.csv")[,3]
caribb_arch <- read.csv("Project 1/Data/Atlantic_Caribbean.csv")[,2]

mshapiro.test(t(caribb_spec)) # get a weird error
mvn(caribb_spec, mvnTest = "mardia") # get a weird error

# Run NMDS ####
caribb_dist <- (vegdist(caribb_spec, "jaccard"))
nmds_carrib <- metaMDS(caribb_dist,k=2, trace=T)
stressplot(nmds_carrib)

treat <- as.matrix(c(rep("Atlantic",),rep("Current",6)))

ordiplot(nmds_carrib,type="n")
orditorp(nmds_carrib, display="sites",col=c(rep("green",35),rep("blue",19)),air=0.01,cex=1.25)
legend(-.5,.45, c("Atlantic","Caribbean"), cex=0.8, col=c("green","blue"), pch=15:15)
ordihull(nmds_carrib, caribb_ocean, display="si",lty=1, col="green", show.groups="Atlantic")
ordihull(nmds_carrib, caribb_ocean, display="si",lty=1, col="blue", show.groups="Caribbean")

ordiplot(nmds_carrib,type="n")
orditorp(nmds_carrib, display="sites",col=c(rep("green",11),rep("red",13),
																						rep("purple",9), rep("blue", 2),
																						rep("black",4), rep("brown",12),
																						rep("gray",3)),air=0.01,cex=1.25)
legend(-.5,.45, c("Canaries","Cape Verdes", "Azores", "Madeira", 
									"Greater Antilles", "Lesser Antilles", "Caymans"), cex=0.8, 
			 						col=c("green","red", "purple", "blue", "black", "brown", "gray"), pch=15:15)
ordihull(nmds_carrib, caribb_arch, display="si",lty=1, col="green", show.groups="Canaries")
ordihull(nmds_carrib, caribb_arch, display="si",lty=1, col="red", show.groups="CapeVerdes")
ordihull(nmds_carrib, caribb_arch, display="si",lty=1, col="purple", show.groups="Azores")
ordihull(nmds_carrib, caribb_arch, display="si",lty=1, col="blue", show.groups="Madeira")
ordihull(nmds_carrib, caribb_arch, display="si",lty=1, col="black", show.groups="GreaterAntilles")
ordihull(nmds_carrib, caribb_arch, display="si",lty=1, col="brown", show.groups="LesserAntilles")
ordihull(nmds_carrib, caribb_arch, display="si",lty=1, col="gray", show.groups="Caymans")

