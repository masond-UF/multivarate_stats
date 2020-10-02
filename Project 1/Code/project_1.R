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
caribb_env <- read.csv("Project 1/Data/Atlantic_Caribbean.csv")[,1:4]

caribb_dist <- as.matrix(vegdist(caribb_spec, "jaccard"))

nmdsBird<-metaMDS(jbirds2,k=2, trace=T)
stressplot(nmdsBird)
