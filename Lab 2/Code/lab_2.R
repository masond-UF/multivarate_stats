# Lab 2—David Mason—10 September 2020
# Install and load packages #####
install.packages("mvnormtest")
install.packages("MVN")
install.packages("MVA")
install.packages("psych")
install.packages("Hmisc")
install.packages("vegan")
install.packages("StatMatch")
install.packages("MASS")
install.packages("raster")
install.packages("cluster")
library(mvnormtest)
library(MVN)
library(MVA)
library(psych)
library(Hmisc)
library(vegan)
library(StatMatch)
library(MASS)
library(raster)
library(cluster)
# Importing data ####
usAir <- USairpollution
usAir_mod <- read.csv("Lab 2/Data/usAir_mod.csv", 
											row=1, header=TRUE)
# Data screening ####
describeBy(usAir)
describeBy(usAir_mod)
# Missing data ####
describe(usAir_mod)
usAir_mod[complete.cases(usAir_mod),] # removes samples with missing data
# Imputation ####
# Calculate the mean of each variable (column) with the NA removed:
meanz <- colMeans(usAir_mod,na.rm=T) #`na.rm=T` removes NAs
# To replace your NAs with the means you just calculated use the following function:
naFunc <- function(column) { 
  column[is.na(column)]  = round(mean(column, na.rm = TRUE),2)
  return(column) 
}
# “apply” it to the usair_mod data set
Impute <- apply(usAir_mod,2,naFunc)
# Multivariate Normal Distribution ####
mshapiro.test(t(usAir)) # Shapiro-Wilk 
mvn(usAir, mvnTest = "mardia") # Mardia test
# Data transformation ####
usAir$SO2 
usAir[,1] 
# Next you can simply wrap either of those commands in the histogram function:
hist(usAir$SO2) 
hist(usAir[,1]) 

library(tidyverse)
norm_hist <- ggplot(gather(usAir), aes(value)) + 
  					 geom_histogram(bins = 10) + 
  					 facet_wrap(~key, scales = 'free_x',ncol = 1)

library(Hmisc)
hist.data.frame(usAir)

for (i in 1:length(names(usAir))){
  hist(usAir[i], main='hist', breaks=20, prob=TRUE)
  par(3,3)
}

usAirlog <- log1p(usAir)
log_hist <- ggplot(gather(usAirlog), aes(value)) + 
    				geom_histogram(bins = 10) + 
    				facet_wrap(~key, scales = 'free_x', ncol = 1)

library(ggpubr)
combined <- ggarrange(norm_hist, log_hist, labels = c("Normal", "Log"))

par(mfrow=c(1,2))
hist(usAir[,1])  
hist(usAirlog[,1])  

# Square root transformation: b_ij=√(x_ij ) 
usAirsqrt <- sqrt(usAir)
hist(usAirsqrt$SO2)
hist(usAirsqrt[,1]) 

par(mfrow=c(1,2))
hist(usAir[,1])  
hist(usAirsqrt[,1]) 

newData <- runif(100, 0, 1)
asin(sqrt(newData))

par(mfrow=c(1,2))
hist(newData)
hist(asin(sqrt(newData)))
# Data standardization ####
cSums <- colSums(usAir)
Sdev <- sd(cSums)
M <- mean(cSums)

Cv <- Sdev/M*100
# Z standardization ####
scaledData <- scale(usAir)
scaledData_tibble <- as_tibble(scale(usAir))

par(mfrow=c(1,2))
hist(usAir[,1], main=colnames(usAir)[1], xlab=" ")
hist(scaledData[,1], main=colnames(usAir)[1], xlab=" ")

scaledData_tibble_plot <- ggplot(gather(scaledData_tibble), aes(value)) + 
  					 	geom_histogram(bins = 10) + 
  					 	facet_wrap(~key, scales = 'free_x',ncol = 1)

norm_hist <- ggplot(gather(usAir), aes(value)) + 
  					 geom_histogram(bins = 10) + 
  					 facet_wrap(~key, scales = 'free_x',ncol = 1)

combined <- ggarrange(scaledData_tibble_plot, norm_hist, labels = c("Scaled", "Norm"))

cSums <- colSums(scaledData_mean)
install.packages("matrixStats")
library("matrixStats")
colSds(scaledData_mean)

summary(scaledData_mean)

# Detecting Outliers ####
scaledData <- scale(usAir)

par(mfrow=c(2,4))
hist(scaledData [,1] ,main=colnames(usAir)[1],xlab=" ")
hist(scaledData [,2] ,main=colnames(usAir)[2],xlab=" ")  
hist(scaledData [,3] ,main=colnames(usAir)[3],xlab=" ")  
hist(scaledData [,4] ,main=colnames(usAir)[4],xlab=" ")
hist(scaledData [,5] ,main=colnames(usAir)[5],xlab=" ")  
hist(scaledData [,6] ,main=colnames(usAir)[6],xlab=" ")  
hist(scaledData [,7] ,main=colnames(usAir)[7],xlab=" ")

par(mfrow=c(2,4))
mapply(hist,as.data.frame(usAir),main=colnames(usAir),xlab=" ")

out <- function(x){
	lier<-x[abs(x)>3]
	return(lier)
	}

apply(scaledData,2,out)
# Distance and Dissimilarity ####
scaledData <- scale(usAir)
eucDist <- vegdist(scaledData,"euclidean")
hist(eucDist)

Fruit <- rbind(c(1,0,1,1),c(2,1,0,0), c(3,0,4,4))
colnames(Fruit) <- c("Farm","Strawaberry","Peach", "Rasberry")
eucDist <- vegdist(Fruit[,-1], "euclidean")

dev.off()
cbDist <- vegdist(scaledData,"manhattan")
hist(cbDist)

brayDist <- vegdist(usAir,"bray")
hist(brayDist)

brayFruit <- vegdist(Fruit[,-1], "bray")
brayFruit

daisy(usAir_mod, metric =  "gower")

brayDist <- vegdist(usAir,"bray")
multOut <- scale(colMeans(as.matrix(brayDist)))
hist(multOut)

multOut[multOut >3,]

colBray <- colMeans(as.matrix(brayDist))
mBray <- mean(colBray)
stdBray <- sd(colBray)
threeSD <- stdBray * 3 + mBray
hist(colBray)

colBray[colBray >threeSD]
