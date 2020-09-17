# 17 September 2020
# Multivariate Statistics Lab 3
# David Mason
# Set up R session ####
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
# A primer of matrix algebra ####
newMatrix<- matrix(c(1,4,5,4,5,6,9,1,9),nrow=3, ncol=3) # numbers row-wise 
newMatrix
dim(newMatrix)
# Matrix addition and subtraction ####
oneMatrix <- matrix(c(5,2,7,2,6,3,2,6,3), nrow = 3, ncol = 3)
newMatrix + oneMatrix
newMatrix - oneMatrix
# Scalar Multiplication #####
3*newMatrix
# Matrix Multiplication ####
oneMatrix%*%newMatrix
newMatrix%*%oneMatrix
# Matrix transposition
transMatrix <- t(newMatrix)
# Identity Matrices ####
Identity <- diag(3)
# Matrix Inversion ####
invMatrix <- solve(newMatrix)
invMatrix%*%newMatrix
round(invMatrix%*%newMatrix,10)
# Eigenvalues and eigenvectors ####
eig <- eigen(newMatrix)
# Transform variables (PCA) ####
usAir <- USairpollution
norm_hist <- ggplot(gather(usAir), aes(value)) + 
  					 geom_histogram(bins = 10) + 
  					 facet_wrap(~key, scales = 'free_x',ncol = 1)
usAir_t <- usAir %>%
            rownames_to_column('city') %>%
            mutate(log_SO2 = log(SO2), log_manu = log(manu), log_popul = log(popul)) %>%
            select(city, temp, wind:log_popul) %>%
            column_to_rownames('city')
# Principal Component Analysis (PCA) ####
mshapiro.test(t(usAir_t))
mvn(usAir_t, mvnTest = "mardia")
ZusAir <- scale(usAir_t)

usAir_pca <- princomp(ZusAir, cor = F)
usAir_pca <- princomp(usAir_t, cor = T)
summary(usAir_pca)

eigenVal <- (usAir_pca$sdev*sqrt(41/40))^2

propVar <- eigenVal/sum(eigenVal)
cumVar <- cumsum(propVar)
pca_Table <- t(rbind(eigenVal,propVar,cumVar))
pca_Table

loadings(usAir_pca)
scores(usAir_pca)

# How many PC Axes to keep? ####
plot(usAir_pca, type="lines")
summary(usAir_pca)
# Significance of factor loading ####
