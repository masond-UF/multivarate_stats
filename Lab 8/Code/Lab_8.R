# David Mason
# Set up workspace ####
# Multivariate Statistics Lab 8—Discriminant Analysis: 
library(dplyr)
library(MASS)
library(candisc)
library(ade4)
library(vegan)
library(ggplot2)

# Testing the assumptions of DA ####
iris_hist <- ggplot(gather(iris_no_species), aes(value)) + 
  					 geom_histogram(bins = 10) + 
  					 facet_wrap(~key, scales = 'free_x',ncol = 1)
# Homogeneity of within-group variance-covariance matrices ####
dis <- vegdist(iris[,1:4], "euclidean")
iris_nmds <- metaMDS(dis)

NMDS <- data.frame(NMDS1=iris_nmds$point[,1], 
									 NMDS2=iris_nmds$point[,2],species=iris$Species)

ggplot(NMDS,aes(x=NMDS1,y=NMDS2))+
  geom_point(data=NMDS,aes(x=NMDS1,y=NMDS2, color=species), alpha = 0.5)+
  viridis::scale_fill_viridis() +
  theme_bw()

fligner.test(iris$Sepal.Length, iris$Species)
fligner.test(iris$Sepal.Width, iris$Species)
fligner.test(iris$Petal.Length, iris$Species)
fligner.test(iris$Petal.Width, iris$Species)

# Euclidean distances between samples
dis <- vegdist(iris[,1:4], "euclidean")

# Groups are the three different species
groups <- iris$Species

# Multivariate dispersions
MVdisp <- betadisper(dis, groups)

# Perform parametric test
disp_aov <- anova(MVdisp)

# Tukey's Honest Significant Differences
MVdisp.HSD <- TukeyHSD(MVdisp)
MVdisp.HSD

## Non-parametric test: Permutation test for F
perm_MVdisp <- permutest(MVdisp, permutations = 99, pairwise = TRUE)
perm_MVdisp

# We need to transform the variables
log <- cbind.data.frame(apply(iris[,1:4]+1,2,log),iris$Species)
names(log)[5] <- "Species"

# Re-run the Fligner-Killeen test on the transformed data:
fligner.test(log$Sepal.Length,log$Species)
fligner.test(log$Sepal.Width ,log$Species)
fligner.test(log$Petal.Length ,log$Species)
fligner.test(log$Petal.Width, log$Species)

# Re-run multivariate homogeneity of variance test 
# Euclidean distances between samples
log_dis <- vegdist(log[,1:4], "euclidean")


# groups are the three different species
groups <- iris$Species

## Calculate multivariate dispersions
log_MVdisp <- betadisper(log_dis, groups)


#Perform parametric test
disp_aov<-anova(log_MVdisp)


# Tukey's Honest Significant Differences
MVdisp.HSD <- TukeyHSD(log_MVdisp)
MVdisp.HSD

# non-parametric test: Permutation test for F
perm_MVdisp <- permutest(log_MVdisp, permutations = 99, pairwise = TRUE)
perm_MVdisp

# Multivariate Normality ####
# Filter for each species:
# setosa 
setosa <- dplyr::filter(iris, Species == "setosa")

# untransformed
mshapiro.test(t(setosa[,1:4]))
mvn(setosa[,1:4], mvnTest = "mardia")

# log
mshapiro.test(t(log(setosa[,1:4])))
mvn(log(setosa[,1:4]), mvnTest = "mardia")

# virginica 
virginica <- dplyr::filter(iris, Species == "virginica")

# untransformed
mshapiro.test(t(virginica[,1:4]))
mvn(virginica[,1:4], mvnTest = "mardia")

# log
mshapiro.test(t(log(virginica[,1:4])))
mvn(log(virginica[,1:4]), mvnTest = "mardia")

# versicolor 
versicolor <- dplyr::filter(iris, Species == "versicolor")

# untransformed
mshapiro.test(t(versicolor[,1:4]))
mvn(versicolor[,1:4], mvnTest = "mardia")

# log
mshapiro.test(t(log(versicolor[,1:4])))
mvn(log(versicolor[,1:4]), mvnTest = "mardia")

# Multicolinearity ####
cor(iris[,1:4])
# Outliers ####
# Calculate a withi-group distance matrix:

# setosa
eucDist <- vegdist(iris[1:50,1:4],"euclidean")

# Calculate the average distance of each sample to all
# other samples (i.e. column average) and turn the means in z-scores:
multOut <- scale(colMeans(as.matrix(eucDist)))

# Now look at a histogram of these data to identify samples 
# that are > 3 sd from the mean:
hist(multOut)
# and get the number of those samples:
setosa_out <- multOut [multOut >3,]
setosa_out

# versicolor
eucDist <- vegdist(iris[51:100,1:4],"euclidean")

# Calculate the average distance of each sample to all
# other samples (i.e. column average) and turn the means in z-scores:
multOut <- scale(colMeans(as.matrix(eucDist)))

# Now look at a histogram of these data to identify samples 
# that are > 3 sd from the mean:
hist(multOut)
# and get the number of those samples:
versicolor_out <- multOut [multOut >3,]
versicolor_out

# virginica
eucDist <- vegdist(iris[101:150,1:4],"euclidean")

# Calculate the average distance of each sample to all
# other samples (i.e. column average) and turn the means in z-scores:
multOut <- scale(colMeans(as.matrix(eucDist)))

# Now look at a histogram of these data to identify samples 
# that are > 3 sd from the mean:
hist(multOut)
# and get the number of those samples:
virginica_out <- multOut [multOut >3,]
virginica_out

# Finally, make a vector of the outliers to pull out of the data set later:
Outliers <- c(setosa_out,versicolor_out,virginica_out)
Outliers
# Linearity ####
pairs(iris[,1:4]) # looks good
# Discriminant analysis ####
set.seed(11)
train <- sample(1:150, 75)
prior <- table(iris$Sp[train])

iris.LDA <- lda(Species ~ ., iris, prior = cbind(prior/75), subset = train)
?lda()
# Assessing and interpreting canonical axes ####
iris.LDA.p <- predict(iris.LDA, iris[train, ])
corTest <- lm(iris.LDA.p$x~iris$Sp[train])
summary(corTest)
# Plot canonical scores and axes ####
plot(iris.LDA, xlim = c(-11,11), ylim = c(-6,6))
# Classification accuracy ####
ct <- table(iris[train, ]$Species, predict(iris.LDA, iris[train, ])$class) 

#Change to a table of proportions:
pct <- prop.table(ct)
pct

# Calculate classification rate by summing the diagonal:
sum(diag(pct))

# Interpreting canonical axes (raw coefficients, 
# Standardized weights, and structure coefficients #####
iris.mod <- lm(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ 
							 	Species, data=iris[train, ])
iris.can <- candisc(iris.mod, data=iris[train, ])

iris.can$coeffs.raw
iris.can$coeffs.std
iris.can$structure
iris.LDA$scaling
# Validating canonical axes ####
iris.LDA.new <- predict(iris.LDA , iris[-train, ])
ynew.table <- table(iris[-train,]$Species,iris.LDA.new$class)
ynew.table
sum(diag(prop.table(ynew.table)))
# MANOVA, the other side of the coin of DA ####
Y <- as.matrix(iris[,1:4])
Sp <- factor(iris[,5])

fit <- manova(Y ~ Sp)
summary(fit,test="Wilks")

# Post-hoc tests:
Yset <- as.matrix(iris[1:50,1:4])
Yversi <- as.matrix(iris[51:100,1:4])
Yvirg <- as.matrix(iris[101:150,1:4])
Sp <- factor(iris[,5])

fit1 <- manova(rbind(Yset,Yversi) ~ Sp[1:100])
summary(fit1,test="Hotelling-Lawley")
fit2 <- manova(rbind(Yversi,Yvirg) ~ Sp[51:150])
summary(fit2,test="Hotelling-Lawley")
fit3 <- manova(rbind(Yset,Yvirg) ~ Sp[-c(51:100)])
summary(fit3,test="Hotelling-Lawley")

