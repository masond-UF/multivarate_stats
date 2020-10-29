# 29 October 2020—Lab 9
# David Mason
# Set-up workspace ####

library(MASS)
library(rpart)
library(ade4)
library(vegan)
library(rattle) 

iris
ozone <- read.csv("Lab 9/Data/ozone.csv")

# CART ####

set.seed(51)
train <- sample(1:150, 75)

freq <- table(iris$Sp[train])
freq 

model <- Species ~ .

# Running the CART algorithm ####
?rpart

iris_rpart <- rpart(model, data = iris[train,], 
										method="class", control = rpart.control(minsplit = 10))

# Look up what rpart.control and it’s parameters do and play with them!

# Plotting Cart Tree and viewing summary ####

?post.rpart
post(iris_rpart, file = "", title = "Iris Classification Tree")
fancyRpartPlot(iris_rpart, sub= " " )   

summary(iris_rpart)
# Cost-complexity pruning ####

printcp(iris_rpart)
plotcp(iris_rpart)

# Pruning the tree 
?prune.rpart

cp <- printcp(iris_rpart)[3,1]

iris_prune <- prune(iris_rpart, cp = cp)
print(iris_prune)

# And plot the pruned tree:
post(iris_prune, file = "", title = " Iris pruned tree")

# Classification accuracy ####
Ct_unprune <- table(predict(iris_rpart, iris[-train,], type = "class"), iris[-train, "Species"])
Ct_prune <- table(predict(iris_prune, iris[-train,], type = "class"), iris[-train, "Species"])

class_unprune<-sum(diag(prop.table(Ct_unprune)))
class_prune<-sum(diag(prop.table(Ct_prune)))

class_unprune
class_prune

# Regression tree ####
train <- sample(1:110, 60)
model <- ozone ~ .

oz_rpart <- rpart(model, data = ozone[train,],method="anova", 
									control = rpart.control(minsplit = 10))
printcp(oz_rpart)

oz_prune <- prune(oz_rpart, cp = cp)

print(oz_prune)
summary(oz_prune)

post(oz_prune, file = "", title = "Ozone Pruned Tree")
sum(residuals(oz_prune)^2)

fancyRpartPlot(oz_prune, sub= " " ) 

pruned<-predict(oz_prune, oz[-train,],type = "vector")
full<-predict(oz_rpart, oz[-train,],type = "vector")
