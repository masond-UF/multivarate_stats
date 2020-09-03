# 3 September 2020
# Multivariate Statistics Lab 1

1 + 1
help.search("data input")
find("anova")
example(lm)
demo(graphics)

install.packages("MVA")
library(MVA)

usAir <- USairpollution
usAir

usAir_mod <- read.csv("Lab 1/Data/usAir_mod.csv", row = 1, header=TRUE)
usAir_mod

dim(usAir)
str(usAir)

usAir[1,1] # first row and column
usAir[1,] # first row 
usAir[,1] # first column
usAir$SO2 # extract by column name
usAir[-1,] # drop the first row
usAir[,-1] # drop the first column
usAir[1:5,] # extract multiple rows
usAir[,1:5] # extract multiple columns
usAir[-(1:5),] # drop multiple rows
usAir[,-(1:5)] # drop multiple columns
usAir[usAir[,2]<50,] # using logical operators

t(usAir) # transpose 
matrix_usAir <- as.matrix(usAir$SO2)
t(matrix_usAir)

temp<-usAir$temp

ranks <- rank(temp)
sorted <- sort(temp)
ordered <- order(temp)

table <- data.frame(temp,ranks,sorted,ordered)
des_sort <- rev(sort(temp))
temp_ordered <- usAir[order(temp),]

mean(usAir[,3])
median(usAir [,3])
var(usAir [,3])
sum(usAir [,3])

mean(t(usAir [3,]))
median(t(usAir [3,]))
var(t(usAir [3,]))
sum(t(usAir [3,]))

columnSum <- colSums(usAir)
rowSums(usAir)
colMeans(usAir)
rowMeans(usAir)

write.csv(columnSum, "Lab 1/Output/columnSum.csv")
