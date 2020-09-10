# 3 September 2020
# Multivariate Statistics Lab 1

# Work through
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

# Pitcher Plant

pitcher <- read.csv("Lab 1/Data/Pitcher.csv")
dim(pitcher)
str(pitcher)

pitcher[1,1] # first row and column
pitcher[1,] # first row 
pitcher[,1] # first column
pitcher$MOSQUI # extract by column name
pitcher[-1,] # drop the first row
pitcher[,-1] # drop the first column
pitcher[1:5,] # extract multiple rows
pitcher[,1:5] # extract multiple columns
pitcher[-(1:5),] # drop multiple rows
pitcher[,-(1:5)] # drop multiple columns
pitcher[pitcher[,2]<50,] # using logical operators

t(pitcher) # transpose 
matrix_pitcher <- as.matrix(pitcher$MOSQUI)
t(matrix_pitcher)

mosqui <- pitcher$MOSQUI

ranks <- rank(mosqui)
sorted <- sort(mosqui)
ordered <- order(mosqui)

table <- data.frame(mosqui,ranks,sorted,ordered)
des_sort <- rev(sort(mosqui))
mosqui_ordered <- pitcher[order(mosqui),]

mean(matrix_pitcher[,1])
median(matrix_pitcher[,1])
var(matrix_pitcher[,1])
sum(matrix_pitcher[,1])

mean(t(matrix_pitcher[1,]))
median(t(matrix_pitcher[1,]))
var(t(matrix_pitcher[1,]))
sum(t(matrix_pitcher[1,]))

columnSum_pitcher <- colSums(pitcher[,2:14])
rowSums(pitcher[,2:14])
colMeans(pitcher[,2:14])
rowMeans(pitcher[,2:14])

write.csv(columnSum_pitcher, "Lab 1/Output/columnSum_pitcher.csv")

