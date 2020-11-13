# 12 November 2020—Lab 11
# David Mason
# Set-up workspace ####
library(vegan)
spe <- read.csv("Lab 11/Data/DoubsSpe.csv", row.names=1)
env <- read.csv("Lab 11/Data/DoubsEnv.csv", row.names=1)

# Remove row with no species
spe <- spe[-8, ]
env <- env[-8, ]

# Remove das
env <- env[, -1]

# Recode the slope variable (pen) into a factor (qualitative) variable
pen2 <- rep("very_steep", nrow(env))
pen2[env$pen <= quantile(env$pen)[4]] <- "steep"
pen2[env$pen <= quantile(env$pen)[3]] <- "moderate"
pen2[env$pen <= quantile(env$pen)[2]] <- "low"
pen2 <- factor(pen2, levels=c("low", "moderate", "steep", "very_steep"))
table(pen2)

# Incorporate the new qualitative slope variable “pen2” into the 
# new environmental data frame:
env2 <- env
env2$pen <- pen2

# Data transformation ####
spe.hel <- decostand(spe, "hellinger")
# RDA using all the environmental variables ####
spe.rda <- rda(spe.hel ~ ., env2)

summary(spe.rda)

R2 <- RsquareAdj(spe.rda)$r.squared
R2adj <- RsquareAdj(spe.rda)$adj.r.squared

plot(spe.rda, scaling=1, main="Triplot RDA spe.hel ~ env2 - scaling 1 - wa scores")

spe.sc <- scores(spe.rda, choices=1:2, scaling=1, display="sp") 
	arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

plot(spe.rda, scaling=1, display=c("sp", "lc", "cn"), 
		 main="Triplot RDA spe.hel ~ env2 - scaling 1 - lc scores")
arrows(0, 0, spe.sc[, 1], spe.sc[, 2], length=0, lty=1, col="red")

anova(spe.rda, step=1000)
anova(spe.rda, by="axis", step=1000)

# Variable Selection: forward selection using ordiR2step in vegan ####
step.forward <- ordiR2step(rda(spe.hel ~ 1, data=env2), scope=formula(spe.rda), 
													 R2scope = F, direction="forward", pstep=1000)

spe.subset.rda <- rda(spe.hel ~ alt + oxy + dur + dbo, data = env2)
summary(spe.subset.rda)
summary(spe.rda)
# Partial RDA ####
partial.alt <- rda(spe.hel ~ alt + Condition(oxy + dbo + dur) , data=env2)

plot(spe.part, digits=2)
plot(spe.part, digits = 2, Xnames = c('alt', 'oxy', 'dur', 'dbo'), bg = 2:5)
# spe.hel ~ alt + oxy + dbo + dur 
# Variance partitioning #####
spe.part <- varpart(spe.hel,~ alt, ~oxy,~ dur ,~dbo ,data=env2)
spe.part
# You can also test the significance of the variance ####
# fractions using a permutation test.
rda.alt <- rda(spe ~ alt, data = env2)
rda.alt.oxy <- rda(spe ~ alt + oxy, data = env2)
rda.alt.oxy.dur <- rda(spe ~ alt + oxy + dur, data = env2)
# testing overlap


