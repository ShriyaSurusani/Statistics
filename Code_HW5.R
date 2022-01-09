

#Question 1
LD <- read.csv(file = "C:/Users/sshri/Desktop/Fall 2021/Stat 8030/liver_drug.csv",header=TRUE)
LD

#1.1
lm_LD <- lm(y~x1+x2+x3, data = Exercise1)
lm_LD
summary(lm_LD)

#1.2
reduced <- lm(y~x1+x3, data = Exercise1)
reduced
summary(reduced)


anova(reduced, lm_LD)

#1.3
L <- as.data.frame(hatvalues(lm_LD))
L

L[order(-L['hatvalues(lm_LD)']), ]

#Cooks distance
CD <- cooks.distance(lm_LD)
CD

#Plots
plot(lm_LD)

# Plot for Cooks distance values Vs Residuals 
library(olsrr)
ols_plot_cooksd_chart(lm_LD)


#Question 2 

P <- read.csv(file = "C:/Users/sshri/Desktop/Fall 2021/Stat 8030/paint_batches.csv",header=TRUE)
library(lme4)

PR <- lmer(Percentage~(1|Batch),data=P)
summary(PR)

P$Batch <- as.factor(P$Batch)
P.lm <-lm(Percentage~Batch, data = P)

anova(P.lm)

a <- 5


#Estimating variance
S <- anova(P.lm)$`Mean Sq`[2]
SB <- ( anova(P.lm)$`Mean Sq`[1] - anova(P.lm)$`Mean Sq`[2] )/a
SB


#Question 3

FA <- read.csv(file = "C:/Users/sshri/Desktop/Fall 2021/Stat 8030/fire_ants.csv",header=TRUE)
FA$Locations <- as.factor(FA$Locations)
FA$Chemicals <- as.factor(FA$Chemicals)

#3.3
FAC <- lmer(NumberKilled~Chemicals+(1|Locations),data=FA,REML=FALSE)
FAR <- lmer(NumberKilled~(1|Locations),data=FA,REML=FALSE)
anova(FAR, FAC)

#3.4
FAL <- lmer(NumberKilled~Chemicals+(1|Locations),data=FA,REML=FALSE)
FALR <- lm(NumberKilled~Chemicals, data=FA)
anova(FAL, FALR)
