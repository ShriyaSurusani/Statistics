

##Question 2
fitness <- read.csv(file = "C:/Users/sshri/Desktop/Fall 2021/Stat 8030/fitness_walking.csv",header=TRUE)
head(fitness)

#2.1
Model <- lm(y~x1+x2+x3+x4,data = fitness)

Value <- summary(Model)$r.squared
Value

#2.2
x1_S <- I(fitness$x1^2)
x2_S <- I(fitness$x2^2)
x3_S <- I(fitness$x3^2)
x4_S <- I(fitness$x4^2)

Quad_Model  <- lm(y~x1+x2+x3+x4+x1_S+x2_S+x3_S+x4_S,data=fitness)
Quad_Model <- summary(Quad_Model)

Quad_Model$r.squared

#2.3
Quad_Model  <- lm(y~x1+x2+x3+x4+x1_S+x2_S+x3_S+x4_S,data=fitness)
anova(Model, Quad_Model)

#2.4
plot(fitted(Model),residuals(Model))
abline(h=0,lty=2)



##Question 3
mercury <- read.csv(file = "C:/Users/sshri/Desktop/Fall 2021/Stat 8030/HomeWork/mercury.csv",header=TRUE)
head(mercury)

#3.1
plot(mercury$X.SoilMerCon.,mercury$X.PlantMerCon.,col = 'red')

library(ggplot2)
Mplot <- ggplot(mercury, aes(X.SoilMerCon., X.PlantMerCon., colour = X.Crop. )) +
  geom_point() + geom_smooth(method=lm) + xlab("Soil_Mercury_Content") + ylab("Plant_Mercury_Content")
Mplot

#3.3
Model <- lm(X.PlantMerCon.~X.SoilMerCon.,data = mercury)
summary(Model)

#3.4
Interaction <-lm(X.PlantMerCon.~X.SoilMerCon.* X.Crop., data = mercury)
summary(Interaction)

#3.7
Quad_model <- lm(X.PlantMerCon.~X.SoilMerCon.+I(X.SoilMerCon.^2)+X.SoilMerCon.*X.Crop., data = mercury)
summary(Quad_model)

#3.8
anova(Interaction,Quad_model)



##Question 4

#4.1
library(alr4)
head(BigMac2003)

BigMacData <- lm(BigMac~FoodIndex, data = BigMac2003)
summary(BigMacData)

#Scatterplot
plot(BigMac2003$FoodIndex,BigMac2003$BigMac,xlab ="FoodIndex" ,ylab ="BigMac",main = "ScatterPlot for Bigmac VS FoodIndex" )

#Resedual vs Fitted etc
plot(BigMacData)

#4.2 Tranformation with 1 predector 
L <- c(1/2, 0, -1/2, -1, -2)

par(mfrow=c(2,3))

plot(BigMac2003$FoodIndex,I((BigMac2003$BigMac^L[1]-1)/L[1]) )
plot(BigMac2003$FoodIndex,log(BigMac2003$BigMac) )
plot(BigMac2003$FoodIndex,I((BigMac2003$BigMac^L[3]-1)/L[3]) )
plot(BigMac2003$FoodIndex,I((BigMac2003$BigMac^L[4]-1)/L[4]) )
plot(BigMac2003$FoodIndex,I((BigMac2003$BigMac^L[5]-1)/L[5]) )


#4.3
library(MASS)
boxcox(BigMacData)

R <- boxcox(BigMacData)
Lambda <- R$x[which.max(R$y)]

#4.4

LModel <- lm(I((BigMac2003$BigMac^Lambda - 1)/Lambda)~FoodIndex, data = BigMac2003)
summary(LModel)

#Q-Q Plot
qqnorm(residuals(LModel))

