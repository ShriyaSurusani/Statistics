library(MASS)
data("Boston")
head(Boston)
?Boston
pairs(Boston)
#Models
model1 <- lm(medv ~ . , data =Boston)
summary(model1)
model2 <- lm(medv ~ lstat:rm, data =Boston)
summary(model2)
model3 <- lm(medv~indus+rm+nox+ptratio+lstat, data = Boston)
summary(model3)
model4 <- lm(medv~lstat+I(rm^2)+I(lstat^2)+chas+ptratio+dis, data = Boston)
summary(model4)
model5 <- lm(medv ~ lstat + I(rm^2)+ I(lstat^2) + rm, data =Boston)
summary(model5)
#Calculating RSS
RSSmodel1 <- sum((Boston$medv - fitted(model1))^2)
RSSmodel2 <- sum((Boston$medv - fitted(model2))^2)
RSSmodel3 <- sum((Boston$medv - fitted(model3))^2)
RSSmodel4 <- sum((Boston$medv - fitted(model4))^2)
RSSmodel5 <- sum((Boston$medv - fitted(model5))^2)
#Caculating AIC and BIC
model_1 <- 13
model_2 <- 2
model_3 <- 5
model_4 <- 6
model_5 <- 4
n <- nrow(Boston)
AICmodel1 <- n*log(RSSmodel1/n)+2*model_1
AICmodel2 <- n*log(RSSmodel2/n)+2*model_2
AICmodel3 <- n*log(RSSmodel3/n)+2*model_3
AICmodel4 <- n*log(RSSmodel4/n)+2*model_4
AICmodel5 <- n*log(RSSmodel5/n)+2*model_5
c(AICmodel1,AICmodel2,AICmodel3,AICmodel4,AICmodel5)
BICmodel1 <- n*log(RSSmodel1/n)+log(n)*model_1
BICmodel2 <- n*log(RSSmodel2/n)+log(n)*model_2
BICmodel3 <- n*log(RSSmodel3/n)+log(n)*model_3
BICmodel4 <- n*log(RSSmodel4/n)+log(n)*model_4
BICmodel5 <- n*log(RSSmodel5/n)+log(n)*model_5
c(BICmodel1, BICmodel2, BICmodel3, BICmodel4,BICmodel5)
#Cross-validation: Leave one out cross validation error
y <- Boston$medv
n <- nrow(Boston)
hvec1 <- hatvalues(model1)
hvec2 <- hatvalues(model2)
hvec3 <- hatvalues(model3)
hvec4 <- hatvalues(model4)
hvec5 <- hatvalues(model5)
mse1 <- mean ( ((y-fitted(model1))/(1-hvec1))^2 )
mse2 <- mean ( ((y-fitted(model2))/(1-hvec2))^2 )
mse3 <- mean ( ((y-fitted(model3))/(1-hvec3))^2 )
mse4 <- mean ( ((y-fitted(model4))/(1-hvec4))^2 )
mse5 <- mean ( ((y-fitted(model5))/(1-hvec5))^2 )
c(mse1, mse2, mse3, mse4,mse5)
plot(c(13,2,5,6,4),
     c(sqrt(mse1),sqrt(mse2),sqrt(mse3),sqrt(mse4),sqrt(mse5)),ylab='???MSE',xlab='Number of Regressors')
#Residual Plot and Q-Q Plot
plot(model4)
#collinearity
vif(model4)
#Box-Cox transformation
r <- boxcox(model4)
t <- r$x[which.max(r$y)]
t
#Fitting Box-Cox model
F <- lm(I((Boston$medv^t-1)/t)~lstat+I(rm^2)+I(lstat^2)+chas+ptratio+dis,data=Boston )
summary(F)
plot(F)




## Leave one out cross validation error
y <- galapagos$NS
n <- nrow(galapagos) # not needed

hvec.lm1 <- hatvalues(model_c)


mse.cv.lm1 <- mean ( ((y-fitted(model_c))/(1-hvec.lm1))^2 ) #Mean square error value

