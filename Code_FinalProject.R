
#Question 1

library(lme4)
library(car)
library(ggplot2)
Time <- read.csv(file = "C:/Users/sshri/Desktop/Fall 2021/Stat 8030/reaction_times.csv",header=TRUE)
head(Time)

Time$subject <- as.factor(Time$subject)
Time$group <- as.factor(Time$group)

summary(Time)

ggplot(Time, aes(y = RT, x=group, col=group)) + geom_point() + ggtitle("Scatterplot") + scale_color_manual(values = c('red', 'orange'))


model_1 <- lmer(RT~ group +(1|subject), data = Time, REML = FALSE)
summary(model_1)

model_2 <- lmer(RT~ (1|subject), data = Time, REML = FALSE)
summary(model_2)

anova(model_1, model_2)

plot(model_1)
qqnorm(residuals(model_1))
qqline(residuals(model_1))

hatvalues(model_1)
cooksD <- cooks.distance(model_1)

influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

plot(residuals(model_1), hatvalues(model_1))
plot(residuals(model_1), cooks.distance(model_1))


#Question 2

AbaloneData <- read.csv(file = "C:/Users/sshri/Desktop/Fall 2021/Stat 8030/abalone.csv",header=TRUE)
head(AbaloneData)


library(corrplot)
AbaloneData <- AbaloneData[-c(1)]
pairs(AbaloneData)

OriginalModel <- lm(Rings ~ . , data =AbaloneData)
summary(OriginalModel)

model <- lm(Rings~Length+Diameter+Shell.weight+Shucked.weight,data = AbaloneData)
summary(model)

plot(fitted(model), resid(model), xlab='Fitted Values', ylab='Residuals')

library(lmtest)
bptest(model)

#Weighted
w <- AbaloneData$Height
w_model <- lm(log(Rings)~log(Diameter)+Length+Shell.weight+Shucked.weight,data=AbaloneData, weights=w)
summary(w_model)


#AIC and BIC
AIC(model)
AIC(w_model)

BIC(model)
BIC(w_model)


#Quadratic
w_model_quad <- lm(log(Rings)~log(Diameter)+poly(Length,2)+Shell.weight+poly(Shucked.weight,2),data=AbaloneData, weights=w)
summary(w_model_quad)

anova(w_model_quad,w_model)

plot(w_model_quad)
