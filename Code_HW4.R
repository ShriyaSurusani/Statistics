library(alr4)


#1.1 (7.7.1)

data(galtonpeas)
head(galtonpeas)
attach(galtonpeas)

#scatterplot
plot(Parent, Progeny, main="Progeny Vs Parent",
     xlab="Parent", ylab="Progeny ", pch=15, col = "Green")


#1.2 (7.7.2)
Elm <- lm(Progeny~Parent, data = galtonpeas)
Elm

#Computing weighted regression
ElmWeighted <- lm(Progeny~Parent, weights = 1/SD^2, data = galtonpeas)
ElmWeighted
summary(ElmWeighted)

#scatter plot for fitting regression
abline(ElmWeighted, col='Red')


#2.1
data(salarygov)
head(salarygov)
names(salarygov)

attach(salarygov)
plot(Score, MaxSalary, main="Salary Vs Score",
     xlab="Score", ylab="MaxSalary ", pch=18, col = 'Green')


# 2.2
N_lm <- lm(MaxSalary~Score, data = salarygov)
N_lm
summary(N_lm)
points(salarygov$Score,fitted(N_lm),col = 'Red', pch=15)

lm_2 <- lm(MaxSalary~Score+I(Score^2), data = salarygov)
lm_2
summary(lm_2)
points(salarygov$Score,fitted(lm_2),col = 'Black', pch=15)


lm_3 <- lm(MaxSalary~Score+I(Score^2)+I(Score^3), data = salarygov)
lm_3
summary(lm_3)
points(salarygov$Score,fitted(lm_3),col = 'brown', pch=15)


lm_5 <- lm(MaxSalary~Score+I(Score^2)+I(Score^3)+I(Score^4)+I(Score^5), data = salarygov)
lm_5
summary(lm_5)
points(salarygov$Score,fitted(lm_5),col = 'blue', pch=15)



#2.3

anova(N_lm,lm_2)

anova(lm_2,lm_3)

anova(lm_2,lm_5)

summary(N_lm)$r.squared 
summary(lm_2)$r.squared
summary(lm_3)$r.squared
summary(lm_5)$r.squared

lm_5

lines(smooth.spline(salarygov$Score, predict(lm_5)), col="Red", lwd=4)

#2.4

AW = salarygov$NE

#2.5

MW <- lm(MaxSalary~Score, data = salarygov, weights = AW)
summary(MW)


N_lm <- lm(MaxSalary~Score, data = salarygov)
N_lm
summary(N_lm)