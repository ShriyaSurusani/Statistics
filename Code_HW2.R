## Solutions


## 3.3
# 3.3.1

data(BGSgirls )
head(BGSgirls)
D = BGSgirls[,c(2,4,1,3,6,11)]

# Plot
pairs(D)

# Correlation 
cor(D)

# 3.3.2

# Marginal Plots 
scatterplotMatrix(~BMI18 + WT9 + ST9, BGSgirls,spread=FALSE, diagonal="none", col='purple')

# Added-Variable Plot
FR <- residuals(lm(BMI18 ~ WT9, BGSgirls))
SR <- residuals(lm(ST9 ~ WT9, BGSgirls))
M <- lm(FR ~ SR)
plot(FR ~ SR, xlab=expression(paste(hat(e), " from ST9 on WT9")),ylab=expression(paste(hat(e), " from BMI18 on WT9")))
grid(col="blue", lty="solid")
abline(M)


# 3.3.3

M2=lm(BMI18~HT2+HT9+WT2+WT9+ST9,data=D)
summary(M2)


# 4.2.1

data("Transact")
p <- (Transact$t1+Transact$t2)/2
q <- (Transact$t1-Transact$t2)

M_1 <-lm(time~t1+t2,data = Transact)
M_2 <-lm(time~a+d,data = Transact)
M_3 <-lm(time~t2+d,data = Transact)
M_4 <-lm(time~t1+t2+a+d,data = Transact)

summary(M_1)
summary(M_2)
summary(M_3)
summary(M_4)

