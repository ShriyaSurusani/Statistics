
##2.1

data("Htwt")
Htwt

#2.1.1
# Scatterplot
plot(Htwt$ht, Htwt$wt, xlab="Height", ylab="Weight")

#2.1.2
# Calculating mean of x i.e., Height
mean_of_height <- mean(Htwt$ht)
mean_of_height

# Calculating the mean of y i.e., Weight
mean_of_weight <- mean(Htwt$wt)
mean_of_weight

# Calculating SXX
sxx <- sum(Htwt$ht^2)-sum(Htwt$ht)^2/10
sxx

# Calculating SYY
syy <- sum(Htwt$wt^2)-sum(Htwt$wt)^2/10
syy

# Calculating SXY
sxy <- sum(Htwt$ht*Htwt$wt)-(sum(Htwt$ht)*sum(Htwt$wt))/10
sxy

# Calculating Slope by considering slope as b1
b1 <- sxy/sxx
b1

# Calculating the intercept by considering intercept as b0
b0 <- mean_of_weight - (b1 * mean_of_height)
b0

# Drawing fitted line on the scatterplot
abline(a=b0,b=b1,col='orange')

# 2.1.3
# Calculating the estimate of variance
RSS <- syy- (sxy^2/sxx)
var_sigma <- RSS/8
var_sigma

# Calculating estimated standard errors
se_b0 <- sqrt(var_sigma * ((1/10)+(mean_of_height^2)/sxx))
se_b0

se_b1 <- sqrt(var_sigma/sxx)
se_b1

# Calculating estimated covariance between b0 and b1
cov_b0_b1 <- -var_sigma * (mean_of_height/sxx)
cov_b0_b1

# Computing t-tests
t_0 = b0/se_b0
t_0
t_1 = b1/se_b1
t_1

# Calculating p values for the two sided tests
2 * pt(-abs(t_0),8)
2 * pt(-abs(t_1),8)


# Cross checking the results with the lm function
m <- lm(ht~wt, data=Htwt)
summary(m)

##2.2.4
data("UBSprices")
UBSprices
head(UBSprices)

LMP <- lm(rice2009 ~ rice2003, data = UBSprices)
LMP

abline(LMP, col='black')


plot(fitted(LMP), residuals(LMP), xlab = "FV", ylab = "RV") #Resedual Plot
abline(0,0, col="orange")



##2.6
data("ftcollinstemp")
ftcollinstemp

#2.6.1
plot(ftcollinstemp$fall, ftcollinstemp$winter, xlab="Fall", ylab="Winter")

#2.6.2
lmtemp <- lm(winter ~ fall, data=ftcollinstemp)
lmtemp
abline(lmtemp, col = "black")

summary(lmtemp)

beta0 <- 13.7834
beta1 <- 0.3132

s0 <- 7.5549
s1 <- 0.1528

t0 <- beta0/s0
t0
t1 <- beta1/s1
t1

2 * pt(abs(t0),109, lower.tail = FALSE) #P values
2 * pt(abs(t1),109, lower.tail = FALSE)

#2.6.4

temperature <- subset(ftcollinstemp, (year >= 1900) & (year <= 1989)) #1900 to 1989
temperature
plot(temperature$fall, temperature$winter, xlab = "Fall",ylab = "Spring")

temperature2 <- subset(ftcollinstemp, (year >= 1990) & (year <= 2010)) #1990 to 2010
temperature2
plot(temperature2$fall, temperature2$winter, xlab = "Fall",ylab = "Spring")


##2.14

data("Heights")
Heights

#2.14.1
n <- 458 #1/3 of the rows making it the validation set
vrows <- sample(1:nrow(Heights),n,replace=F)
vset <- Heights[vrows,]
cset <- Heights[-vrows,]

#2.14.2

lmHeights <- lm(mheight~dheight, data = cset)
cp <- predict(lmHeights, newdata = data.frame(cset["dheight"])) #Prediction calculation

ASR <- mean((cset$mheight-cp)^2) #Average squre residual
ASR

APE <- sqrt(ASR) #Average prediction error
APE

#2.14.3

vp <- predict(lmHeights, newdata = data.frame(vset["dheight"])) #Prediction calculation

ASRV <- mean((vset$mheight-vp)^2) #Average square residual
ASRV

APEV <- sqrt(ASRV) #Average prediction error
APEV



##2.15

data("wblake")
wblake

m <- lm( Length ~ Age, data=wblake )

plot(wblake$Age, wblake$Length, xlab="Age", ylab="Length" )
abline(m)

#2.15.1
predict(m,data.frame(Age=c(2,4,6)), interval="confidence", level=0.95)

#2.15.2
predict(m,data.frame(Age=c(9)), interval="confidence", level=0.95)


##2.16


data("UN11")
UN11

# Scatterplot
plot(UN11$fertility, UN11$ppgdp, xlab="Fertility", ylab="ppgdp")

#2.16.1
Lppgdb     = log(UN11$ppgdp,base=10)
Lfertility = log(UN11$fertility,base=10)

plot( Lppgdb, Lfertility )

#2.16.2
lm_un <- lm( LFertility ~ LPPgdb, data=UN11 ) 
abline( lm_un ) 

#2.16.3
summary(lm_un)

NOR <- 199 #Number of rows

sbeta1 <- -0.20715

sesbeta1 <- 0.01401 #Standard error

tt <- sbeta1/sesbeta1 #T-test
tt

pv <- pt(-abs(tt), df= NOR-2, lower.tail = T) #P-value
pv

#2.16.4
summary(lm_un)

