library("alr4")
library("ggplot2")
library("car")

salaries <- read.csv(path="Downloads/Salaries.csv",header=TRUE)
#Salaries <- read_csv("Downloads/Salaries.csv", show_col_types = FALSE)
Salaries

head(Salaries)


aggregate(Salaries$salary, list(Salaries$gender), FUN=mean)
boxplot(salary~gender,data=Salaries, main="Faculty Salaries",
        xlab="Gender", ylab="Salary")
#--------------------------------------------------------------
aggregate(Salaries$salary, list(Salaries$discipline), FUN=mean)
boxplot(salary~discipline,data=Salaries, main="Faculty Salaries",
        xlab="Discipline", ylab="Salaries")
#--------------------------------------------------------------
aggregate(Salaries$salary, list(Salaries$rank), FUN=mean)
boxplot(salary~rank,data=Salaries, main="Faculty Salaries",
        xlab="Rank of the faculty", ylab="Salaries")

sal_vs_d_r <- ggplot(Salaries) +
  geom_boxplot(aes(rank,salary, colour=gender)) + 
  geom_smooth(aes(rank,salary, colour=gender), method=lm, se=FALSE) +
  labs(x = "Seniority", y = "Salary",
       title = "Faculty Salaries")
sal_vs_d_r
#--------------------------------------------------------------
names(Salaries)
lm.Salaries <- lm(salary~., data = Salaries)
summary(lm.Salaries)
lm.Salaries1 <- lm(salary~gender, data=Salaries)
summary(lm.Salaries1)
lm.Salaries2 <- lm(salary~rank, data=Salaries)
summary(lm.Salaries2)
lm.Salaries3 <- lm(salary~discipline, data=Salaries)
summary(lm.Salaries3)

sal_gen <- t.test(salary ~ gender, data = Salaries)
sal_gen
sal_dis <- t.test(salary ~ discipline, data = Salaries)
sal_dis



#Let's change the data types of columns to numeric
# For Gender, 1-> Female, 2-> Male
class(Salaries$gender) = "Numeric"
# For rank, 
class(Salaries$rank) = "Numeric"
# For Discipline
class(Salaries$discipline) = "Character"
class(Salaries$yrs.since.phd) = "Numeric"
class(Salaries$yrs.service) = "Numeric"
# For Salary
class(Salaries$salary) = "Numeric"
head(Salaries)
lm.Salaries <- lm(salary~., data = Salaries)
summary(lm.Salaries)
cols <- c(1,2,5,6)
cor(Salaries[,cols])
pairs(Salaries[,cols])

Salaries$discipline <- recode_factor(Salaries$discipline, "B" = 1, "A"= 0)





vif(lm.Salaries)
cov2cor(vcov(lm.Salaries))