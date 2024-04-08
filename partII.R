###########################################
## For Research Project, SOC282, Part II ##
# Hypothesis Testing Section #
#############################

# set working directory
setwd("/Users/tylermsmith/Documents/MSU/2024/Spring/SOC282/research_project/1")

# load libraries
library(tidyverse)
library(psych)
library(gssr)
library(ggplot2)
library(dplyr)

# load gss22 data
load("/Users/tylermsmith/Documents/MSU/2024/Spring/SOC282/r/GSS22.Rdata")

# clean the data further on the basis of 1 common dependent variable (depression) making a new sheet referred to as: gss22a.
gss22a <- gss22 %>% 
  drop_na(mntlhlth)

# chi square
chisq.test(gss22a$depress, gss22a$wrkstat)

# Make data subsets based on workstat
fulltimedata <- gss22a[which(gss22a$wrkstat==1), ]
parttimedata <- gss22a[which(gss22a$wrkstat>=2), ]
othertimedata <- gss22a[which(gss22a$wrkstat>=3), ] ## with job but out of work, due to strike, illness, lay off ##

## two sample t tests of bad mental health days and hours worked, sampled by work status
t1 <- t.test(fulltimedata$mntlhlth, parttimedata$mntlhlth)
t2 <- t.test(fulltimedata$mntlhlth, othertimedata$mntlhlth) 
t3 <- t.test(parttimedata$mntlhlth, othertimedata$mntlhlth)

##ANOVA
a1 <- oneway.test(mntlhlth ~ wrkstat, data = gss22a)
oneway.test(mntlhlth ~ wrkstat + sex, data = gss22a)
oneway.test(mntlhlth ~ wrkstat + race, data = gss22a)

anova(a1)
## regression ##

## regression ##
plot(gss22a$hrs1, gss22a$mntlhlth, main="Days of poor mental health and hours worked.",
     xlab = "work hours", ylab="mental health")
  abline(reg1, col="red")

#Bivariate Regression
reg1 <- lm(mntlhlth ~ hrs1, gss22a)
summary(reg1)

plot(gss22a$hrs1, gss22a$mntlhlth, main="Days of poor mental health and hours worked.",
     xlab = "Work Hours", ylab="Poor Mental Health")
     abline(reg1, col="red")

p1 <- ggplot(gss22a, aes(x = hrs1, y = mntlhlth)) +
  geom_point() +
  geom_smooth(data = reg1, col="red")

print(p1)

####################
#multivariate regression?##

mlm1 <- lm(formula = mntlhlth ~ hrs1 + sex, gss22a)
summary(mlm1)

p2 <- ggplot(mlm1, aes(x = hrs1, y = mntlhlth, col = sex)) +
  geom_point() +
  geom_smooth(data = mlm1)

print(p2)

ggplot(
  data = gss22a,
  mapping = aes(x = hrs1, y = mntlhlth, na.rm = T)) +
  geom_point(mapping = aes(color = sex, shape = sex)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  labs(
    title = "Poor mental health predicted by hours worked.",
    subtitle = "Depicted by gender.",
    x = "Hours worked", y = "Mental Health",
    color = "Sex", shape = "Sex") 

##########plotting three vars############
mdata <- gss22a[which(gss22a$sex==1), ]
fdata <- gss22a[which(gss22a$sex>=2), ]

chisq.test(mdata$depress, mdata$wrkstat)
chisq.test(fdata$depress, fdata$wrkstat)

fit1 <- lm(cbind(mdata$mntlhlth, mdata$hrs1) ~ mdata$race)
fit2 <- lm(cbind(fdata$mntlhlth, fdata$hrs1) ~ fdata$race)

v3 <- ggplot(gss22a, aes(x = hrs1, y = mntlhlth, col = sex))
print(v3)

summary(fit2)

testreg <- lm(mntlhlth ~ age, gss22a)
summary(testreg)

plot(gss22a$age, gss22a$mntlhlth, main="Mental Health by Age",
     xlab = "Age", ylab="Mental Health")
#add a line of best fit
abline(testreg, col="red")

p3 <- ggplot(gss22a, aes(x = hrs1, y = mntlhlth)) +
  geom_point() +
  geom_abline(intercept = 1.248, slope = 0.006) + facet_wrap(~ sex)

print(p3)

ggplot(gss22a, aes(hrs1, mntlhlth)) + geom_point() +
  scale_linetype_binned() +
  abline(lm(mntlhlth ~ hrs1 + sex, gss22a)) + facet_wrap(~ sex)






hist(gss22a$mntlhlth)
