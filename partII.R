######For Research Project, SOC282, Part II#######

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

####################
# chi square
chisq.test(mdata$depress, mdata$wrkstat)
chisq.test(fdata$depress, fdata$wrkstat)

####################
#T testing with 2 sample means#
# Make data subsets based on workstat
fulltimedata <- gss22a[which(gss22a$wrkstat==1), ]
parttimedata <- gss22a[which(gss22a$wrkstat>=2), ]
othertimedata <- gss22a[which(gss22a$wrkstat>=3), ] ## with job but out of work, due to strike, illness, lay off ##

mdata <- gss22a[which(gss22a$sex==1), ]
fdata <- gss22a[which(gss22a$sex==2), ]

## two sample t tests of bad mental health days and hours worked, sampled by work status
t1 <- t.test(fulltimedata$mntlhlth, parttimedata$mntlhlth)
t2 <- t.test(fulltimedata$mntlhlth, othertimedata$mntlhlth) 
t3 <- t.test(parttimedata$mntlhlth, othertimedata$mntlhlth)

print(t1)
print(t2) ###
print(t3)

####################
##ANOVA
a1 <- oneway.test(mntlhlth ~ wrkstat, data = gss22a)
oneway.test(mntlhlth ~ wrkstat + sex, data = gss22a)
oneway.test(mntlhlth ~ wrkstat + race, data = gss22a)

print(a1)

####################
#Regression
reg1 <- lm(mntlhlth ~ hrs1, gss22a)
summary(reg1)


##########################
#multiple regression?##

##factoring categorical variables for regression

gss22a$racef <- as.factor(gss22a$race)
class(gss22a$race)
class(gss22a$racef)
gss22a$sexf <- as.factor(gss22a$sex)
class(gss22a$sex)
class(gss22a$sexf)

##m regresion
summary(mlm1 <- (lm(formula = mntlhlth ~ hrs1 + sex, gss22a)))
# factored
summary(mlm2 <- (lm(formula = mntlhlth ~ hrs1 + sexf, gss22a)))

summary(mlm2 <- (lm(formula = mntlhlth ~ hrs1 + physhlth + educ + age + sexf + racef, gss22a)))

summary(mlm2 <- (lm(formula = mntlhlth ~ hrs1 * physhlth + sexf * racef + educ + age, gss22a)))

summary(sexlm <- (lm(formula = mntlhlth ~ hrs1 + sexf, gss22a)))

ggplot(gss22a)+
  aes(sexf, mntlhlth) +
  geom_density()

ggplot(gss22a, aes(x = hrs1, y = mntlhlth, fill = sexf))+
  geom_violin() +
  facet_wrap(~sexf)

###########DO NOT#####################
#######################################
###bigggggg and unreadable graph#######
ggplot(gss22a)+
  aes(hrs1, mntlhlth) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = F) +
  geom_abline(intercept = 4.03, slope = -0.002, color = "blue") +
  geom_abline(intercept = 4.03, slope = -0.18, color = "green") +
  geom_abline(intercept = 4.03, slope = -5.8, color = "orange") +
  geom_abline(intercept = 4.03, slope = 0.75, color = "cyan") +
  geom_abline(intercept = 4.03, slope = 0.04, color = "yellow") +
  geom_abline(intercept = 4.03, slope = 0.08, color = "beige") +
  geom_abline(intercept = 4.03, slope = -0.05, color = "tan") +
  geom_abline(intercept = 4.03, slope = 7.98, color = "black") +
  geom_abline(intercept = 4.03, slope = 3.56, color = "grey") +
  geom_abline(intercept = 4.03, slope = -0.17, color = "purple") +
  geom_abline(intercept = 4.03, slope = -0.07, color = "white") +
  labs(
    title = "Regression with multiple lines of best fit shown.")

