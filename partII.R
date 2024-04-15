######For Research Project, SOC282, Part II#######

# set working directory
setwd("/Users/tylermsmith/Documents/MSU/2024/Spring/SOC282/research_project/1")

# load libraries
library(tidyverse)
library(psych)
library(gssr)
library(ggplot2)
library(dplyr)
library(xtable)

# load gss22 data
load("/Users/tylermsmith/Documents/MSU/2024/Spring/SOC282/r/GSS22.Rdata")

# clean the data further on the basis of 1 common dependent variable (depression) making a new sheet referred to as: gss22a.
gss22a <- gss22 %>% 
  drop_na(mntlhlth)

####################
# chi square
chisq.test(gss22a$depress, gss22a$wrkstat)

####################
#T testing with 2 sample means#
# Make data subsets based on workstat
fulltimedata <- gss22a[which(gss22a$wrkstat==1), ]
parttimedata <- gss22a[which(gss22a$wrkstat>=2), ]
othertimedata <- gss22a[which(gss22a$wrkstat>=3), ] ## with job but out of work, due to strike, illness, lay off ##

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

print(a1)

####################
#Regression
reg1 <- lm(mntlhlth ~ hrs1, gss22a)
summary(reg1)

##########################
#multiple regression##

##factoring categorical variables for regression
gss22a$sexf <- as.factor(gss22a$sex)
class(gss22a$sex)
class(gss22a$sexf)

##m regresion
summary(mlm1 <- (lm(formula = mntlhlth ~ hrs1 + sex, gss22a)))
# factored
summary(mlm2 <- (lm(formula = mntlhlth ~ hrs1 + sexf, gss22a)))

