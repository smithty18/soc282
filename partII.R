## For Research Project, SOC282, Part II ##
# Hypothesis Testing Seciton #

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

###chisquare with subsetted data
chisq.test(maledata$depress, maledata$wrkstat)
chisq.test(femaledata$depress, femaledata$wrkstat)

# Make data subsets based on sex
maledata <- gss22a[which(gss22a$sex==1), ]
femaledata <- gss22a[which(gss22a$sex>=2), ]

## two sample t tests of bad mental health days and hours worked, sampled by sex
t.test(maledata$mntlhlth, maledata$hrs1)
t.test(femaledata$mntlhlth, femaledata$hrs1)

##ANOVA
oneway.test(mntlhlth ~ hrs1,)
oneway.test(mntlhlth ~ sex, gss22a)

anova(lm(mntlhlth ~ hrs1, data = gss22a))

anova(lm((cbindmntlhlth, hrs1) ~ sex, data = gss22a))


## regression ##

## regression ##
plot(gss22a$mntlhlth, gss22a$hrs1, main="Days of poor mental health and hours worked.",
     xlab = "Poor mental health", ylab="Work hours")
#add a line of best fit
abline(lm(gss22a$mntlhlth~gss22a$hrs1), col="red")

#Bivariate Regression
reg1 <- lm(mntlhlth ~ hrs1, gss22a)
summary(reg1)

## for female##
plot(femaledata$mntlhlth, femaledata$hrs1, main="Days of poor mental health and hours worked for women.",
     xlab = "Poor mental health", ylab="Work hours",
     abline(lm(femaledata$mntlhlth~femaledata$hrs1), col="red"))

reg2 <- lm(mntlhlth ~ hrs1, femaledata)
summary(reg2)

## for male##
plot(maledata$mntlhlth, maledata$hrs1, main="Days of poor mental health and hours worked for men.",
     xlab = "Poor mental health", ylab="Work hours",
     abline(lm(maledata$mntlhlth~maledata$hrs1), col="red"))

reg3 <- lm(mntlhlth ~ hrs1, maledata)
summary(reg3)reg1

mlm1






