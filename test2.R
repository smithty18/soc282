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

# get descriptive statistics of relevant variables
desctable <- describe(gss22a[ , c('wrkstat', 'hrs1', 'sex', 'mntlhlth', 'depress')], 
                      na.rm = TRUE, check = TRUE, quant = c(.25,.75), IQR = TRUE)

  ## descriptive statistics of relevant variable but of the most common variables
desctable.fast  <- describe(gss22a[ , c('wrkstat', 'hrs1', 'sex', 'mntlhlth', 'depress')], na.rm = TRUE, fast = TRUE)
                            
## descriptive statistics for mental health, one of the dependent variables in the research project,
### drawn from the raw data set
desctable.mnthlhlth.solo <- describe(gss22$mntlhlth, na.rm = TRUE, quant = c(.25,.75), IQR = TRUE)

# make a data frame of hours worked
workhours <- data.frame(gss22a$hrs1)

# make a data frame of mntlhlth
mntlhealth <- data.frame(gss22a$mntlhlth)

# make a data frame of sex
sex_df <- data.frame(gss22a$sex)

# make a data frame of work status
workstatus <- data.frame(gss22a$wrkstat)

#make a data frame of are you depressed
depressed <- data.frame(gss22a$depress)

#make a data frame of mntlhlth and working hours
hist_hours <- data.frame(gss22a$mntlhlth, gss22a$hrs1)

#plot mntlhlth as a histogram
ggplot(data = mntlhealth, aes(x = gss22a.mntlhlth)) + 
  geom_histogram(binwidth = 3, color = "black", fill = "white") + 
  labs(title = "Days of Poor Mental Health", x = "Days of Poor Mental Health", y = "Frequency")

#plot hours worked as a histogram
ggplot(data = workhours, aes(x = gss22a.hrs1)) + 
  geom_histogram(binwidth = 3, color = "black", fill = "white") + 
  labs(title = "Hours worked in a work week", x = "Hours worked", y = "Frequency")

#### recode continuous data into ordinal data
## WIP ##


## make bivariate table(s)
bivartable<- table(gss22a$depress, gss22a$wrkstat)
bivartable
coltable <- prop.table(bivartable, 2)
coltable
round(coltable, 2)

#Separate bivariate tables by gender
#make a new dataset that is Female only:
femaledata <- gss22a[which(gss22a$sex==2), ]
#use the same code as above to make a table with column percentages with the 
# female only dataset this time:
bivartablef <- table(femaledata$depress, femaledata$wrkstat)
coltablef <- prop.table(bivartablef)
round(coltablef, 2)
#then do the same thing with a Male only dataset:
maledata <- gss22a[which(gss22a$sex==1), ]
bivartablem <- table(maledata$depress, maledata$wrkstat)
coltablem <- prop.table(bivartablem)
round(coltablem, 2)

## chi square test of bivariate table of depression and work status
chisq.test(bivartable)
## chi sq of bivartable controlled for sex, females
chisq.test(bivartablef)
## chi sq of bivartable controlled for sex, males
chisq.test(bivartablem)
## two sample t tests of bad mental health days and hours worked, sampled by sex
t.test(femaledata$mntlhlth, maledata$mntlhlth)
t.test(femaledata$hrs1, maledata$hrs1)

## anova
oneway.test(mntlhlth ~ wrkstat, gss22a)
oneway.test(mntlhlth ~ wrkstat, maledata)
oneway.test(mntlhlth ~ wrkstat, femaledata)

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
summary(reg3)
