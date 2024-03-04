# set working directory
setwd("/Users/tylermsmith/Documents/MSU/2024/1. Spring/SOC282/research project/1")

# load libraries
library(tidyverse)
library(psych)
library(gssr)

# load gss22 data
load("/Users/tylermsmith/Documents/MSU/2024/1. Spring/SOC282/r/GSS22.Rdata")

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

##recode continuous dependent variable mntlhlth into categories
gss22a %>%
  mutate(mntlhlth = case_when(
    `mntlhlth(Self-report)` = 0 ~ "0",
    `mntltlth(Self-report)` > 1 & `mntlhlth(Self-report)` < 3  ~ "1-3",
    `mntlhlth(Self-report)` < 4 ~ "3+"
  ))

# make a pie chart of depression
pietablesrh <- table(gss22a$depress)
pie(pietablesrh)
#add labels:
#print variable to see what the labels/values are
gss22a$depress
#add label names
pie(pietablesrh, labels = c("Yes", "No"))

# make a histogram of days of poor mental health
hist(gss22a$mntlhlth)
hist(gss22a$hrs1)
