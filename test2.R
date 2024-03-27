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

