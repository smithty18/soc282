
##SOC 282
##Spring 2024
#Example Code for Elements of Research Project

#Load Cleaned GSS2022 R data
#Change the filepath to match your machine
load("C:/Users/mcope/Documents/Soc 282 Data/GSS22.Rdata")

#load useful librarys (need to have already installed the packages)
library(tidyverse)
library(psych)


###Basic Data Cleaning
# subset data to only observations that aren't missing on most missing DV (physhlth)
# This should make your data have the same sample size as your cleaned dataset in Excel
gss22a <- gss22 %>% 
  drop_na(physhlth)
#N=1922 (see in Global Environment window on right)


###Get descriptive statistics of variables of interest
#include 'fast = F' to get full statistics (fast=T is only most common)
#replace these variable names with your variable names, inside the ''
desctable <- describe(gss22a[ , c('marital', 'sex', 'health', 'physhlth','sibs')], 
                      fast=F)
desctable
###NOTE: R will print out descriptive statistics if you tell it to, EVEN if 
# those measures don't make sense for that variable (ex: standard deviation for
# a nominal variable is meaningless, but R will print it), so you need to be
# smarter than the computer to know what measures make sense.


###Make Some Graphs

#pie charts

#example with self-rated health variable:
#need to make the variable into a table for the "pie" function to read
pietablesrh <- table(gss22a$health)
pie(pietablesrh)
#add labels:
#print variable to see what the labels/values are
gss22a$health
#add label names
pie(pietablesrh, labels = c("Excellent", "Good", "Fair", "Poor"))

#repeat with marital status
gss22a$marital
pietablemarital <- table(gss22a$marital)
pie(pietablemarital, labels = c("Married", "Widowed", "Divorced", "Separated", "Never Married"))

#make a histogram
hist(gss22a$physhlth)
#just like the pie chart, can search for options to make the graph prettier, fix labels, etc.

#can make many more types of charts, just search online for the code

#can install/load/explore the "RColorBrewer" package for more color/design options
#google how to for specific elements, like "R pie chart add percentages" or "R hist() fix labels"
#save graphs by using the "Export" tab at the top of the graphs window


###Calculating Measures of Variability
# Standard Deviation is already in the descriptives table above, can look at it here
desctable

# Calculating IQV - There is a "qualvar" package that you can load, but this is
# not the same calculation of IQV as in class, so would recommend using excel instead
# (otherwise need to create a loop function to calculate the IQV without a specific
#pre-made package function, which is beyond the scope of what we are doing in R in this class)

###Calculating a Confidence Interval
# Example with physical health missed workdays
# Calculate and save the mean as a data object
physhlthmean <- mean(gss22a$physhlth)
# Calculate and save the standard deviation as a data object
physhlthsd <- sd(gss22a$physhlth)
# Calculate the standard error of the sample
#N for this variable is 1922 (because cleaned dataset to non-missing)
# if need to check what the N for a variable is: length(gss22$varname)
physhlthse <- physhlthsd/(sqrt(1922))
#Calculate CI at 95% confidence level
#Lower Bound:
LB <- physhlthmean - (1.96*physhlthse)
LB
#Upper Bound
UB <- physhlthmean + (1.96*physhlthse)
UB
# CI[2.46, 3.0] (note, this is very close but slightly different from the same
#calculations in excel, due to rounding differences)

#Margin of Error is 1/2 of CI:
ME <- (UB-LB)/2
ME
#Margin of error is .28


