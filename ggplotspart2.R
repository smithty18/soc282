require(ggplot2)
require(pscl)
require(MASS)
require(boot)
library(gssr)
library(tidyverse)
library(dplyr)
load("/Users/tylermsmith/Documents/MSU/2024/Spring/SOC282/r/GSS22.Rdata")

gss22a <- gss22 %>% 
  drop_na(mntlhlth)

gss22a$sex <- as.factor(gss22a$sex)
gss22a$depress <- as.factor(gss22a$depress)
gss22a$wrkstat <- as.factor(gss22a$wrkstat)

##bargraph
ggplot(gss22a, aes(x = fct_infreq(wrkstat, fill = sex)))+
  geom_bar() +
  labs(
    title = "Work Status",
    subtitle = "Subsetted with Depression",
    x = "Work Status (1 = Full Time, 2 = Part Time, 3 = Laid Off)", y = "Count")

##histogram
ggplot(gss22a, aes(x = mntlhlth))+
  geom_histogram() +
  labs(
    title = "Histogram of Poor Mental Health Days"
  )

##histogram subsetted by workstatus

ggplot(gss22a, aes(x = mntlhlth, fill = wrkstat))+
  geom_histogram() +
  facet_wrap(~wrkstat) +
  labs(
    title = "Histogram of Poor Mental Health Days",
    subtitle = "Faceted by Work Status")

## violin plot
ggplot(gss22a, aes(x = wrkstat, y = mntlhlth, fill = wrkstat))+
  geom_violin() +
  labs(
    title = "Work Status",
  subtitle = "Violin Plots of Work Status and Mental Health",
  x = "Work Status (1 = Full Time, 2 = Part Time, 3 = Laid Off)", y = "Count of Poor Mental Health Days")

##regression/scatterplots
ggplot(
  data = gss22a,
  mapping = aes(x = hrs1, y = mntlhlth)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red") +
  labs(
    title = "Scatter Plot of Poor Mental health Predicted by Hours Worked.",
    subtitle = "Depicting no association between variables.",
    x = "Hours worked", y = "Mental Health")

##multi of dv and iv by sex
ggplot(
  data = gss22a,
  mapping = aes(x = hrs1, y = mntlhlth)) +
  geom_point(mapping = aes(color = sex, shape = sex)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  facet_wrap(~sex) +
  labs(
    title = "Scatter Plot of Poor Mental health Predicted by Hours Worked..",
    subtitle = "Depicted by differing gender plots.",
    x = "Hours worked", y = "Mental Health",
    color = "Sex", shape = "Sex")

