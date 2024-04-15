require(ggplot2)
require(pscl)
require(MASS)
require(boot)
library(gssr)
library(tidyverse)
library(dplyr)
library(GGally)
load("/Users/tylermsmith/Documents/MSU/2024/Spring/SOC282/r/GSS22.Rdata")

gss22a <- gss22 %>% 
  drop_na(mntlhlth)

gss22a$sex <- as.factor(gss22a$sex)
gss22a$depress <- as.factor(gss22a$depress)
gss22a$wrkstat <- as.factor(gss22a$wrkstat)
gss22a$race <- as.factor(gss22a$race)

ggpairs(gss22a[,5:8])

##
ggplot(gss22a) +
  (aes(hrs1, fill = depress, na.rm = T)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~sex)

##bargraph
ggplot(gss22a, aes(x = fct_infreq(wrkstat), fill = depress))+
  geom_bar() +
  labs(
    title = "Work Status  (1 = Full Time, 2 = Part Time, 3 = Laid Off)",
    subtitle = "Subsetted with Depression (1 = yes, 2 = no)",
    x = "Work Status", y = "Count")

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

##density plot
ggplot(gss22a, aes(x = mntlhlth, color = wrkstat, fill = wrkstat))+
  geom_density(alpha = 0.25) +
  labs(
    title = "Density Plot of Poor Mental Health Days",
    subtitle = "Varying by Work Status (1 = full time, 2 = part time, 3 = with job, but out of work)"
  )

## bar graph for chi square visual
ggplot(gss22a) +
  aes(x = as.factor(depress), fill = wrkstat) +
  geom_bar() +
  facet_grid(~wrkstat~sex) +
  labs(
    title = "Facet Grid of Depression rates by Workstatus as varies by Gender",
    subtitle = "Columns '1 = Male, 2 = Female', Rows '1 = FT, 2 = PT, 3 = NA'",
    x = "Depression (1 = Yes, 2 = No)"
  )

## box and whisker plot 
ggplot(gss22a, aes(x = wrkstat, y = mntlhlth)) +
       geom_boxplot(outlier.colour="red", outlier.shape=8,
                    outlier.size=4) + 
  labs(
    title = "Box Plot of Continuous Variables; Mental Health and Hours Worked",
    subtitle = "By Gender; With Dot Plot and Jitter Plot Overlayed")

ggplot(gss22a, aes(x = wrkstat)) +
  geom_boxplot() +
  labs(
    title = "Box Plot of Continuous Variables; Mental Health and Hours Worked",
    subtitle = "By Gender; With Dot Plot and Jitter Plot Overlayed")

##regression/scatterplots
ggplot(
  data = gss22a,
  mapping = aes(x = hrs1, y = mntlhlth)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red") +
  labs(
    title = "Scatter Plot of Poor Mental health Predicted by Hours Worked.",
    subtitle = "Depicting little association between variables.",
    x = "Hours worked", y = "Mental Health")

## jitter plot
ggplot(gss22a)+
  aes(sexf, mntlhlth) +
  geom_jitter() +
  facet_wrap(~wrkstat) +
  labs(
    title = "Jitterplot of Mental Health by Gender",
    subtitle = "",
    x = "Sexf = 1(Male), 2(Female).",
    y = "Poor days of mental health, range from 0-30."
  )

##multi of dv and iv by sex
ggplot(
  data = gss22a,
  mapping = aes(x = hrs1, y = mntlhlth)) +
  geom_point(mapping = aes(color = factor(sex), shape = factor(sex))) +
  geom_smooth(method = "lm", se = F, color = "black") +
  facet_wrap(~factor(sex)) +
  labs(
    title = "Scatter Plot of Poor Mental health Predicted by Hours Worked.",
    subtitle = "Faceted by gender.",
    x = "Hours worked", y = "Mental Health",
    color = "Sex", shape = "Sex")

## plotting interactionals
#by sex
ggplot(gss22a) +
  aes(x = hrs1, y = mntlhlth, color = factor(sex)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", se = F) +
  labs(
    title = "Mental health and Hours Worked.",
    subtitle = "Differing lines of best fit by gender.",
    x = "Hours worked", y = "Mental Health",
    color = "Sex", shape = "Sex")

#by race
ggplot(gss22a) +
  aes(x = hrs1, y = mntlhlth, color = factor(race)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", se = F)+
  labs(
    title = "Scatter Plot of Poor Mental health Predicted by Hours Worked.",
    subtitle = "Differing lines of best fit by race.",
    x = "Hours worked", y = "Mental Health",
    color = "Race", shape = "Race")

#faceted, sex faceted by race
ggplot(gss22a) +
  aes(x = hrs1, y = mntlhlth, color = factor(race)) +
  geom_point(color = "grey") +
  facet_grid(~sex) +
  geom_smooth(method = "lm", se = F)

## heatmap?
ggplot(gss22a, aes(x=hrs1, y=mntlhlth, fill = mntlhlth)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="red", name="Mental Health")



