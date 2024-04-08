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
gss22a$mntlhlth <- as.numeric

ggplot(
  data = gss22a,
  mapping = aes(x = hrs1, y = mntlhlth, na.rm = T)) +
  geom_point(mapping = aes(color = sex, shape = sex)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  labs(
    title = "Poor mental health predicted by hours worked.",
    subtitle = "Depicted by differing gender plots.",
    x = "Hours worked", y = "Mental Health",
    color = "Sex", shape = "Sex")
gss22a$race <- as.factor(gss22a$race)  

ggplot(
  data = gss22a,
  mapping = aes(x = hrs1, y = mntlhlth, na.rm = T)) +
  geom_point(mapping = aes(color = race, shape = race)) +
  geom_smooth(method = "lm", se = F, color = "black") +
  facet_wrap(~sex + race)
  labs(
    title = "Poor mental health predicted by hours worked.",
    subtitle = "Depicted by differing gender plots.",
    x = "Hours worked", y = "Mental Health",
    color = "Sex", shape = "Sex")

gss22a$race <- as.factor(gss22a$race)  
 
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

gss22a$age <- as.numeric(gss22a$age)
gss22a$hrs1 <- as.numeric(gss22a$hrs1)
ggplot(
  data = gss22a,
  mapping = aes(x = mntlhlth, y = age, na.rm = T)) +
  geom_point(mapping = aes(color = hrs1)) +
  geom_smooth(method = "lm", se = F, color = "red")

gss22a$wrkstat <- as.factor(gss22a$wrkstat)
ggplot(gss22a, aes(x = wrkstat, y = mntlhlth)) +
  geom_boxplot()

ggplot(gss22a, aes(x = mntlhlth, color = wrkstat, fill = wrkstat)) +
  geom_density(alpha = 0.5)

ggplot(gss22a, aes(x = mntlhlth, color = wrkstat, fill = wrkstat)) +
  geom_density(alpha = 0.5) +
facet_wrap(~wrkstat )

ggplot(gss22a, aes(x = hrs1, y = mntlhlth)) +
  geom_point(aes(color = sex)) +
  facet_wrap(~race) +
  geom_smooth(method = "lm", se = F)

gss22a$depress <- as.factor(gss22a$depress)
ggplot(gss22a, aes(x = fct_infreq(wrkstat), fill = depress))+
  geom_bar() +
  facet_grid(~race)
