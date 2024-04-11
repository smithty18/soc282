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
## poisson regression
pois1 <- glm(formula = mntlhlth ~ hrs1, family = poisson, data = gss22a)
summary(pois1)

coefficients(pois1)
pois1fitted <- predict(pois1)

plot(pois1)

## poisson regresspois1## poisson regression with factored variables and interactions
pois2 <- glm(formula = mntlhlth ~ hrs1 * factor(sex) * factor(race), family = poisson, data = gss22a)
summary(pois2)

coefficients(summary(pois2))

anova(pois2)

plot(pois2)


pois1fitted <- predict(pois1)

ggplot(pois1) + 
  geom_point(aes(hrs1, mntlhlth)) +
  geom_line(aes(hrs1, mntlhlth))


###########################################
###########################################
###########################################
###########################################
## what is all this??##############################
bino1 <- zeroinfl(mntlhlth ~ hrs1 + race + sex + age, gss22a)
summary(bino1)

bino2 <- zeroinfl(mntlhlth ~ hrs1, gss22a)
summary(bino2)


lmsss <- lm(mntlhlth ~ hrs1 * sex + age + race * hrs1, gss22a)
summary(lmsss)


lmsss2 <- zeroinfl(mntlhlth ~ hrs1 * sex + age + race * hrs1, data = gss22a,
                   + dist(mntlhlth))
summary(lmsss2)





############# to dos?
## code continuous by categorical?
# how to plot interactions
