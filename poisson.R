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

bino1 <- zeroinfl(mntlhlth ~ hrs1 + race + sex + age, gss22a)
summary(bino1)

bino2 <- zeroinfl(mntlhlth ~ hrs1, gss22a)
summary(bino2)


lmsss <- lm(mntlhlth ~ hrs1 * sex + age + race * hrs1, gss22a)
summary(lmsss)


lmsss2 <- zeroinfl(mntlhlth ~ hrs1 * sex + age + race * hrs1, data = gss22a,
                   + dist(negbin) + EM = TRUE
summary(lmsss2)





############# to dos?
## code continuous by categorical?
# how to plot interactions
