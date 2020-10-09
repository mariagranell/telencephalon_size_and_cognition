# Title     : Long
# Objective : Problem solving data
# Created by: mariagranell
# Created on: 27/08/2020

library(car)
library(MASS)
library(lattice)
library(MCMCglmm)
library(lme4)
library(lmerTest)
library("ggpubr")
library(dplyr)
library(tidyverse)

#model disgnosis
library(nortest)
library(gridExtra)
library(emmeans)
library(ggeffects)
## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
library(MuMIn)

#data for SI experiment-----------------------------------------------------------------------------------------------
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Problem_solving")
data1 <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Problem_solving/Problem_solving.csv")

data1 <- data1[complete.cases(data1$time),]
data1 <- data1[data1$selection != "C",]
bins <- data1[data1$trial == 1,]
last <- data1[data1$trial == 12,]

#data for memory experiment------------------------------------
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Memory")
data2 <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Memory/LT_memory.csv")

data2 <- data2[complete.cases(data2$seconds),]
first <- data2[data2$trial == 1,]

#ther's three trials per experiment. Remember, from individual 19 is batch 2.
#the assigned colour is to the reversal learning for batch 1

#Comparision last to first---------------------------------------------------------------------

comp<-combina(last,first)

hist(log(comp$time))
lillie.test(log(comp$time)) # is not normal..

Anova(model <- lmer(log(time) ~ selection + trial + (1 | id) + (1|replicate), data = comp,
                    na.action = na.exclude, control = lmerControl(optimizer = "bobyqa")))
summary(model)

dd <- dotplot(ranef(model, condVar = TRUE))
do.call(grid.arrange, c(dd, list(nrow = 1)))
plot(model, residuals(.) ~ log(fitted(.)))
plot(model)
plot(resid(model))
lillie.test(resid(model))
qqPlot(resid(model))
# Plot of marginal effects for significant results
plot(ggeffect(model, terms = c("selection")))
test(emmeans(model, pairwise ~ selection)) #the comparision between A and B is the significant one

#Graph---------------------------------------------------------------------------------------------------------------------
ggplot(comp, aes(x=trial,y=time, fill=selection))+
  geom_boxplot()














