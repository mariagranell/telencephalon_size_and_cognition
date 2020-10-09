# Title     : Short
# Objective : Analyze short term memory
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

#model disgnosis
library(nortest)
library(gridExtra)
library(emmeans)
library(ggeffects)
## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
library(MuMIn)

#Batch1----------------------------------------------------

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Reversal_learning")
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Reversal_learning/Reversal_learning_copy1.csv")

#redefine the class of each variable
data <- within(data, {
  id <- factor(id)
  replicate <- factor(replicate)
  selection <- factor(selection) })
table(data$selection)

#select the 2 frist trials
d<-data[data$trial <3,]
d<-d[complete.cases(d$choice),]

Anova(batch1 <- glmer(choice ~ selection + trial + colour +
  (1|id) + (1|replicate),
                      data = d,
                      na.action=na.exclude,
                      family=binomial,control=glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations

summary(batch1)

## model diagnostic
dd <- dotplot(ranef(batch1,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(batch1, terms = c("trial")))
test(emmeans(batch1, pairwise ~ trial))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = batch1)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(batch1) #most of the variance is due to id.

#Selections batch1----------------------------------------

dA<-d[d$selection=="A",]
dB<-d[d$selection== "B",]

Anova(m <- glmer(choice ~ trial + colour +
  (1|id) + (1|replicate),
                      data = dA,
                      na.action=na.exclude,
                      family=binomial,control=glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations

summary(m)

## model diagnostic
dd <- dotplot(ranef(m,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(m, terms = c("trial")))
test(emmeans(m, pairwise ~ trial))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = m)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(m) #most of the variance is due to id.

#So if you do the test for both selections.
#Selection A(down) did change their behaviour between trials 1 and 2
#Selection B(up) did nothing

#Batch2----------------------------------------------------------------

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Spacial_learning")
data2 <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Spacial_learning/Spacial_RevLearning2.csv")

#redefine the class of each variable
data2 <- within(data, {
  id <- factor(id)
  replicate <- factor(replicate)
  selection <- factor(selection) })
table(data2$selection)

#select 1 and 2 trial
d2<-data2[data2$trial<3,]
d2<-d2[complete.cases(d2$choice),]

Anova(batch2 <- glmer(choice ~ selection + trial + side +
  (1|id) + (1|replicate),
                      data = d2,
                      na.action=na.exclude,
                      family=binomial,control=glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
summary(batch2)

## model diagnostic
dd <- dotplot(ranef(batch2,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(batch2, terms = c("selection")))
test(emmeans(batch2, pairwise ~ selection))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = batch2)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(batch2) #most of the variance is due to id.

#Selections batch2----------------------------------------

d2A<-d2[d2$selection=="A",]
d2B<-d2[d2$selection== "B",]

Anova(m2 <- glmer(choice ~ trial + side +
  (1|id) + (1|replicate),
                 data = d2B,
                 na.action=na.exclude,
                 family=binomial,control=glmerControl(optimizer="bobyqa",
                                                      optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations

summary(m2)

## model diagnostic
dd <- dotplot(ranef(m2,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(m2, terms = c("trial")))
test(emmeans(m2, pairwise ~ trial))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = m2)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(m2) #most of the variance is due to id.

#So if you do the test for both selections.
#No difference.

#Graphs-------------------------------------------------------------

d$trial<-as.factor(d$trial)
d %>%
  group_by(selection, trial) %>%
  summarize(correct=mean(choice)) %>%
  ggplot() +
  geom_boxplot(aes(x=trial, y=correct, color=selection), size=1)+
  ggtitle("Short term memory Colour") +
  xlab("Trial") + ylab("Proportion of correct chices")+
  theme_linedraw()+
  scale_color_manual(values = c("#00AFBB","#FC4E07"))


d2$trial<-as.factor(d2$trial)
d2 %>%
  group_by(selection, trial) %>%
  summarize(correct=mean(choice))%>%
  ggplot() +
  geom_boxplot(aes(x=trial, y=correct, color=selection), size=1)+
  ggtitle("Short term memory Spacial") +
  xlab("Trial") + ylab("Proportion of correct choices")+
  theme_linedraw()+
  scale_color_manual(values = c("#00AFBB","#FC4E07"))

