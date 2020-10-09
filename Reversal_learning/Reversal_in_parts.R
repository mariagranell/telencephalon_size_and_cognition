# Title     : Reversal in 3 parts
# Objective : Analyze reversal learning
# Created by: mariagranell
# Created on: 25/08/2020

#they learned the reversal better with yellow,
#each selection and all together in all parts.

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

#data--------------------------------------------------------------

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Reversal_learning")
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Reversal_learning/Reversal_learning_copy1.csv")

#redefine the class of each variable
data <- within(data, {
  id <- factor(id)
  replicate <- factor(replicate)
  selection <- factor(selection) })
table(data$selection)

#choice means correct or incorrect. Colour is the assigned colour.

#select the associative learning experiment data:
d<-data[data$experiment == "revlearning",]
d<-d[complete.cases(d$choice),]

#dividing the data in 3 parts
d1<-d[d$trial<23,]
d2<-d[between(d$trial,23,44),]
d3<-d[d$trial>44,]

#MODEL PART1-------------------------------------------------------
#centering the trials
d1$trial_centered<-d1$trial-11  #middle trial

Anova(model1 <- glmer(choice ~ selection +trial_centered + colour +
  (trial_centered|id) + (trial_centered|replicate),
                          data = d1,
                          na.action=na.exclude,
                          family=binomial,control=glmerControl(optimizer="bobyqa",
                                                               optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
summary(model1)

## model diagnostic
dd <- dotplot(ranef(model1,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(model1, terms = c("selection")))
test(emmeans(model1, pairwise ~ selection))

plot(ggeffect(model1, terms = c("colour")))
test(emmeans(model1, pairwise ~ colour))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = simple.model)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

#if you do a model for eahc selection, both learn better with yellow.

#MODEL PART2-------------------------------------------------------
#centering the trials
d2$trial_centered<-d2$trial-33  #middle trial

Anova(model2 <- glmer(choice ~ selection +trial_centered + colour +
  (trial_centered|id) + (trial_centered|replicate),
                            data = d2,
                            na.action=na.exclude,
                            family=binomial,control=glmerControl(optimizer="bobyqa",
                                                                 optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
summary(model2)

## model diagnostic
dd <- dotplot(ranef(model2,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(model2, terms = c("selection")))
test(emmeans(model2, pairwise ~ selection))

plot(ggeffect(model2, terms = c("colour")))
test(emmeans(model2, pairwise ~ colour))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = model2)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

#if you do a model for each selection, both learn better with yellow.

#MODEL PART3-------------------------------------------------------
#centering the trials
d3$trial_centered<-d3$trial-55  #middle trial

Anova(model3 <- glmer(choice ~ selection +trial_centered + colour +
  (trial_centered|id) + (trial_centered|replicate),
                      data = d3,
                      na.action=na.exclude,
                      family=binomial,control=glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
summary(model3)

## model diagnostic
dd <- dotplot(ranef(model3,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(model3, terms = c("selection")))
test(emmeans(model3, pairwise ~ selection))

plot(ggeffect(model3, terms = c("colour")))
test(emmeans(model3, pairwise ~ colour))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = model3)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

#if you do a model for each selection, both learn better with yellow.
