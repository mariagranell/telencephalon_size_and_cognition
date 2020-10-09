# Title     : Spatial in 3 parts
# Objective : Analyze spatial learning
# Created by: mariagranell
# Created on: 26/08/2020

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

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Spatial_learning")
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Spatial_learning/Spatial_RevLearning2.csv")

#redefine the class of each variable
data <- within(data, {
  id <- factor(id)
  replicate <- factor(replicate)
  selection <- factor(selection) })
table(data$selection)

d<-data
d<-d[complete.cases(d$choice),]

#choice means correct or incorrect. Side is the assigned side, RL the choice.

#divide in parts
d1<-d[between(d$trial, 1, 14),]
d2<-d[between(d$trial, 15, 27),]
d3<-d[between(d$trial, 28, 40),]

#MODEL PART1-------------------------------------------------------
d1$trial_centered <- d1$trial - 7   # or whatever trial that is the midtrial

Anova(model1 <- glmer(choice ~ selection + trial_centered + side +
  (trial_centered|id) + (trial_centered|replicate/selection),
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

plot(ggeffect(model1, terms = c("side")))
test(emmeans(model1, pairwise ~ side))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = model1)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(simple.model1) #most of the variance is due to id.


#MODEL PART2-------------------------------------------------------
d2$trial_centered <- d2$trial - 21 #the middle could be 21 or 22

Anova(model2 <- glmer(choice ~ selection *trial_centered + side +
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

plot(ggeffect(model2, terms = c("side")))
test(emmeans(model2, pairwise ~ side))

emmip(model2, selection ~ side|trial_centered) + theme_classic()

d2 %>%
  filter(!is.na(choice)) %>%
  group_by(trial, selection) %>%
  summarize(mean_choice = mean(choice),
            lower = binom.test(sum(choice), n())$conf.int[1],
            upper = binom.test(sum(choice), n())$conf.int[2]) %>%
  {ggplot(., aes(trial, mean_choice, ymin = lower, ymax = upper, col=selection)) +
  geom_pointrange(position = position_dodge(0.5), alpha = 0.7) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 0.8, lty = 2) +
  scale_color_manual(values = c("#fcca00", "#1baccc")) +
  theme_classic()+
  ggtitle('average success over time by treatment and stimulus color (95% CI)')}

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = model2)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(model2) #most of the variance is due to id.

#MODEL PART3-------------------------------------------------------
d3$trial_centered <- d3$trial - 34 #the middle could be 34 or 35

Anova(model3 <- glmer(choice ~ selection +trial_centered + side +
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

plot(ggeffect(model3, terms = c("side")))
test(emmeans(model3, pairwise ~ side))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = model3)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(model3) #most of the variance is due to id.

