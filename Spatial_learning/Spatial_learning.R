# Title     : Spatial
# Objective : Analyze spcial learning
# Created by: mariagranell
# Created on: 26/08/2020

#there is a trend on the model that A(down) is better.
#If you ignore the anova and go for simplifiying the model you have then a significance
#I preffer following the anova and reporting the trend

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

d$trial_centered<-d$trial-20  #middle trial

Anova(full.model <- glmer(choice ~ selection * (trial_centered + side) + #the interactions are nor significant
  (trial_centered|id) + (trial_centered|replicate/selection),
                          data = d,
                          na.action=na.exclude,
                          family=binomial,control=glmerControl(optimizer="bobyqa",
                                                               optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
summary(full.model)

Anova(simple.model <- glmer(choice ~ selection + trial_centered + side +
  (trial_centered|id) + (trial_centered|replicate),
                            data = d,
                            na.action=na.exclude,
                            family=binomial,control=glmerControl(optimizer="bobyqa",

                                                                 optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
Anova(simple.model1 <- glmer(choice ~ selection+ trial_centered + side +
  (1|id) + (1|replicate),
                             data = d,
                             na.action=na.exclude,
                             family=binomial,control=glmerControl(optimizer="bobyqa",
                                                                  optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations



anova(simple.model1,simple.model, type=3) #is significant, suggesting a better fit for the simple.model (i.e. it tests whether reduction in the residual sum of squares are statistically significant or not)
summary(simple.model1)

## model diagnostic
dd <- dotplot(ranef(simple.model1,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(simple.model1, terms = c("selection")))
test(emmeans(simple.model1, pairwise ~ selection))

plot(ggeffect(simple.model1, terms = c("side")))
test(emmeans(simple.model1, pairwise ~ side))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = simple.model1)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(simple.model1) #most of the variance is due to id.

d %>%
  filter(!is.na(choice)) %>%
  group_by(trial, side, selection) %>%
  summarize(mean_choice = mean(choice),
            lower = binom.test(sum(choice), n())$conf.int[1],
            upper = binom.test(sum(choice), n())$conf.int[2]) %>%
  {ggplot(., aes(trial, mean_choice, ymin = lower, ymax = upper, col = side)) +
  geom_pointrange(position = position_dodge(0.5), alpha = 0.7) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 0.8, lty = 2) +
  scale_color_manual(values = c("#fcca00", "#1baccc")) +
  facet_grid(~selection) +
  theme_classic()+
  ggtitle('learning sides')}

d %>%
  filter(!is.na(choice)) %>%
  group_by(trial, selection) %>%
  summarize(mean_choice = mean(choice),
            lower = binom.test(sum(choice), n())$conf.int[1],
            upper = binom.test(sum(choice), n())$conf.int[2]) %>%
  {ggplot(., aes(trial, mean_choice, ymin = lower, ymax = upper, col = selection)) +
  geom_pointrange(position = position_dodge(0.5), alpha = 0.7) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 0.8, lty = 2) +
  scale_color_manual(values = c("#fcca00", "#1baccc")) +
  geom_smooth(method = "loess")+
  theme_classic()+
  ggtitle('learning sides')}

dA<-d[d$selection=="A",]
dB<-d[d$selection=="B",]

Anova(simple.model1 <- glmer(choice ~  trial_centered + side +
  (1|id) + (1|replicate),
                             data = dA,
                             na.action=na.exclude,
                             family=binomial,control=glmerControl(optimizer="bobyqa",
                                                                  optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations



anova(simple.model1,simple.model, type=3) #is significant, suggesting a better fit for the simple.model (i.e. it tests whether reduction in the residual sum of squares are statistically significant or not)
summary(simple.model1)

## model diagnostic
dd <- dotplot(ranef(simple.model1,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(simple.model1, terms = c("selection")))
test(emmeans(simple.model1, pairwise ~ selection))

plot(ggeffect(simple.model1, terms = c("side")))
test(emmeans(simple.model1, pairwise ~ side))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = simple.model1)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(simple.model1) #most of the variance is due to id.

##End performance levels----------------------------

dA <- d[d$selection=="A",]
dB <- d[d$selection=="B",]
dend<-dA[dA$trial>2,]
dend<-d[d$trial>2,]
dend$trial_centered<-dend$trial- 21 #middle trial

Anova(simple.model <- glmer(choice ~   trial_centered + side  +
  (1|id) + (1|replicate),
                            data = dend,
                            na.action=na.exclude,
                            family=binomial,control=glmerControl(optimizer="bobyqa",
                                                                 optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
summary(simple.model)
r.squaredGLMM(simple.model)

summarySE(data = dend, groupvars = c("selection"), conf.interval = 0.95, .drop = TRUE,
          measurevar = "choice", na.rm = T)

##second plot--------------------------------------------

d$Selection<-ifelse(d$selection == "A", "Down", "Up")

m <- glmer(choice ~ trial_centered + Selection + side + trial_centered:Selection + trial_centered:side +(1|id) + (1|replicate), family = binomial, data = d, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
Anova(m, type = 3)

m_fit <- effect("trial_centered * Selection", m, xlevels = 40) %>%
  as.data.frame()

m_means <- d%>%
  filter(!is.na(choice)) %>%
  group_by(trial, Selection) %>%
  summarize(mean_choice = mean(choice))

darkcolor<-c("#00AFBB","#ff8657")
lightcolor<-c("#00AFBB","#FC4E07")

m_plot<-ggplot(m_means, aes(x=trial, y= mean_choice, colour = Selection, shape=Selection)) +
  geom_point(size = 3.5)+
  geom_hline(yintercept = 0.5, lty = 2, alpha= 1) +
  geom_hline(yintercept = 0.8, lty = 5, alpha=1) +
  geom_line(data = m_fit, aes(trial_centered+20, fit))  +
  geom_ribbon(data = m_fit, col = NA, alpha = 0.15, size = 4,
              aes(trial_centered+20, NULL, ymin = lower, ymax = upper, fill=Selection)) +
  scale_fill_manual(name = " Selection lines",
                    labels = c("Down", "Up"),
                    values=lightcolor) +
  scale_shape_manual(name = " Selection lines",
                     labels = c("Down", "Up"),
                     values=c(16,17))+
  labs(x = "Trials",y="Proportion of correct choices")+
  scale_x_continuous(breaks=seq(2,40,2))+
  scale_colour_manual(name = " Selection lines",
                      labels = c("Down", "Up"),
                      values=lightcolor) +
  ylim(0.57,1)+#0.4 in the other graphs
  theme_classic(base_size = 16)


m_plot+ theme_update(legend.justification = c(1, 0),
                     legend.position = c(0.9, 0.15))#0.9, 0.3 in the other graphs
