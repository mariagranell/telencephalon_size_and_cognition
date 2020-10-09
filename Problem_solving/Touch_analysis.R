# Title     : Touch
# Objective : Analyze the touch data
# Created by: mariagranell
# Created on: 25/08/2020

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

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Problem_solving")
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Problem_solving/Problem_solving.csv")

#redefine the class of each variable
data <- within(data, {
  id <- factor(id)
  replicate <- factor(replicate)
  selection <- factor(selection) })
table(data$selection)

d%>%
  group_by(selection)%>%
  summarize(mean=(mean(touch)))

#for testing the learning time I will remove the bins trials because they give too much variance
d <- data[data$trial > 2,]
d<- d[complete.cases(d$time),]
d$Selection<-ifelse(d$selection == "A", "Down", ifelse(d$selection == "B", "Up","Control"))

#centering the trials
d$trial_centered<-d$trial- 5 #middle trial

Anova(full.model <- glmer(touch ~ selection * trial_centered + #the anova shows no significant interaction
                    (trial_centered|id) + (trial_centered|replicate/selection),
                    data = d,
                    na.action=na.exclude,
                    family=binomial,control=glmerControl(optimizer="bobyqa",
                                                         optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
summary(full.model)

Anova(simple.model <- glmer(touch ~ Selection + trial_centered +
  (trial_centered|id) + (trial_centered| replicate) ,
                          data = d,
                          na.action=na.exclude,
                          family=binomial,control=glmerControl(optimizer="bobyqa",
                                                               optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations

Anova(simple.model2 <- glmer(touch ~ selection + trial_centered +
  (1|id) + (1| replicate) ,
                            data = d,
                            na.action=na.exclude,
                            family=binomial,control=glmerControl(optimizer="bobyqa",
                                                                 optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations


summary(simple.model)

## model comparirsion
anova(simple.model, simple.model2, type=3)

## model diagnostic
dd <- dotplot(ranef(simple.model,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(simple.model, terms = c("selection")))
test(emmeans(simple.model, pairwise ~ selection))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLMM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = simple.model)
plot(simulationOutput)
#?outliers() outliers are mostly a technical term: these are points that are outside our simulations, but we don't know how far away they are

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(simple.model)

d$Selection<-ifelse(d$selection == "A", "Down", ifelse(d$selection == "B", "Up","Control"))
darkcolor<-c("#ffee00","#00AFBB","#ff4a00")
lightcolor<-c("#f5db74","#00AFBB", "#FC4E07")

#plot
m_plot<-d %>%
  filter(!is.na(touch)) %>%
  group_by(trial, Selection) %>%
  summarize(mean_touch = mean(touch),
            lower = binom.test(sum(touch), n())$conf.int[1],
            upper = binom.test(sum(touch), n())$conf.int[2]) %>%
  {ggplot(., aes(trial, mean_touch, ymin = lower, ymax = upper, color=Selection, shape=Selection )) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 0.8, lty = 2) +
  geom_pointrange(position = position_dodge(0.5), alpha = 1, size=0.8) +
  labs(x = "Trials",y="Touch (mean)")+
  scale_color_manual(name = "Selection lines",
                     labels = c("Control", "Down", "Up"),
                     values = darkcolor)+
  scale_shape_manual(name = "Selection lines",
                     labels = c("Control", "Down", "Up"),
                     values=c(15,16,17))+
  scale_x_continuous(breaks=1:12)+
  theme_classic(base_size = 16)
}
m_plot+ theme_update(legend.justification = c(1, 0),
                     legend.position = c(0.9, 0.15))



##OUTPUT GLM: https://www.guru99.com/r-generalized-linear-model.html
#AIC (Akaike Information Criteria): This is the equivalent of R2 in logistic regression. It measures the fit when a penalty is applied to the number of parameters. Smaller AIC values indicate the model is closer to the truth.
#Null deviance: Fits the model only with the intercept. The degree of freedom is n-1. We can interpret it as a Chi-square value (fitted value different from the actual value hypothesis testing).
#Residual Deviance: Model with all the variables. It is also interpreted as a Chi-square hypothesis testing.
#Number of Fisher Scoring iterations: Number of iterations before converging.


