# Title     : LT Reversal
# Objective : Reversal memory analysis
# Created by: mariagranell
# Created on: 28/08/2020

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

#data for memory experiment------------------------------------
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Memory")
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Memory/LT_memory.csv")

data <- data[complete.cases(data$seconds),]
d<-data[data$id<100,]

d <- within(d, {
  id <- factor(id)
  replicate <- factor(replicate)
  selection <- factor(selection) })
table(d$selection)

d$trial_centered<-d$trial - 2
#taking all trials into account
Anova(model <- glmer(choice ~ selection  +trial_centered + assignedR + side +#the interactions are nor significant
  (1|id) ,
                     data = d,
                     na.action=na.exclude,
                     family=binomial,control=glmerControl(optimizer="bobyqa",
                                                          optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations

summary(model)

## model diagnostic
dd <- dotplot(ranef(model,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(model = model , terms = c("assignedR")))
test(emmeans(model, pairwise ~ assignedR))

#remember all trials
d %>%
  group_by(selection, assignedR) %>%
  dplyr::summarize(proportion=sum(choice)/length(choice),sd=sd(choice))%>%
  ggplot( aes(x = assignedR, y = proportion, colour=selection,
                ymin=proportion-sd, ymax=proportion+sd)) +
  geom_hline(yintercept=0.8, linetype="dashed", color = "red")+
  geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
  geom_linerange(position = position_dodge(width = 0.5)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  theme_classic()+
  labs(title = "Long term memory", subtitle = "Reversal learning")+
  ylab("proportion of correct choices(SE)")+
  xlab("color trained on")


#Approximation how to do it with CI
#d %>%
#  group_by(selection, assignedR) %>%
#  dplyr::summarize(proportion=sum(choice)/length(choice),conf1=first(CI(choice,0.99)), conf2=last(CI(choice, 0.99)))%>%
#  ggplot( aes(x = assignedR, y = proportion, colour=selection,
#              ymin=proportion+conf2, ymax=proportion-conf1)) +
#  geom_hline(yintercept=0.8, linetype="dashed", color = "red")+
#  geom_hline(yintercept=0.5, linetype="dashed", color = "black")+
#  geom_linerange(position = position_dodge(width = 0.5)) +
#  geom_pointrange(position = position_dodge(width = 0.5)) +
#  theme_classic()


  ## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = model)
plot(simulationOutput)
testDispersion(simulationOutput)

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(model)

