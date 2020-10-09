# Title     : Reversal
# Objective : Analyze reversal learning
# Created by: mariagranell
# Created on: 25/08/2020

#they learned the reversal better with yellow

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

#centering the trials
d$trial_centered<-d$trial-33  #middle trial

Anova(full.model <- glmer(choice ~ selection * (trial_centered + colour) + #the interactions are nor significant
  (trial_centered|id) + (trial_centered|replicate/selection),
                          data = d,
                          na.action=na.exclude,
                          family=binomial,control=glmerControl(optimizer="bobyqa",
                                                               optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
summary(full.model)

Anova(simple.model <- glmer(choice ~ selection + trial_centered + colour +
  (trial_centered|id) + (trial_centered|replicate),
                          data = d,
                          na.action=na.exclude,
                          family=binomial,control=glmerControl(optimizer="bobyqa",
                                                               optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
Anova(simple.model1 <- glmer(choice ~ selection + trial_centered + colour +
  (1|id) + (1|replicate),
                            data = d,
                            na.action=na.exclude,
                            family=binomial,control=glmerControl(optimizer="bobyqa",
                                                                 optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations

anova(simple.model1,simple.model, type=3) #is significant, suggesting a better fit for the simple.model
summary(simple.model)

## model diagnostic
dd <- dotplot(ranef(simple.model,condVar=TRUE))
do.call(grid.arrange,c(dd,list(nrow=1))) #to extract estimates of the random effects
#plotting the residuals in glm is not very useful

# Plot of marginal effects for significant results
plot(ggeffect(simple.model, terms = c("selection")))
test(emmeans(simple.model, pairwise ~ selection))

plot(ggeffect(simple.model, terms = c("colour")))
test(emmeans(simple.model, pairwise ~ colour))

## PERFECT TO CHECK MODEL DIAGNOSIS IN GLM. CHECK OVERDISPERSION. CHECK LINK IF MORE INFORMATION NEEDED.
#library(DHARMa) #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulationOutput <- simulateResiduals(fittedModel = simple.model)
plot(simulationOutput)
testDispersion(simulationOutput) #the test is not significant, is okei the overfitting

## Explained varaince (pseudo-R2 for binomial models) (theoretical R2 for binomial models)
#library(MuMIn) #https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/comment-page-1/
r.squaredGLMM(simple.model)

#END PERFORMANCES

#So in each collum I will have the difference between performances. If the number has a - means that they are performing worst
#And i would like to have the number that approximates the most to just 0. Which mean that there is no difference between the numbers and that they have reached
#their top performance

EndPerformance(d)

d2A <- d[between(d$trial, 60, 66),] #to run summary detach dplyr
summarySE(data = d2A, groupvars = c("selection"), conf.interval = 0.95, .drop = TRUE,
          measurevar = "choice", na.rm = T)

dA <- d[d$selection=="A",]
dB <- d[d$selection=="B",]
dend<-dB[dB$trial>25,]
dend<-d[d$trial>34,]
dend$trial_centered<-dend$trial- 50 #middle trial

Anova(simple.model <- glmer(choice ~   selection +trial_centered + colour  +
  (1|id) + (1|replicate),
                            data = dend,
                            na.action=na.exclude,
                            family=binomial,control=glmerControl(optimizer="bobyqa",
                                                                 optCtrl=list(maxfun=2e5)))) #extend the maximum number of iterations
summary(simple.model)
r.squaredGLMM(simple.model)

summarySE(data = dend, groupvars = c("selection", "colour"), conf.interval = 0.95, .drop = TRUE,
          measurevar = "choice", na.rm = T)

d$Selection<-ifelse(d$selection == "A", "Down", "Up")

#fist plot--------------------------------
selection_names<-c("A"="Down-selected","B"="Up-selected")
d%>%
  filter(!is.na(choice)) %>%
  group_by(trial, colour, selection) %>%
  summarize(mean_choice = mean(choice),
            lower = binom.test(sum(choice), n())$conf.int[1],
            upper = binom.test(sum(choice), n())$conf.int[2]) %>%
  {ggplot(., aes(trial, mean_choice, ymin = lower, ymax = upper, col = colour, shape= colour)) +
  geom_pointrange(position = position_dodge(0.5), alpha = 0.7) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 0.8, lty = 2) +
  scale_color_manual(values = c('red', 'gold')) +
  facet_grid(~selection,labeller = as_labeller(selection_names)) +
  theme_classic()+
  ggtitle('Average success over time (95% CI)', subtitle = "Reversal learning")+
  xlab("Trial")+ ylab("Proportion of correct choices")}


##second plot--------------------------------------------


d$Selection<-ifelse(d$selection == "A", "Down", "Up")

m <- glmer(choice ~ trial_centered + Selection + colour + trial_centered:Selection + trial_centered:colour +(1|id) + (1|replicate), family = binomial, data = d, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
Anova(m, type = 3)

m_fit <- effect("trial_centered:Selection", m, xlevels = 66) %>%
  as.data.frame()

m_means <- d%>%
  filter(!is.na(choice)) %>%
  group_by(trial, Selection) %>%
  summarize(mean_choice = mean(choice))

darkcolor<-c("#00AFBB","#ff4a00")
lightcolor<-c("#00AFBB","#FC4E07")

m_plot<-ggplot(m_means, aes(x=trial, y= mean_choice, colour = Selection, shape=Selection)) +
  geom_point(size = 3.5)+
  geom_line(data = m_fit, aes(trial_centered+33, fit))  +
  geom_ribbon(data = m_fit, col = NA, alpha = 0.15, size = 4,
              aes(trial_centered+33, NULL, ymin = lower, ymax = upper, fill=Selection)) +
  scale_fill_manual(name = " Selection lines",
                    labels = c("Down", "Up"),
                    values=darkcolor) +
  scale_shape_manual(name = " Selection lines",
                     labels = c("Down", "Up"),
                     values=c(16,17))+
  labs(x = "Trials",y="Proportion of correct choices")+
  scale_x_continuous(breaks=seq(2,66,2))+
  geom_hline(yintercept = 0.5, lty = 2, alpha= 1) +
  geom_hline(yintercept = 0.8, lty = 5, alpha=1) +
  scale_colour_manual(name= " Selection lines",
                      labels = c("Down", "Up"),
                      values=darkcolor) +
  ylim(0.4,1)+
  theme_classic(base_size = 16)


m_plot+ theme_update(legend.justification = c(1, 0),
                     legend.position = c(0.9, 0.3))

colores<-c("#7c2c2b", "#00729f")

setwd("~/Downloads")
dt<- read.csv("~/Downloads/tel_rlR.csv", header = T)
filter<- dt$fish!=70
dt<-dt[filter,]

dt$trial_centered<-dt$trial-33
m <- glmer(success ~ trial_centered + line + r_colour + trial_centered:line + trial_centered:r_colour +(1|fish) + (1|rep), family = binomial, data = dt, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
m_fit <- effect("trial_centered:line", m, xlevels = 66) %>%
  as.data.frame()

m_means <- dt%>%
   filter(!is.na(success)) %>%
   group_by(trial, line) %>%
   summarize(mean_choice = mean(success))
m_plot<-ggplot(m_means, aes(x=trial, y= mean_choice, colour = line, shape=line)) +
   geom_point(size = 3.5)+
   geom_line(data = m_fit, aes(trial_centered+33, fit))  +
   geom_ribbon(data = m_fit, col = NA, alpha = 0.15, size = 4,
               aes(trial_centered+33, NULL, ymin = lower, ymax = upper, fill=line)) +
   scale_fill_manual(values=colores) +
   labs(x = "Trials",y="Proportion of correct choices")+
   scale_x_continuous(breaks=seq(2,66,2))+
   geom_hline(yintercept = 0.5, lty = 2, alpha= 1) +
   geom_hline(yintercept = 0.8, lty = 5, alpha=1) +
   scale_colour_manual(values=colores) +
   ylim(0.0,1)+
   theme_classic(base_size = 16)

m_plot + theme_update(legend.justification = c(1, 0),
                     legend.position = c(0.9, 0.3))



