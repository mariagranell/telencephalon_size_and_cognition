# Title     : Bins
# Objective : Analyze the bins data
# Created by: mariagranell
# Created on: 24/08/2020

library(ggplot2)
library(magrittr)
library(dplyr)
library(survival)
library(survminer)
library(coxme)
library(emmeans)
library(car)
library(MASS)
library(lattice)
library(ggpubr)
require(cowplot)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Problem_solving")
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Problem_solving/Problem_solving.csv")

#redefine the class of each variable
data<-within(data,{
  id<-factor(id)
  replicate<-factor(replicate)
  selection<-factor(selection)})
table(data$selection)

#select the data for the survival analysis
d <- data[data$trial == 1,]
d <- d[complete.cases(d),]
table(d$selection)
d$Selection<-ifelse(d$selection == "A", "Down", ifelse(d$selection == "B", "Up","Control"))

## COPX MODEL
status <- 1*(d$totalbins<38) #select the females that manage to do it in 38 bins
surv_control <-Surv(d$totalbins,status)

fitC<-coxph(surv_control ~ selection +replicate, data = d)
summary(fitC)

#if you change Selection vs selection you have all the p-values
fitC.mixed<-coxme(surv_control ~ selection + (1|selection/replicate)  , data = d)
Anova(fitC.mixed)
summary(fitC.mixed)
emmeans(fitC.mixed, pairwise~selection)

##CHECKING THE ASSUMPTIONS
# so best thing is to run a coxph, check the assumptions and then report the results from the coxme

#to test the assumption of proportional hazards. Equal probability of succeeding within groups.
test.model<-cox.zph(fitC)
test.model
#test is not statistically significant for each of the covariates, and the global test is also not statistically significant.
#Therefore, we can assume the proportional hazards.

# Graphical diagnosis
plot(test.model)
ggcoxzph(test.model)
ggforest(fitC, data = d) #fav graph

ggcoxdiagnostics(fitC, type="deviance",
                 ox.scale="linear.predictions")
ggcoxdiagnostics(fitC, type="deviance",linear.predictions = FALSE, ggtheme_bw = theme_bw())

# Visualize the dfbeta values
dfbeta <- residuals(fitC, type="dfbeta")
par(mfrow=c(1, 1))
for (j in 1:2) {
  plot(dfbeta[, j], ylab=names(coef(fitC))[j])
  abline(h=0, lty=2)
}

## GRAPH
model<-survfit(Surv(d$totalbins,status)~d$selection)#to check the mean

#theme <- theme(axis.line = element_line(colour = "black"),
#               panel.grid.major = element_line(colour = "grey90"),
#               panel.grid.minor = element_line(colour = "grey98"))
#               panel.border = element_blank(),
#               panel.background = element_blank())

darkcolor<-c("#00AFBB","#ff4a00","#ffee00")
lightcolor<-c("#00AFBB","#FC4E07", "#f5db74")
ggsurvplot(model, data = d, xlab="Number of bins", ylab="Proportion of successful individuals",
           surv.median.line = "hv", # Add medians survival
           linetype = c("solid", "twodash","dotdash"), #friedly to colour blind


           xlim=c(0,37), #to put limitd to the x axis

           # Change legends: title & labels
           legend= c(0.9, 0.3), #coordinates legend
           legend.title = "Selection lines",
           legend.labs = c("Down", "Up","Control"),
           # Add p-value and tervals
           pval = TRUE,
           pval.coord = c(0, 0.75), #coordinates
           conf.int = TRUE,
           conf.int.alpha= 0.15,

           break.time.by=1, #to put all the bins

           # Add risk table
           #risk.table = TRUE,
           #tables.height = 0.2,
           #tables.theme = theme_cleantable(),
           #risk.table.title= "N° of individuals that did not succeded",

           # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
           # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
           palette = darkcolor,
           ggtheme = theme_classic(base_size = 16), # Change ggplot2 theme

           fun = "event" #to draw the plot "upside down". "cumhaz" plots the cumulative hazard function, and "pct" for survival probability in percentage.
)

#What to report for a survival analysis:
#https://www.researchgate.net/post/What_to_report_from_a_Cox_Proportional_Hazards_Regression_analysis