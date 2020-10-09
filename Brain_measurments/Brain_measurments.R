#Title     : Brain measurmets
# Objective : Analyze the brain sizes differnces
# Created by: mariagranell
# Created on: 10/09/2020

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

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Brain_measurments")
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Telencephalon/R_scripts/Brain_measurments/Brain_measurments-Batch1.csv")
d<-data[complete.cases(data$line),]
d <- na.omit(d)
head(d)

ggplot(d, aes(x=tel, y=TotalBrain, color=line)) +
  geom_point(size=6) +
  theme_classic()


scatterplot(d$tel,d$TotalBrain)

table(d$sel)

darkcolor<-c("#00AFBB","#ff8657")
lightcolor<-c("#00AFBB","#FC4E07")

d<-d[d$ID>1,]
d$Selection<-ifelse(d$line == "A", "Down", "Up")


ggplot(d, aes(x=line, y=tel, fill=Selection)) +
  scale_fill_manual(name = " Selection lines",
                    labels = c("Down", "Up"),
                    values=darkcolor)+
  labs(x = "Selection line",y="Volume of the telencephalon")+
  scale_x_discrete(labels = c("Down","Up"))+
  theme_classic(base_size = 16)+
  geom_boxplot()



ggplot(d, aes(x=line, y=Total.OT, color=line)) +
  geom_boxplot()
ggplot(d, aes(x=line, y=Vol..Cer, color=line)) +
  geom_boxplot()
ggplot(d, aes(x=line, y=Vol..DM, color=line)) +
  geom_boxplot()
ggplot(d, aes(x=line, y=Total.Hyp, color=line)) +
  geom_boxplot()
ggplot(d, aes(x=line, y=Total.OB, color=line)) +
  geom_boxplot()
ggplot(d, aes(x=line, y=TotalBrain, color=line)) +
  geom_boxplot()