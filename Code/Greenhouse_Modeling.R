#Load objects
load("main_dfs.RData")
library(tidyverse)
library(glmmTMB) #allows us to use a beta distribution
library(DHARMa)
library(emmeans)
library(car)

greenhouse$Density <- as.factor(greenhouse$Density)
greenhouse$Phrag_Presence <- as.factor(greenhouse$Phrag_Presence)
greenhouse$Species <- as.factor(greenhouse$Species)


#check <- greenhouse %>%
#filter(Date_Cleaned == "2022-05-16",
#       Cover.Native < 5.0)
#Species that did not really grow: JUTO LWO 1, JUGE LW 1, SYCI HW 1, SCAM LWO 1, 
#JUTO LW 2, SCAM LWO 2, SCAM LW 2, JUTO HW 2, JUGE LWO 2, JUTO HWO 3, BOMA LW 3, JUTO LW 3,
#JUTO LWO 3, SCAM LWO 3, BOMA HW 3, BOMA LWO 3, BOMA HWO 2, JUGE LW 3

####Graphs to look quickly at species####

#how each species changes over time by density and presence of phrag - up close for each species
greenhouse %>%
  filter(!is.na(Density), Species == "SYCI") %>% #everything that is not NA for density 
  ggplot(aes(x = Date, y = Cover.Native, col = Block, group = Block)) +
  geom_point() + geom_line() +
  facet_wrap(~Density + Phrag_Presence)
#will need to do this individually for each species

#how each species changes over time by density and presence of phrag - up close for each species
biomass %>%
  filter(!is.na(Density), Species == "RUMA") %>% #everything that is not NA for density 
  ggplot(aes(x = Density, y = Native.Biomass, col = Block, group = Block)) +
  geom_point() + geom_line() +
  facet_wrap(~Density + Phrag_Presence)
#will need to do this individually for each species

####Example of how to model####
#use glmmtmb because will eventually use beta 

#this is for the type of test - needed for Type III test - also required you to use Anova () in car package
options(contrasts = c("contr.sum", "contr.poly"))

##Example
#model for a single species - the mean of native height WO and W, and Date
henu <- greenhouse %>%
  filter(Species == "HENU", !is.na(Density),
         Date == "2022-05-16" | Date == "2022-05-17")

henu.m1 <- glmmTMB(log(Height.Native) ~ Phrag_Presence * Density #* for interaction
                   + (1|Block),
                   data = henu,
                   family = gaussian
)
summary(henu.m1)
#model specification probably okay because 12 obs and 3 blocks


simulateResiduals(henu.m1, plot = T) #not a great fit, but only 12 obs
plotResiduals(henu.m1, form= henu$Phrag_Presence) #unequal variances, median is below so distribution is skewed
#some evidence that the residuals are not normal and the residuals are not even
#we can take the log to address this - but it didn't help in this case

library(car)
Anova(henu.m1) #no evidence of significant differences
emmip(henu.m1, Phrag_Presence~Density, CIs = T)
#looks like there is an interaction, but not once we add confidence intervals

#so appears that there is no difference on the last date between density or when there is or is not phrag
#but there isn't much power
 
##Model to run for Height ####
#Use this to go through all the species - results saved on google doc 

mdf <- greenhouse %>%
  filter(Species == "EPCI", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m1 <- glmmTMB(Height.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian)

summary(mdf.m1)
#model specification probably okay because 12 obs and 3 blocks


simulateResiduals(mdf.m1, plot = T) #not a great fit, but only 12 obs
plotResiduals(mdf.m1, form= mdf$Phrag_Presence)

#library(car)
Anova(mdf.m1) 
emmip(mdf.m1, Phrag_Presence~Density, CIs = T)

##Model to run for Cover ####
mdf <- greenhouse %>%
  filter(Species == "SOCA", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m1 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
                  )


summary(mdf.m1)
#model specification probably okay because 12 obs and 3 blocks


simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Phrag_Presence)

#A lot of the residual estimates look really bad but I checked with Susan and she said it is fine
#There is not a lot of variation in the data, so there are not a lot of residuals to be checked
#The tests and estimates themselves seem reasonable so it is okay

#library(car)
Anova(mdf.m1) 
emmip(mdf.m1, Phrag_Presence~Density, CIs = T)
emmip(mdf.m1, Density ~ Phrag_Presence, CIs = T)

##Model to run for biomass ####
mdf <- biomass %>%
  filter(Species == "EPCI", !is.na(Density))

mdf.m1 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)

summary(mdf.m1)
#model specification probably okay because 12 obs and 3 blocks

simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Phrag_Presence)

#library(car)
Anova(mdf.m1) 
emmip(mdf.m1, Phrag_Presence~Density, CIs = T)

emmeans(mdf.m1, pairwise ~ Density|Phrag_Presence)
emmeans(mdf.m1, pairwise ~ Phrag_Presence|Density)
