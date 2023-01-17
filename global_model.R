#Load objects
load("main_dfs.RData")
library(magrittr)
library(tidyr)
library(ggplot2)
library(glmmTMB) #allows us to use a beta distribution
library(DHARMa)
library(emmeans)
library(car)

greenhouse$Density <- as.factor(greenhouse$Density)
greenhouse$Phrag_Presence <- as.factor(greenhouse$Phrag_Presence)
greenhouse$Species <- as.factor(greenhouse$Species)

options(contrasts = c("contr.sum", "contr.poly"))

#take out species with missing observations

#check <- greenhouse %>%
#filter(Date_Cleaned == "2022-05-16",
#       Cover.Native < 5.0)
#Species that did not really grow: JUTO LWO 1, JUGE LW 1, SYCI HW 1, SCAM LWO 1, 
#JUTO LW 2, SCAM LWO 2, SCAM LW 2, JUTO HW 2, JUGE LWO 2, JUTO HWO 3, BOMA LW 3, JUTO LW 3,
#JUTO LWO 3, SCAM LWO 3, BOMA HW 3, BOMA LWO 3, BOMA HWO 2, JUGE LW 3

#Native cover
mdf <- greenhouse %>%
  filter(!is.na(Density),
         Date_Cleaned == "2022-05-16",
         Species != "JUTO" & Species != "JUGE"  & Species != "SCAM" & Species != "BOMA")

mdf$Cover.Native[mdf$Cover.Native == 0] <- 0.005 #make 0s a trace amount - could be half the smallest amount

mdf.m1<- glmmTMB(Cover.Native ~ Phrag_Presence * Density * Species  #* for interaction
                 + (1|Block),
                 data = mdf,
                 family = beta_family)

summary(mdf.m1)
#everything is being compared to the reference species (BICE) with the default contrast but we changed it to sum
#intercept now represents the mean across all the species - re-read email about the contrasts
#the standard errors should be the same if everything is balanced

#Native biomass
table(biomass$Species)
mdf <- biomass %>%
  filter(!is.na(Density),
         Species != "JUTO" & Species != "JUGE"  & Species != "SCAM" & Species != "BOMA") %>%
  mutate(Species = factor(Species)) #need to remove the factor levels (species) that were removed
table(mdf$Species)
with(mdf, table(Species, Density, Phrag_Presence, useNA = "ifany"))
mdf_avg <- mdf %>% 
  group_by(Block, Phrag_Presence, Density, Species) %>%
  summarize(Native.Biomass = mean(Native.Biomass), 
            nobs = n()) %>%#to average the BICE where there is an extra observation
  ungroup() 
table(mdf_avg$nobs) #double check to make sure BICE is the only one with 2 observations
summary(mdf_avg$Native.Biomass) #no 0s so log shouldnt be a problem

mdf.m1 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density * Species #* for interaction
                  + (1|Block),
                  data = mdf_avg
)

summary(mdf.m1)

mdf.m1_res <- simulateResiduals(mdf.m1, plot = T)
#log was too much, sqrt not quite enough but better than nothing

useData <- drop_na(mdf_avg) #need to get rid of NAs
plotResiduals(mdf.m1_res, form= useData$Phrag_Presence)
#warning message because Phrag_Presence is a character, but it runs
#slight difference but good enough
plotResiduals(mdf.m1_res, form= useData$Density)
#variance slightly less in low density but again good enough
plotResiduals(mdf.m1_res, form= useData$Species)
#this is messier, fewer observations for each, some boxplots bigger than others, but only 12 observations
#species have different variances that the transformation doesn't stabilize
#library(car)
Anova(mdf.m1, type = 3) 
#there is evidence of a significant 3-way interaction - other interactions not interpretable because of 3 way
emmip(mdf.m1, Species~Density|Phrag_Presence, CIs = T)
emmip(mdf.m1, Phrag_Presence~Density|Species, CIs = T)
#a lot of species that phrag presence doesn't matter for, for some a pattern fo WO being on top
#probably no itneraction between density and phrag if the lines are parallel
#there is an interaction between density and phrag if the lines are not parallel
#three way interaction because only some species have the density x phrag_presence interaction

#now we test for all the pieces
#we want to know which species have an interaction and which don't, and which might have one effect
#then we want to know whether the interaction pattern is the same for each species
mdf.m1.emm <- emmeans(mdf.m1, ~Species * Density * Phrag_Presence)
pairs(mdf.m1.emm, by = c("Density", "Phrag_Presence"))
pairs(mdf.m1.emm, simple = "Species")

#a good way to check the contrast coding would be how each species looks individually compared to the contrast in the full model 
#the interaction effect is the phrag difference for high - the phrag difference for low - if we can get these, then we can compare - a contrast of contrasts
#contrast vignettes and interaction vignettes 



##if I do just look at the species specific models, it should be pretty similar but not exactly because
#it is now using contrasts only for that species instead of all of them