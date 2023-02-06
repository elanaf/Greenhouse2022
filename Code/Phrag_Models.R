load("main_dfs.RData")
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(glmmTMB) #allows us to use a beta distribution
library(DHARMa)
library(emmeans)
library(car)
library(multcompView)
library(gridExtra)
options(contrasts = c("contr.sum", "contr.poly"))

greenhouse$Density <- as.factor(greenhouse$Density)
greenhouse$Species <- as.factor(greenhouse$Species)

mdf <- greenhouse %>%
  filter(Species != "PHAU", Phrag_Presence == "W",
         Date_Cleaned == "2022-05-16")

##plotResiduals wasn't working so Susan sent me this code to figure out why
##You cannot have any missing values or 0 observations

#see what variables have missing values
mdf %>% 
  select(Cover.Phrag, Species, Density, Block) %>%
  summarise_all(list(~sum(is.na(.))))

#drop observations with NA Cover.Phrag and refactor so that level is dropped
with(mdf, table(Species, useNA = "ifany")) #no observations for PHAU

mdf <- mdf %>%
  drop_na(Cover.Phrag) %>%
  mutate(Species = factor(Species))

with(mdf, table(Species, useNA = "ifany")) #note absence of Phrag level

#back to model
mdf.m1 <- glmmTMB(Cover.Phrag ~ Species * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)

summary(mdf.m1)

simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Species) 

#library(car)
car::Anova(mdf.m1) 
emmip(mdf.m1, Species~Density, CIs = T)

#to see which species Density mattered for - but results don't match what I would have expected?
emmeans(mdf.m1, pairwise ~ Density | Species)

emm <- emmeans(mdf.m1, pairwise ~ Species, adjust = "tukey", type = "response")
pwpm(emm)
data1 <- multcomp::cld(emm$emmeans, alpha = 0.05, Letters = letters)
data1

##Model to run for biomass
mdf <- biomass %>%
  filter(Species != "PHAU", Phrag_Presence == "W")

mdf.m1 <- glmmTMB(sqrt(Phrag.Biomass) ~ Species * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)

summary(mdf.m1)
#model specification probably okay because 12 obs and 3 blocks

simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Density)

#library(car)
Anova(mdf.m1) 
emmip(mdf.m1, Species~Density, CIs = T)

emm <- emmeans(mdf.m1, pairwise ~ Species, adjust = "tukey", type = "response")
data2 <- multcomp::cld(emm$emmeans, alpha = 0.05, Letters = letters)
data2

