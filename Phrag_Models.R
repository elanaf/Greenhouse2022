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
#model specification probably okay because 12 obs and 3 blocks

ranef(mdf.m1)

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



##Relationships between traits and phrag cover 

##using means
traits1 <- greenhouse %>%
  filter(Date_Cleaned == "2022-05-16" & Phrag_Presence == "W") %>%
  select(Species, Block, Density, Cover.Native, Height.Native, Cover.Phrag) %>%
  group_by(Species, Density) %>%
  summarise(final.cover = mean(Cover.Native),
            final.height = mean(Height.Native, na.rm = TRUE),
            phrag.cover = mean(Cover.Phrag))

traits2 <- biomass %>%
  filter(Phrag_Presence == "W") %>%
  select(Species, Block, Density, Native.Biomass, Phrag.Biomass) %>%
  group_by(Species, Density) %>%
  summarise(final.biomass = mean(Native.Biomass, na.rm = TRUE),
            phrag.biomass = mean(Phrag.Biomass))

traits <- left_join(traits1, traits2, by = c("Species", "Density"))


b_b <- ggplot(traits, aes(x = final.biomass, y = phrag.biomass)) +
  geom_point() +
  labs(x = "Native Biomass", y = "P. australis Biomass")
b_c <- ggplot(traits, aes(x = final.cover, y = phrag.biomass)) +
  geom_point() +
  labs(x = "Native Cover", y = "P. australis Biomass")
b_h <- ggplot(traits, aes(x = final.height, y = phrag.biomass)) +
  geom_point() +
  labs(x = "Native Height", y = "P. australis Biomass")
c_b <- ggplot(traits, aes(x = final.biomass, y = phrag.cover)) +
  geom_point() +
  labs(x = "Native Biomass", y = "P. australis Cover")
c_c <- ggplot(traits, aes(x = final.cover, y = phrag.cover)) +
  geom_point() +
  labs(x = "Native Cover", y = "P. australis Cover")
c_h <- ggplot(traits, aes(x = final.height, y = phrag.cover)) +
  geom_point() +
  labs(x = "Native Height", y = "P. australis Cover")

lm_plots <- grid.arrange(b_b, b_c, b_h, c_b, c_c, c_h, ncol = 3)
ggsave("lm_plots.jpeg", lm_plots)

lm_cover_height <- lm(phrag.cover ~ final.height, data = traits)
summary(lm_cover_height)

lm_cover_biomass <- lm(phrag.cover ~ final.biomass, data = traits)
summary(lm_cover_biomass)

lm_cover_cover <- lm(phrag.cover ~ final.cover, data = traits)
summary(lm_cover_cover)

lm_biomass_height <- lm(phrag.biomass ~ final.height, data = traits)
summary(lm_biomass_height)

lm_biomass_biomass <- lm(phrag.biomass ~ final.biomass, data = traits)
summary(lm_biomass_biomass)

lm_biomass_cover <- lm(phrag.biomass ~ final.cover, data = traits)
summary(lm_biomass_cover)

#using the full dataset
final.traits <- greenhouse %>%
  filter(Date_Cleaned == "2022-05-16" & Phrag_Presence == "W") %>%
  select(Species, Block, Density, Cover.Native, Height.Native, Cover.Phrag)

final.biomass <- biomass %>%
  filter(Phrag_Presence == "W") %>%
  select(Species, Block, Density, Native.Biomass, Phrag.Biomass)


final.all <- left_join(final.traits, final.biomass, by = c("Species", "Density", "Block"))

lm_cover_height <- lm(Cover.Phrag ~ Height.Native + Density, data = final.all)
summary(lm_cover_height)

lm_cover_biomass <- lm(Cover.Phrag ~ Native.Biomass + Density, data = final.all)
summary(lm_cover_biomass)

lm_cover_cover <- lm(Cover.Phrag ~ Cover.Native + Density, data = final.all)
summary(lm_cover_cover)

lm_biomass_height <- lm(Phrag.Biomass ~ Height.Native + Density, data = final.all)
summary(lm_biomass_height)

lm_biomass_biomass <- lm(Phrag.Biomass ~ Native.Biomass + Density, data = final.all)
summary(lm_biomass_biomass)

lm_biomass_cover <- lm(Phrag.Biomass ~ Cover.Native + Density, data = final.all)
summary(lm_biomass_cover)
