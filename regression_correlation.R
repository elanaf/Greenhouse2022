#Call objects
load("main_dfs.RData")
func_group <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Greenhouse2022/Cleaned_Data/Groups_Final.csv")

##Graphing
library(ggplot2)
library(magrittr)
library(dplyr)
library(patchwork)
library(emmeans)

####Relationships between traits and phrag cover ####

#using the full dataset
final.traits <- greenhouse %>%
  filter(Date_Cleaned == "2022-05-16" & Phrag_Presence == "W") %>%
  select(Species, Block, Density, Cover.Native, Height.Native, Cover.Phrag)

final.biomass <- biomass %>%
  filter(Phrag_Presence == "W") %>%
  select(Species, Block, Density, Native.Biomass, Phrag.Biomass)

final <- left_join(final.traits, final.biomass, by = c("Species", "Density", "Block"))
final.all <- left_join(final, func_group, by = c("Species"))

#Regressions ####

# lm_cover_height <- lm(Cover.Phrag ~ Height.Native + Density, data = final.all)
# summary(lm_cover_height)

# lm_biomass_height <- lm(Phrag.Biomass ~ Height.Native + Density, data = final.all)
# summary(lm_biomass_height)

lm_cover_biomass <- lm(Cover.Phrag ~ Native.Biomass + Density + Group, data = final.all)
summary(lm_cover_biomass)
#Only biomass significant but Susan says to leave in things that are not 
#significant because we can then show that they weren’t significant
#In fact, we can see that there might be some trends with my groups
#can use ANOVA function to see the group level as a whole


lm_cover_cover <- lm(Cover.Phrag ~ Cover.Native + Density + Group, data = final.all)
summary(lm_cover_cover)


lm_biomass_biomass <- lm(Phrag.Biomass ~ Native.Biomass + Density + Group, data = final.all)
summary(lm_biomass_biomass)
#group seems to be significant for this one but only this one - bulrush, rush (make sense because they are 0), and grass (idk...)
emmeans(lm_biomass_biomass, pairwise~Group)
#shows the contrasts, adjusted for type 1 error - talk to Maddie about adjusting
#default is the Tukey method

#Might be worth standardizing - she thought it could be good, she will send me a paper 
#easy to do with the scale function in R

## correlation ####
#might be better if i just want to show a relationship between the two things
#scale centers and scales by default
lm_bb_correlation <- lm(scale(Phrag.Biomass) ~ scale(Native.Biomass), data = final.all)
cor(final.all$Phrag.Biomass, final.all$Native.Biomass, method = "spearman", use = "complete.obs")
cor(final.all$Phrag.Biomass, final.all$Native.Biomass, method = "pearson", use = "complete.obs")
#cor different from scale because top is scaling across the missing values but cor is using a slightly different
#dataset by removing the NAs - so really we should make a different complete dataset and then scale 
#recommended to report the spearman or the pearson, line 534 is problematic because of the NAs
#if the NAs are removed from 534 then we should get the pearson value, but easier to use the other two 
#tells us that when standardized, we get the correlation as the slope
#spearman is a pearson correlation on the ranked data

cor.test(final.all$Phrag.Biomass, final.all$Native.Biomass, 
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#gives you the conifidence interval, only for the pearson
#might be other functions that can give you for the spearman
#r(100) = -0.56, p = 7.052e-10

cor.test(final.all$Cover.Phrag, final.all$Native.Biomass, 
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(100) = -0.61, p = 1.352e-11

cor.test(final.all$Cover.Phrag, final.all$Cover.Native, 
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(106) = -0.60, p = 9.93e-12

cor.test(final.all$Phrag.Biomass, final.all$Cover.Native, 
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(108) = -0.60, p = 5.097e-12

lm_biomass_cover <- lm(Phrag.Biomass ~ Cover.Native + Density + Group, data = final.all)
summary(lm_biomass_cover)

#Graphs ####
#when graphing, we want to see whether the rule is the same for all the groups
#if not, that would suggest an interaction

b_b <- ggplot(final.all, aes(x = Native.Biomass, y = Phrag.Biomass)) +
  geom_point(aes(color = Group), size = 3) +
  theme(axis.title.y = ggtext::element_markdown())+
  labs(x = "Native Biomass", y = "*Phragmites* Biomass", color = "Functional Group") +
  geom_smooth(method="lm", se=FALSE, fullrange = TRUE) +
  ylim(0, 50) +
  theme(legend.position = "none")
b_b

ggsave(filename = "lm_native-biomass_phrag-biomass.jpeg", 
       device = "jpeg")

b_c <- ggplot(final.all, aes(x = Cover.Native, y = Phrag.Biomass)) +
  geom_point(aes(color = Group), size = 3) +
  theme(axis.title.y = ggtext::element_markdown())+
  labs(x = "Native Cover", y = "*Phragmites* Biomass", color = "Functional Group") +
  geom_smooth(method="lm", se=FALSE, fullrange = TRUE) +
  ylim(0, 50)+
  theme(legend.position = "none")
b_c

ggsave(filename = "lm_native-cover_phrag-biomass.jpeg", 
       device = "jpeg")


# b_h <- ggplot(traits, aes(x = final.height, y = phrag.biomass)) +
#   geom_point() +
#   labs(x = "Native Height", y = "P. australis Biomass")

c_b <- ggplot(final.all, aes(x = Native.Biomass, y = Cover.Phrag, group = Group, color = Group)) +
  geom_point( size = 3) +
  theme(axis.title.y = ggtext::element_markdown())+
  labs(x = "Native Biomass", y = "*Phragmites* Cover",color = "Functional Group")+
  geom_smooth(se=FALSE, fullrange = TRUE, span = 1.5)+
  theme(legend.position = "none") 
#facet_wrap(~Group, scales = "free_x")
c_b

#negative relationship between forbs and phrag but not the rushes - looks a little positive
#but probably not significant
#we can't tell with the rushes because they grew so little
#the two forbs seem to have the same relationship
#the grass we don't know because they didn't have high enough biomass, but because of interactions
#probably don't need the interaction in this model but could add it and check 

ggsave(filename = "lm_native-biomass_phrag-cover.jpeg", 
       device = "jpeg")


c_c <- ggplot(final.all, aes(x = Cover.Native, y = Cover.Phrag)) +
  geom_point(aes(color = Group), size = 3) +
  theme(axis.title.y = ggtext::element_markdown())+
  labs(x = "Native Cover", y = "*Phragmites* Cover", color = "Functional Group")+
  geom_smooth(method="lm", se=FALSE, fullrange = TRUE)+
  theme(legend.position = "none")
c_c

ggsave(filename = "lm_native-cover_phrag-cover.jpeg", 
       device = "jpeg")


(c_b + c_c) / (b_b + b_c)
ggsave(filename = "phrag_native_biomass_cover_graphs_all.jpeg",
       device = "jpeg")
