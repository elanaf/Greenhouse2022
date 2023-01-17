#Call objects
load("main_dfs.RData")

##Graphing
library(ggplot2)
library(magrittr)
library(dplyr)

####Graphs for native species model####

#how does native height change over time by density and presence of phrag
height_native <- greenhouse %>% 
  filter(Species != "PHAU") %>%
  #pull out the phrag panel because no data
  ggplot(aes(x = Date_Cleaned, y = Height.Native, color = Phrag_Presence, shape = Density)) +
  #using the means of the blocks
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
               fun = mean, geom = "point", size = 2) +
  #error bars added
  stat_summary(aes(group = interaction(Density, Phrag_Presence), width = .5),
               fun.data = mean_se, geom = "errorbar") +
  #add a line to connect the dates
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
              fun = mean, geom = "line") +
  facet_wrap(~Species) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = "Date", y = "Native Height (cm)", color = "Phragmites Presence", shape = "Density") +
  scale_color_hue(labels = c('Present', 'Absent')) + #change the legend labels
  scale_shape(labels = c("High", "Low")) 

height_native

ggsave(filename = "height_native.jpeg", 
       device = "jpeg")

#if I wanted to do just low density
# greenhouse %>% 
#   filter(Density == "L")%>%
#   ggplot(aes(x = Date, y = Height.Native, color = Phrag_Presence)) +
#   #using the means of the blocks
#   stat_summary(aes(group = Phrag_Presence),
#                fun = mean, geom = "point") +
#   #error bars added
#   stat_summary(aes(group = Phrag_Presence, width = .5),
#                fun.data = mean_se, geom = "errorbar") +
#   facet_wrap(~Species)

#how does native cover change over time by density and presence of phrag
cover_native <- greenhouse %>% 
  filter(Species != "PHAU") %>%
  ggplot(aes(x = Date_Cleaned, y = Cover.Native, color = Phrag_Presence, shape = Density)) +
  #using the means of the blocks
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
               fun = mean, geom = "point", size = 2) +
  #error bars added
  stat_summary(aes(group = interaction(Density, Phrag_Presence), width = .5),
               fun.data = mean_se, geom = "errorbar") +
  #add a line to connect the dates
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
               fun = mean, geom = "line") +
  facet_wrap(~Species) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = "Date", y = "Native Cover (%)", color = "Phragmites Presence", shape = "Density") +
  scale_color_hue(labels = c('Present', 'Absent')) + #change the legend labels
  scale_shape(labels = c("High", "Low"))

cover_native

ggsave(filename = "cover_native.jpeg", 
       device = "jpeg")

#if I wanted to do just low density
# greenhouse %>% 
#   filter(Density == "L")%>%
#   ggplot(aes(x = Date, y = Cover.Native, color = Phrag_Presence)) +
#   #using the means of the blocks
#   stat_summary(aes(group = Phrag_Presence),
#                fun = mean, geom = "point") +
#   #error bars added
#   stat_summary(aes(group = hrag_Presence, width = .5),
#                fun.data = mean_se, geom = "errorbar") +
#   facet_wrap(~Species)


#how does native biomass change by density and presence of phrag
biomass_native <- biomass %>% 
  filter(Species != "PHAU") %>%
  ggplot(aes(x = factor(Density, levels = rev(levels(factor(Density)))),#make L first
             y = Native.Biomass, color = Phrag_Presence)) +
  #using the means of the blocks
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
               fun = mean, geom = "point", size = 2, shape = 1) +
  #error bars added
  stat_summary(aes(group = interaction(Density, Phrag_Presence), width = .5),
               fun.data = mean_se, geom = "errorbar") +
  facet_wrap(~Species) +
  labs(x = "Density", y = "Native Biomass (g)", color = "Phragmites Presence") +
  scale_color_hue(labels = c('Present', 'Absent')) #change the legend labels

biomass_native

ggsave(filename = "biomass_native.jpeg", 
       device = "jpeg")


#if I wanted to do just high density
# biomass %>%
#   filter(Density == "H")%>%
#   ggplot(aes(x = Phrag_Presence, y = Native.Biomass, color = Phrag_Presence)) +
#    #using the means of the blocks
#   stat_summary(aes(group = Phrag_Presence),
#                 fun = mean, geom = "point") +
#      #error bars added
#      stat_summary(aes(group = Phrag_Presence, width = .5),
#                   fun.data = mean_se, geom = "errorbar") +
#      facet_wrap(~Species)

####Graphs about Phrag####

#how does phrag height change over time by density
greenhouse$Species <- factor(greenhouse$Species, #make PHAU last
                     levels = c("BICE", 'BOMA', 'DISP', 'EPCI', 'EUMA',
                                'EUOC', 'HENU', 'JUAR', 'JUGE', 'JUTO',
                                'MUAS', 'PUNU', 'RUMA', 'SCAC', 'SCAM',
                                'SCPU', 'SOCA', 'SYCI', 'PHAU'))
height_phrag <- greenhouse %>%
  ggplot(aes(x = Date_Cleaned, y = Height.Phrag, color= Density)) +
  #using the means of the blocks
  stat_summary(aes(group = Density),
               fun = mean, geom = "point") +
  #add a line to connect the dates
  stat_summary(aes(group = Density),
               fun = mean, geom = "line") +
  #error bars added
  stat_summary(aes(group = Density, width = .5),
               fun.data = mean_se, geom = "errorbar") +
  facet_wrap(~Species) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = "Date", y = "Phragmites Height (cm)", color = "Native Seeding Density") +
  scale_color_hue(labels = c("High", "Low", "Control")) 

height_phrag

ggsave(filename = "height_phrag.jpeg", 
       device = "jpeg")


#how does phrag cover change over time by density
# greenhouse$Species <- factor(greenhouse$Species, #make PHAU last
#                              levels = c("BICE", 'BOMA', 'DISP', 'EPCI', 'EUMA',
#                                         'EUOC', 'HENU', 'JUAR', 'JUGE', 'JUTO',
#                                         'MUAS', 'PUNU', 'RUMA', 'SCAC', 'SCAM',
#                                         'SCPU', 'SOCA', 'SYCI', 'PHAU'))
cover_phrag <- greenhouse %>%
  ggplot(aes(x = Date_Cleaned, y = Cover.Phrag, color= Density)) +
  #using the means of the blocks
  stat_summary(aes(group = Density),
               fun = mean, geom = "point") +
  #add a line to connect the dates
  stat_summary(aes(group = Density),
               fun = mean, geom = "line") +
  #error bars added
  stat_summary(aes(group = Density, width = .5),
               fun.data = mean_se, geom = "errorbar") +
  facet_wrap(~Species) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = "Date", y = "Phragmites Cover (%)", color = "Native Seeding Density") +
  scale_color_hue(labels = c("High", "Low", "Control")) 

cover_phrag

ggsave(filename = "cover_phrag.jpeg", 
       device = "jpeg")


#how does phrag biomass change by density
# greenhouse$Species <- factor(greenhouse$Species, #make PHAU last
#                              levels = c("BICE", 'BOMA', 'DISP', 'EPCI', 'EUMA',
#                                         'EUOC', 'HENU', 'JUAR', 'JUGE', 'JUTO',
#                                         'MUAS', 'PUNU', 'RUMA', 'SCAC', 'SCAM',
#                                         'SCPU', 'SOCA', 'SYCI', 'PHAU'))
biomass_phrag <- biomass %>% 
  filter(Phrag_Presence == "W") %>%
  ggplot(aes(x = factor(Density, levels = rev(levels(factor(Density)))),#make L first
             y = Phrag.Biomass, color = Density)) +
  #using the means of the blocks
  stat_summary(aes(group = Density),
               fun = mean, geom = "point", size = 2) +
  #error bars added
  stat_summary(aes(group = Density, width = .5),
               fun.data = mean_se, geom = "errorbar") +
  facet_wrap(~Species) +
  labs(x = "Density", y = "Phragmites Biomass (g)", color = "Density") +
  scale_color_hue(labels = c('High', 'Low', "Control")) #change the legend labels

biomass_phrag

ggsave(filename = "biomass_phrag.jpeg", 
       device = "jpeg")

####Other Graphs####
#how does native height change by density over time (without phrag)
greenhouse %>%
  filter(Phrag_Presence == "WO") %>%
  ggplot(aes(x = Date, y = Height.Native, color = Density)) +
  geom_point() +
  facet_wrap(~Species)

#how does native cover change by density over time (without phrag)
greenhouse %>%
  filter(Phrag_Presence == "WO") %>%
  ggplot(aes(x = Date, y = Cover.Native, color = Density)) +
  geom_point() +
  facet_wrap(~Species)

#how does native biomass change by density (without phrag)
biomass %>%
  filter(Phrag_Presence == "WO") %>%
  ggplot(aes(x = Density, y = Native.Biomass, color = Density)) +
  geom_point() +
  facet_wrap(~Species)



#how each species changes over time by density and presence of phrag - up close for each species
greenhouse %>%
  filter(!is.na(Density), Species == "DISP") %>% #everything that is not NA for density 
  ggplot(aes(x = Date, y = Cover.Native, col = Block, group = Block)) +
  geom_point() + geom_line() +
  facet_wrap(~Density + Phrag_Presence)
  #will need to do this individually for each species



#Tallest species
greenhouse %>%
  filter(Phrag_Presence == "W",
         Date == "2022-05-16" | Date == "2022-05-17") %>%
  dplyr::arrange(desc(Height.Native)) %>%
  head(20)
  
greenhouse %>%
  filter(Phrag_Presence == "WO",
         Date == "2022-05-16" | Date == "2022-05-17") %>%
  dplyr::arrange(desc(Height.Native)) %>%
  head(20)

#Most cover
greenhouse %>%
  filter(Phrag_Presence == "W",
         Date == "2022-05-16" | Date == "2022-05-17") %>%
  dplyr::arrange(desc(Cover.Native)) %>%
  head(22)

greenhouse %>%
  filter(Phrag_Presence == "WO",
         Date == "2022-05-16" | Date == "2022-05-17") %>%
  dplyr::arrange(desc(Cover.Native)) %>%
  head(19)

#lowest phrag cover
greenhouse %>%
  filter(Phrag_Presence == "W",
         Date == "2022-05-16" | Date == "2022-05-17") %>%
  dplyr::arrange(Cover.Phrag) %>%
  head(13)

#most biomass

biomass %>%
  filter(Phrag_Presence == "W") %>%
  dplyr::arrange(desc(Native.Biomass)) %>%
  head(20)

biomass %>%
  filter(Phrag_Presence == "WO") %>%
  dplyr::arrange(desc(Native.Biomass)) %>%
  head(20)

#least phrag biomass

biomass %>%
  filter(Phrag_Presence == "W") %>%
  dplyr::arrange(Phrag.Biomass) %>%
  head(20)


####How much did phrag cover/etc. actually reduce?####
##New graphs that show how phrag changes over time compared to what is expected
#create a new, smaller dataset to work with
final.data <- greenhouse %>%
                filter(Date == "2022-05-16" | Date == "2022-05-17") %>% 
                select(Tub, Species, Density, Block, Phrag_Presence,
                       Date_Cleaned, Cover.Native, Cover.Phrag,
                       Height.Native, Height.Phrag) #only select the columns I will need to work with


#make different datasets based on block to make calculations easier
final.data.b1 <- greenhouse %>%
                  filter(Date == "2022-05-16" | Date == "2022-05-17",#only the last sampling date
                          Phrag_Presence == "W", #only the ones that had phrag grow with them
                         Block == 1) %>% #only block 1
                  select(Tub, Species, Density, Block, 
                          Date_Cleaned, Cover.Native, Cover.Phrag,
                          Height.Native, Height.Phrag) #only select the columns I will need to work with
final.data.b2 <- greenhouse %>%
  filter(Date == "2022-05-16" | Date == "2022-05-17",#only the last sampling date
         Phrag_Presence == "W", #only the ones that had phrag grow with them
         Block == 2) %>% #only block 1
  select(Tub, Species, Density, Block, 
         Date_Cleaned, Cover.Native, Cover.Phrag,
         Height.Native, Height.Phrag) #only select the columns I will need to work with


final.data.b3 <- greenhouse %>%
  filter(Date == "2022-05-16" | Date == "2022-05-17",#only the last sampling date
         Phrag_Presence == "W", #only the ones that had phrag grow with them
         Block == 3) %>% #only block 3
  select(Tub, Species, Density, Block, 
         Date_Cleaned, Cover.Native, Cover.Phrag,
         Height.Native, Height.Phrag) #only select the columns I will need to work with

#mutate them to have the reduction value, based on the control in each block
#first, get the phrag height and cover values of the control
control.matrix <- final.data %>%
                      filter(Species == "PHAU")
height.control.1 <- control.matrix$Height.Phrag[1]
cover.control.1 <- control.matrix$Cover.Phrag[1]

height.control.2 <- control.matrix$Height.Phrag[2]
cover.control.2 <- control.matrix$Cover.Phrag[2]

height.control.3 <- control.matrix$Height.Phrag[3]
cover.control.3 <- control.matrix$Cover.Phrag[3]
                      
#now mutate the block datasets to get the reduction value
final.data.b1 <- final.data.b1 %>%
                mutate(P.Height.Red = 
                            (final.data.b1$Height.Phrag - height.control.1)/height.control.1,
                      P.Cover.Red = 
                            (final.data.b1$Cover.Phrag - cover.control.1)/cover.control.1)

final.data.b2 <- final.data.b2 %>%
  mutate(P.Height.Red = 
           (final.data.b2$Height.Phrag - height.control.2)/height.control.2,
         P.Cover.Red = 
           (final.data.b2$Cover.Phrag - cover.control.2)/cover.control.2)

final.data.b3 <- final.data.b3 %>%
  mutate(P.Height.Red = 
            (final.data.b3$Height.Phrag - height.control.3)/height.control.3,
          P.Cover.Red = 
            (final.data.b3$Cover.Phrag - cover.control.3)/cover.control.3)

#combine back into 1 dataset to graph
combine1 <- bind_rows(final.data.b1, final.data.b2)
final.data.red <- bind_rows(combine1, final.data.b3)

#and now for biomass
#make different datasets based on block to make calculations easier
biomass.b1 <- biomass %>%
  filter(Phrag_Presence == "W", #only the ones that had phrag grow with them
         Block == 1) %>% #only block 1
  select(Tub, Species, Density, Block, 
         Phrag.Biomass) #only select the columns I will need to work with

biomass.b2 <- biomass %>%
  filter(Phrag_Presence == "W", #only the ones that had phrag grow with them
         Block == 2) %>% #only block 1
  select(Tub, Species, Density, Block, 
         Phrag.Biomass) #only select the columns I will need to work with

biomass.b3 <- biomass %>%
  filter(Phrag_Presence == "W", #only the ones that had phrag grow with them
         Block == 3) %>% #only block 1
  select(Tub, Species, Density, Block, 
         Phrag.Biomass) #only select the columns I will need to work with

#mutate them to have the reduction value, based on the control in each block
#first, get the phrag height and cover values of the control
control.matrix <- biomass %>%
  filter(Species == "PHAU")
biomass.control.1 <- control.matrix$Phrag.Biomass[1]
biomass.control.2 <- control.matrix$Phrag.Biomass[2]
biomass.control.3 <- control.matrix$Phrag.Biomass[3]

#now mutate the block datasets to get the reduction value
biomass.b1 <- biomass.b1 %>%
  mutate(P.Biomass.Red = 
           (biomass.b1$Phrag.Biomass - biomass.control.1)/biomass.control.1)

biomass.b2 <- biomass.b2 %>%
  mutate(P.Biomass.Red = 
           (biomass.b2$Phrag.Biomass - biomass.control.2)/biomass.control.2)

biomass.b3 <- biomass.b3 %>%
  mutate(P.Biomass.Red = 
           (biomass.b3$Phrag.Biomass - biomass.control.3)/biomass.control.3)

#combine back into 1 dataset to graph
combine1 <- bind_rows(biomass.b1, biomass.b2)
final.biomass.red <- bind_rows(combine1, biomass.b3)

#and graph to see if there are any major differences
#Phrag Height
# final.data.red %>%
#   ggplot(aes(x = Species, y = P.Height.Red, fill = Density)) +
#   stat_summary(aes(group = Density),
#                fun = mean, geom = "bar", 
#                position = position_dodge(0.95)) +
#   stat_summary(aes(group = Density, width = .5),
#                fun.data = mean_se, geom = "errorbar",
#                position = position_dodge(0.95)) 

final.data.red %>%
  ggplot(aes(x = Species, y = P.Height.Red, color = Density)) +
  stat_summary(aes(group = Density),
               fun = mean, geom = "point", 
               position = position_dodge(0.95)) +
  stat_summary(aes(group = Density, width = .5),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.95)) 

#Phrag Cover
# final.data.red %>%
#   ggplot(aes(x = Species, y = P.Cover.Red, fill = Density)) +
#   stat_summary(aes(group = Density),
#                fun = mean, geom = "bar", 
#                position = position_dodge(0.95)) +
#   stat_summary(aes(group = Density, width = .5),
#                fun.data = mean_se, geom = "errorbar",
#                position = position_dodge(0.95)) 

cover.red <- final.data.red %>%
  filter(Species != "PHAU") %>%
  ggplot(aes(x = Species, y = P.Cover.Red, color = Density)) +
  stat_summary(aes(group = Density),
               fun = mean, geom = "point", 
               position = position_dodge(0.95)) +
  stat_summary(aes(group = Density, width = .5),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.95)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9))+
  labs(y = "Percent Change in Phrag Cover")

cover.red

ggsave(filename = "cover_reduction.jpeg", 
       device = "jpeg")

#Biomass
# final.biomass.red %>%
#   ggplot(aes(x = Species, y = P.Biomass.Red, fill = Density)) +
#   stat_summary(aes(group = Density),
#                fun = mean, geom = "bar", 
#                position = position_dodge(0.95)) +
#   stat_summary(aes(group = Density, width = .5),
#                fun.data = mean_se, geom = "errorbar",
#                position = position_dodge(0.95)) 

biomass.red <- final.biomass.red %>%
  filter(Species != "PHAU") %>%
  ggplot(aes(x = Species, y = P.Biomass.Red, color = Density)) +
  stat_summary(aes(group = Density),
               fun = mean, geom = "point", 
               position = position_dodge(0.95)) +
  stat_summary(aes(group = Density, width = .5),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.95)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(y = "Percent Change in Phrag Biomass")

biomass.red

ggsave(filename = "biomass_reduction.jpeg", 
       device = "jpeg")

#Get the actual numbers
final_means_cover <- final.data.red %>%
  filter(Species != "PHAU") %>%
  group_by(Species, Density) %>%
  summarise(trt_mean = mean(P.Cover.Red, na.rm = TRUE))

final_means_biomass <- final.biomass.red %>%
  filter(Species != "PHAU") %>%
  group_by(Species, Density) %>%
  summarise(trt_mean = mean(P.Biomass.Red, na.rm = TRUE))

