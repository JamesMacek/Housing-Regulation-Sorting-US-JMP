
#PACKAGES_____________
library(sf)
library(collapse)
library(plm)
library(haven)
library(pacman)
library(tidyr)
library(dplyr)
library(readr)
library(tmap)
library(ggplot2)
library(readxl)
library(stringr)
library(geosphere)
library(ggrepel)
library(data.table)
library(estimatr)
library(broom)
library(labelled)

#This file gives you additional facts post Jan 22
#WORKING DIRECTORY
setwd("Z:/Dropbox/SchoolFolder/Projects/Zoning/Us_Data")


#importing data
load(file = "Constructed_2010_Tract.Rdata")
load(file = "CBSA_med_house_price.Rdata")
load(file = "CBSA_quantiles.Rdata")


#Importing ACS data 2008-2012

ACS_2010 <- read_stata("ACS PUMS/ACS_data_cleaned.dta")

#Group by top25 sample 

ACS_2010["Sample"] <- rep("", nrow(ACS_2010))
ACS_2010 <- ACS_2010 %>% 
                 mutate(Sample = replace(Sample, top25 == 1, "Top 25%")) %>%
                 mutate(Sample = replace(Sample, top25 == 0, "Bottom 25%")) %>%
                 mutate(Sample = replace(Sample, top25 == 2, "New York MSA")) %>%
                 mutate(Sample = replace(Sample, top25 == 3, "San Francisco MSA"))

ACS_2010["Housing_Units_Type"] <- rep("", nrow(ACS_2010))
ACS_2010 <- ACS_2010 %>% 
  mutate(Housing_Units_Type = replace(Housing_Units_Type, unitsstr == 3, "Single Family, detached")) %>%
  mutate(Housing_Units_Type = replace(Housing_Units_Type, unitsstr == 4, "Single Family, attached")) %>%
  mutate(Housing_Units_Type = replace(Housing_Units_Type, unitsstr == 5, "2 Family Building")) %>%
  mutate(Housing_Units_Type = replace(Housing_Units_Type, unitsstr == 6, "3-4 Family Building")) %>%
  mutate(Housing_Units_Type = replace(Housing_Units_Type, unitsstr == 7, "5-9 Family Building")) %>%
  mutate(Housing_Units_Type = replace(Housing_Units_Type, unitsstr == 8, "10-19 Family Building")) %>%
  mutate(Housing_Units_Type = replace(Housing_Units_Type, unitsstr == 9, "20-49 Family Building")) %>%
  mutate(Housing_Units_Type = replace(Housing_Units_Type, unitsstr == 10, "50+ Family Building"))

#Constructing culmulative shares 
ACS_2010["units_culm_share"] <- rep(0, nrow(ACS_2010))

for (i in 3:10) {
  ACS_2010[ACS_2010$unitsstr == i & ACS_2010$top25 == 0,]$units_culm_share <- sum(ACS_2010[ACS_2010$unitsstr <= i & ACS_2010$top25 == 0, ]$units_share)
  ACS_2010[ACS_2010$unitsstr == i & ACS_2010$top25 == 1,]$units_culm_share <- sum(ACS_2010[ACS_2010$unitsstr <= i & ACS_2010$top25 == 1, ]$units_share)
}  


##_____FACT 1: ___________________________________________________________________________
## There exist differences in the distribution of housing types in superstars. Most of the differences come from
## comparatively more supply in large structures. Structure type appears to facilitate stronger sorting on income in superstars.

#Super clear: most of the relative differences coming from 20-49 buildings + 50+ buildings. Omitting single detached from graph
ggplot(ACS_2010, aes(reorder(Housing_Units_Type, unitsstr), units_share)) +
  geom_bar(aes(fill = Sample), position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  xlab("Structure Type") +
  ylab("Share of Housing Units in Structure Type") + 
  ggsave("Structure_type_dist.png", width = 16, height = 10, units = "cm")

#Now: plotting average income! we see comparatively lower income in middle buildings. 
#That is, 
ggplot(ACS_2010[ACS_2010$top25 <= 1,], aes(reorder(Housing_Units_Type, unitsstr), demeaned_hhincome)) +
  geom_bar(aes(fill = Sample), position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  xlab("Structure Type") +
  ylab("Average household income (demeaned by sample)") + 
  ggsave("Structure_type_income.png", width = 16, height = 10, units = "cm")

#Race
ggplot(ACS_2010[ACS_2010$top25 <= 1,], aes(reorder(Housing_Units_Type, unitsstr), demeaned_white)) +
  geom_bar(aes(fill = Sample), position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  xlab("Structure Type") +
  ylab("White Share (demeaned by sample)") + 
  ggsave("Structure_type_race.png", width = 16, height = 10, units = "cm") #not much differences in racial sorting on higher density structures. 

##____FACT 2:_____________________________________________________________________________
## Structure types don't give you the complete picture. Its the combination of structure type mandates (zoning) + minimum
## lot size restrictions. Can we say something about differences in the distribution of housing unit density in superstars?
## Answer is yes. 

# Differences in the housing density distribution: medium density tracts are relatively less dense in superstars, but the highest density tracts are of high density.
#These are not small differences. 
ggplot() + 
  geom_smooth(method = 'loess', span= 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
              aes(x=rank_density_CBSA, y=demeaned_Housing_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
              aes(x=rank_density_CBSA, y=demeaned_Housing_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Tract level)") +
  ylab("Housing unit density (MSA Average = 1)") +
  ggsave("tractdens_dist.png", width = 20, height = 12, units = "cm") 

#What does Houston look like? CBSA CODE 
ggplot() + 
  geom_smooth(method = 'loess', span= 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA == 26420,],
              aes(x=rank_density_CBSA, y=demeaned_Housing_density, colour = 'Houston')) +
  geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
              aes(x=rank_density_CBSA, y=demeaned_Housing_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Tract level)") +
  ylab("Housing unit density (MSA Average = 1)") +
  ggsave("tractdens_dist_houston.png", width = 20, height = 12, units = "cm") 


#___________________________________________________________________
# How do structure types play a role? 
#Single family
ggplot() +
  geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
              aes(x=rank_density_CBSA, y=demeaned_Single_family_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
              aes(x=rank_density_CBSA, y=demeaned_Single_family_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Tract level)") +
  ylab("Single family housing density component") +
  ggsave("singlefamily_dist.png", width = 20, height = 12, units = "cm") 


#The Middle
ggplot() +
  geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
              aes(x=rank_density_CBSA, y=demeaned_Middle_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
              aes(x=rank_density_CBSA, y=demeaned_Middle_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Tract level)") +
  ylab("2-19 unit building density component")
  ggsave("219building_dist.png", width = 20, height = 12, units = "cm")

  
#20+ building density 
ggplot() +
  geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
              aes(x=rank_density_CBSA, y=demeaned_Building_20_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
              aes(x=rank_density_CBSA, y=demeaned_Building_20_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Tract level)") +
  ylab("20+ unit building density component")
  ggsave("20building_dist.png", width = 20, height = 12, units = "cm")
  
#Single Family Density Margin (looks good, but what does it even mean? Is this getting at something? What is this?)
ggplot() +
    geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
                aes(x=rank_density_CBSA, y=log(demeaned_SingleF_Density_Margin), colour = 'Top 25% Density Margin')) +
    geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
                aes(x=rank_density_CBSA, y=log(demeaned_SingleF_Density_Margin), colour = 'Bottom 25% Density Margin')) +
    scale_colour_manual(name="Sample", values = c("red", "blue")) +
    xlab("Ranked housing unit density (Tract level)") +
    ylab("Log(Single Family Density Margin)") 
ggsave("SingleFamilyDensity.png", width = 20, height = 12, units = "cm")

ggplot() +
    geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
                aes(x=rank_density_CBSA, y=log(demeaned_SingleF_Land_Margin), colour = 'Top 25% Land Margin')) +
    geom_smooth(method = 'loess', span = 0.5, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
                aes(x=rank_density_CBSA, y=log(demeaned_SingleF_Land_Margin), colour = 'Bottom 25% Land Margin')) + 
    scale_colour_manual(name="Sample", values = c("red", "blue")) +
    xlab("Ranked housing unit density (Tract level)") +
    ylab("Log(Single Family Land Margin)") 
    ggsave("SingleFamilyLand.png", width = 20, height = 12, units = "cm")
    
#These differences in distribution also accompany stronger sorting on income in superstars
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Tract level)") +
  ylab("Log Average Income (demeaned by MSA)") +
  ggsave("income.png", width = 16, height = 10, units = "cm")

#Robustness: does this hold after controlling for average household size + census family status? In a linear regression
lm_inc_top25 <- lm_robust(demeaned_log_Income ~ demeaned_log_Average_HH_size + demeaned_FamilyShare + rank_density_CBSA,
                          data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),])
lm_inc_bot25 <- lm_robust(demeaned_log_Income ~ demeaned_log_Average_HH_size + demeaned_FamilyShare + rank_density_CBSA,
                          data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),])

#Race
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Tract level)") +
  ylab("Log White Share (demeaned by MSA)") +
  ggsave("race.png", width = 16, height = 10, units = "cm")

#FACT 3_____________________________________________________________________
#These differences are reflected in interesting patterns in the housing supply elasticity
#non-demeaned. missing middle have the smallest supply elasticities!
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_BSH_Elasticity, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_BSH_Elasticity, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Tract level)") +
  ylab("Housing unit supply elasticities (demeaned by MSA)") +
ggsave("supplyelasticity.png", width = 16, height = 10, units = "cm")

