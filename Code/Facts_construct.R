
#PACKAGES
library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(ggplot2)
library(estimatr)

#Date edited: June 26, 2022
#This file gives you additional facts post Jan 22. Requires output from 
                                      #Data_CensusACS_block_construct.R

#importing data
load(file = "Data/US_Data/Output/Constructed_2010_Tract.Rdata")
load(file = "Data/US_Data/Output/CBSA_med_house_price.Rdata")
load(file = "Data/US_Data/Output/CBSA_quantiles.Rdata")
load(file = "Data/US_Data/Output/CBSA_quantiles_popdens.Rdata")


##____FACTS:_____________________________________________________________________________
## Structure types don't give you the complete picture. Its the combination of structure type mandates (zoning) + minimum
## lot size restrictions. Can we say something about differences in the distribution of housing unit density in superstars?
## Answer is yes. 

# Differences in the housing density distribution: medium density BLOCKs are relatively less dense in superstars, but the highest density tracts are of high density.
#These are not small differences. 

#NOTE: because of RAM issues, one could take a random subset of the top25 sample to calculate standard errors. 
set.seed = 1234 #To reproduce random sample
top25 <- US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]
top25 <- top25[sample(nrow(top25), floor(nrow(top25)/3)),] #take approx 33% of the sample, 43k observations

bot25 <- US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]

#Construct inverse weights for robustness
#calculating inverse weights by CBSA (to weight each city equally)
bot25 <- bot25 %>% group_by(CBSA) %>% mutate(citywt = 1/n())
top25 <- top25 %>% group_by(CBSA) %>% mutate(citywt = 1/n())
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED %>% group_by(CBSA) %>% mutate(citywt = 1/n()) 

ggplot() + 
  geom_smooth(method = 'loess' , data = top25,
              aes(x=rank_density_CBSA, y=demeaned_Housing_density, colour = 'Top 25%'), se = TRUE,
              span = 0.5) + 
  geom_smooth(method = 'loess', data = bot25,
              aes(x=rank_density_CBSA, y=demeaned_Housing_density, colour = 'Bottom 25%'), se = TRUE,
              span = 0.5) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Housing unit density (MSA Average = 1)")
ggsave("Data/US_Data/Output/blockdens_dist.png", width = 20, height = 12, units = "cm") 

#What does Houston look like? CBSA CODE 
ggplot() + 
  geom_smooth(method = 'loess', span= 0.5, data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA == 26420,],
              aes(x=rank_density_CBSA, y=demeaned_Housing_density, colour = 'Houston')) +
  geom_smooth(method = 'loess', span = 0.5, data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
              aes(x=rank_density_CBSA, y=demeaned_Housing_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Housing unit density (MSA Average = 1)")
ggsave("Data/US_Data/Output/blockdens_dist_houston.png", width = 20, height = 12, units = "cm") 


#___________________________________________________________________
# How do structure types play a role? 
#Single family
ggplot() +
  geom_smooth(method = 'loess', span = 0.5, data = top25,
              aes(x=rank_density_CBSA, y=demeaned_Single_family_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = bot25,
              aes(x=rank_density_CBSA, y=demeaned_Single_family_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Single family housing density component")
ggsave("Data/US_Data/Output/singlefamily_dist.png", width = 20, height = 12, units = "cm") 


#The Middle
ggplot() +
  geom_smooth(method = 'loess', span = 0.5, data = top25,
              aes(x=rank_density_CBSA, y=demeaned_Middle_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = bot25,
              aes(x=rank_density_CBSA, y=demeaned_Middle_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("2-19 unit building density component")
  ggsave("Data/US_Data/Output/219building_dist.png", width = 20, height = 12, units = "cm")

#Middle Split 1: Considering only duplexes-quadplexes (Aradhya's suggestion)
ggplot() +
  geom_smooth(method = 'loess', span = 0.5, data = top25,
                aes(x=rank_density_CBSA, y=demeaned_Middlesp1_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = bot25,
                aes(x=rank_density_CBSA, y=demeaned_Middlesp1_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("2-4 unit building density component")
  ggsave("Data/US_Data/Output/24building_dist.png", width = 20, height = 12, units = "cm")
  
  
#20+ building density 
ggplot() +
  geom_smooth(method = 'loess', span = 0.5, data = top25,
              aes(x=rank_density_CBSA, y=demeaned_Building_20_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = bot25,
              aes(x=rank_density_CBSA, y=demeaned_Building_20_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("20+ unit building density component")
  ggsave("Data/US_Data/Output/20building_dist.png", width = 20, height = 12, units = "cm")
  
#All other buildings   
ggplot() +
  geom_smooth(method = 'loess', span = 0.5, data = top25,
              aes(x=rank_density_CBSA, y=demeaned_Building_2_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = bot25,
              aes(x=rank_density_CBSA, y=demeaned_Building_2_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("All other buildings (2+ Units)")
ggsave("Data/US_Data/Output/2plusbuilding_dist.png", width = 20, height = 12, units = "cm") 
  
#ROBUSTNESS  
#Single Family Density Margin
ggplot() +
    geom_smooth(method = 'loess', span = 0.5, data = top25,
                aes(x=rank_density_CBSA, y=log(demeaned_SingleF_Density_Margin), colour = 'Top 25% Density Margin')) +
    geom_smooth(method = 'loess', span = 0.5, data = bot25,
                aes(x=rank_density_CBSA, y=log(demeaned_SingleF_Density_Margin), colour = 'Bottom 25% Density Margin')) +
    scale_colour_manual(name="Sample", values = c("red", "blue")) +
    xlab("Ranked housing unit density (Block Group level)") +
    ylab("Log(Single Family Density Margin)") 
ggsave("Data/US_Data/Output/SingleFamilyDensity.png", width = 20, height = 12, units = "cm")

ggplot() +
    geom_smooth(method = 'loess', span = 0.5, data = top25,
                aes(x=rank_density_CBSA, y=log(demeaned_SingleF_Land_Margin), colour = 'Top 25% Land Margin')) +
    geom_smooth(method = 'loess', span = 0.5, data = bot25,
                aes(x=rank_density_CBSA, y=log(demeaned_SingleF_Land_Margin), colour = 'Bottom 25% Land Margin')) + 
    scale_colour_manual(name="Sample", values = c("red", "blue")) +
    xlab("Ranked housing unit density (Block Group level)") +
    ylab("Log(Single Family Land Margin)") 
    ggsave("Data/US_Data/Output/SingleFamilyLand.png", width = 20, height = 12, units = "cm")
    
    
#These differences in distribution also accompany stronger sorting on income in superstars
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = top25, aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = bot25, aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)")
ggsave("Data/US_Data/Output/income.png", width = 16, height = 10, units = "cm")

#linear regressions
summary(lm_robust(demeaned_log_Income ~ rank_density_CBSA, 
                  data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]))
summary(lm_robust(demeaned_log_Income ~ rank_density_CBSA, 
                  data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]))

summary(lm_robust(demeaned_log_Income ~ rank_density_CBSA, 
                  data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]),
                  weights = citywt)
summary(lm_robust(demeaned_log_Income ~ rank_density_CBSA, 
                  data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
                  weights = citywt))
#alternative definitions
summary(lm_robust(demeaned_log_Income ~ rank_density_CBSA, 
                  data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),]),
                  )
summary(lm_robust(demeaned_log_Income ~ rank_density_CBSA, 
                  data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),],
                  ))

summary(lm_robust(demeaned_log_Income ~ rank_density_CBSA, 
                  data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["50.0%"]),]),
)
summary(lm_robust(demeaned_log_Income ~ rank_density_CBSA, 
                  data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["50.0%"]),],
))

#Very robust fact in 2010! 


#Robustness: does this hold after controlling for average household size + census family status? In a linear regression
lm_inc_top25 <- lm_robust(demeaned_log_Income ~ demeaned_log_Average_HH_size + demeaned_FamilyShare + rank_density_CBSA,
                          data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),])
lm_inc_bot25 <- lm_robust(demeaned_log_Income ~ demeaned_log_Average_HH_size + demeaned_FamilyShare + rank_density_CBSA,
                          data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),])

#Race
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = top25, aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = bot25, aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log White Share (demeaned by MSA)")
ggsave("Data/US_Data/Output/race.png", width = 16, height = 10, units = "cm")



#ROBUSTNESS: POPULATION DENSITY
ggplot() + 
  geom_smooth(method = 'loess', span= 0.5, data = top25,
              aes(x=rank_popdensity_CBSA, y=demeaned_Pop_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = bot25,
              aes(x=rank_popdensity_CBSA, y=demeaned_Pop_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked Population density (Block Group level)") +
  ylab("Population Density (MSA Average = 1)") 
ggsave("Data/US_Data/Output/blockpopdens_dist.png", width = 20, height = 12, units = "cm") 

