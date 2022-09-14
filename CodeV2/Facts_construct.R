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

#Date created: August 26th, 2022
#This file replicates the facts used in the paper for the 2016-2020 pooled ACS. 
#Data_CensusACS_BlockGroup_Construct.R

#importing data
load(file = "DataV2/US_Data/Output/Constructed_Block.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_med_house_price.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")

#NOTE: because of RAM issues, one could take a random subset of the top25 sample to calculate standard errors. (pattern holds for non random sample)
set.seed = 1234 #To reproduce random sample
top25 <- US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]
top25 <- top25[sample(nrow(top25), floor(nrow(top25)/3)),] #take approx 33% of the sample, 32k observations

bot25 <- US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]

#HOUSING UNIT DENSITY (FACT 1)
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
ggsave("DataV2/US_Data/Output/blockdens_dist.png", width = 20, height = 12, units = "cm") 

#INCOME SORTING ON DENSITY (FACT 2) -- as we suspected: some gentrification downtown in the past few years. 
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = top25, aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = bot25, aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)")
ggsave("DataV2/US_Data/Output/income.png", width = 16, height = 10, units = "cm")

#Checking alternative specifications
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = top25, aes(x=rank_density_CBSA, y=demeaned_Avg_Income, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = bot25, aes(x=rank_density_CBSA, y=demeaned_Avg_Income, color = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (MSA Average == 1)") #Same deal


#Linear regressions (i.e. regression with maximum smoothing?)
summary(lm(demeaned_log_Income ~ rank_density_CBSA, data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]))
summary(lm(demeaned_log_Income ~ rank_density_CBSA, data = bot25))

summary(lm(demeaned_Avg_Income ~ rank_density_CBSA, data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]))
summary(lm(demeaned_Avg_Income ~ rank_density_CBSA, data = bot25))

summary(lm(demeaned_Housing_density ~ rank_density_CBSA, data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]))
summary(lm(demeaned_Housing_density ~ rank_density_CBSA, data = bot25))

#Is this the correct way to chop up the data? 