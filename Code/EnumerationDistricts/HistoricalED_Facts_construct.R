#This file repeats the empirical application for the enumeration districts data.
#Requires output from ED_IPUMS_MERGE.do

#Date created: June 22, 2022
#Date edited: June 22 2022

library(tidyverse)
library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(collapse)
library(ggplot2)

ED <- read_dta("Data/US_Data/Historical/Merged_IPUMS_ED_cleaned.dta")

#Demeaning housing unit density and population density by IPUMS metro
ED <- ED %>% group_by(metarea) %>% mutate(demeaned_housing_density = 
                                      housing_unit_density/mean(housing_unit_density)) %>%
                                   mutate(demeaned_population_density = 
                                      population_density/mean(population_density)) %>%
                                   mutate(mean_housing_density = mean(housing_unit_density))

#These two variables are very close to each other 

#Generating order variables
ED <- ED %>% group_by(metarea) %>% mutate(rank_hdensity_ED = 
                                           order(order(housing_unit_density, decreasing = FALSE))/(max(order(order(housing_unit_density, decreasing = FALSE))) + 1)) %>% 
                                  mutate(rank_pdensity_ED = 
                                           order(order(population_density, decreasing = FALSE))/(max(order(order(population_density, decreasing = FALSE))) + 1))

#Generating groupings of ED's based on underlying prices
metarea_prices <- ED %>% select(metarea, valueh)
metarea_prices <- metarea_prices[!duplicated(metarea_prices), ] #average house values for 46 metros
ED_quantile_landval <- quantile(metarea_prices$valueh, probs = seq(0, 1, 0.05), na.rm = TRUE)


#Running GGPlot

ggplot() + 
  geom_smooth(method = 'loess', span= 0.5, data = ED[ED$valueh >= as.numeric(ED_quantile_landval["75%"]),],
              aes(x=rank_hdensity_ED, y=demeaned_housing_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = ED[ED$valueh <= as.numeric(ED_quantile_landval["25%"]),] ,
              aes(x=rank_hdensity_ED, y=demeaned_housing_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked housing unit density (1930 Enumeration District Level)") +
  ylab("Housing Unit Density (Metro Area Average = 1)")
ggsave("Data/US_Data/Output/1930ED_dens_disp.png", width = 16, height = 10, units = "cm")
#Opposite happens-- if anything the bottom of the distribution have higher density downtowns. 

#raw plots (adjust scale)
ggplot() +
  geom_point(data =  ED[ED$valueh >= as.numeric(ED_quantile_landval["75%"]),], alpha = 0.25, size = 0.1,
             aes(x = rank_hdensity_ED, y=demeaned_housing_density, color = "Top 25%")) + 
  geom_point(data =  ED[ED$valueh <= as.numeric(ED_quantile_landval["25%"]),], alpha = 0.25, size = 0.1,
             aes(x = rank_hdensity_ED, y=demeaned_housing_density, color = "Bottom 25%")) + 
  ylim(0, 12) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) 

#raw plots comparing Birmingham and LA
ggplot() +
  geom_point(data =  ED[ED$metarea == 448,], alpha = 0.25, size = 0.1,
             aes(x = rank_hdensity_ED, y=demeaned_housing_density, color = "LA")) +
  geom_point(data =  ED[ED$metarea == 100,], alpha = 0.25, size = 0.1,
             aes(x = rank_hdensity_ED, y=demeaned_housing_density, color = "Birmingham")) + 
  ylim(0, 12)
  scale_colour_manual(name="Sample", values = c("red", "blue")) 


Test_top25 <- ED[ED$valueh >= as.numeric(ED_quantile_landval["75%"]),] #washington, chicago, LA, Boston, New York, New Haven, mostly "Old" cities
Test_bot25 <- ED[ED$valueh <= as.numeric(ED_quantile_landval["25%"]),] #Birmingham AL, Chatanooga, Seattle, San Antonio. (all cities that are not "old")

