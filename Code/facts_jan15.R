
#PACKAGES_____________
library(sf)
library(collapse)
library(plm)
library(haven)
library(pacman)
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

#This file gives you the initial facts sent to Nate on Jan 15th.
#WORKING DIRECTORY
setwd("Z:/Dropbox/SchoolFolder/Projects/Zoning/Us_Data")

#importing data
load(file = "Constructed_2010_Tract.Rdata")
load(file = "CBSA_med_house_price.Rdata")
load(file = "CBSA_quantiles.Rdata")

#_______________________________________________________________________________________________________________________________________________________________________________________________________________________
#FACT 1: Superstar cities have more "sprawl" relative to cheap cities -- that is-- high density housing at the bottom of the distribution. 
#This is driven by the detached housing margin. 
#_______________________________________________________________________________________________________________________________________________________________________________________________________________________

#Top 10% of MSAs
ggplot() + 
  geom_smooth(method = 'loess', span= 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density), color = 'red') +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density), color = 'blue')

#75vs bot 25?
ggplot() + 
  geom_smooth(method = 'loess', span= 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red")) +
  ggsave("tractdens_dist.png", width = 16, height = 10, units = "cm")

#Is the density gradient driven by single detached housing? 
#This "flatter" geography manifests itself in a flatter density gradient of detached housing units. This is where the Missing Middle comes in! 
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_detached_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_detached_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red")) +
  ggsave("detached_dens_dist.png", width = 16, height = 10, units = "cm")

#Is the density gradient also driven by attached housing units? NO! -- i.e. semi detached, row, apartments. 
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_attached_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_attached_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red")) +
  ggsave("attached_dens_dist.png", width = 16, height = 10, units = "cm")

#try single vs multifamily
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_single_family_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_single_family_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_multi_family_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_multi_family_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

#________________________________________________________________________________________________________________________________________________________________________________________
#FACT 2: In the lower densities (where "sprawl" occurs), superstar cities have  a relatively 
#1) higher share of whites, and 
#2) higher income when compared with cheap cities.
#3) Inverse relationships between quality adjusted housing price (self reported)
#4) 
#________________________________________________________________________________________________________________________________________________________________________________________
#Who is occupying the large stretch of uniform density in superstars?

#White: Top 10% of MSAs
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share), color = 'blue')

#Top 75% of MSAs
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Top 25%'))  +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Bottom 25%')) + 
  scale_colour_manual(name="legend", values = c("blue", "red")) +
  ggsave("white_share.png", width = 16, height = 10, units = "cm")

#Calculating MSE associated in each sample
models_WS <- do(US_TRACT_2010_JOINED, glance(lm(demeaned_log_White_share ~ rank_density_CBSA, data = .)))
mean(models_WS$r.squared) #20 percent average 
rm(models_WS)

#Income
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income), color = 'blue')

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red")) + 
  ggsave("income.png", width = 16, height = 10, units = "cm")

#Calculating MSE associated in each sample regression
models_INC_glance <- do(US_TRACT_2010_JOINED, glance(lm(demeaned_log_Income ~ rank_density_CBSA, data = .)))
models_INC_tidy <- do(US_TRACT_2010_JOINED, tidy(lm(demeaned_log_Income ~ rank_density_CBSA, data = .)))
models_INC_glance <- left_join(models_INC_glance, US_CBSA_2010_c, by = "CBSA")
models_INC_tidy <- left_join(models_INC_tidy, US_CBSA_2010_c, by = "CBSA")

mean(models_INC$r.squared) #16 percent with lots of heterogeneity 

#Question: adjust income for household size (robust result) 
noctrl <- lm_robust(Average_income ~ rank_density_CBSA, 
                    data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], fixed_effects = CBSA)
ctrl <- lm_robust(Average_income ~ rank_density_CBSA + Average_HH_size + Share_household_type2, 
                  data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], fixed_effects = CBSA)

noctrl_bot25 <- lm_robust(Average_income ~ rank_density_CBSA, 
                          data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], fixed_effects = CBSA)
ctrl_bot25 <- lm_robust(Average_income ~ rank_density_CBSA + Average_HH_size + Share_household_type2, 
                        data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], fixed_effects = CBSA)


#Question: repeat distributional exercise with white share distributions, income distributions, etc to see if there is actually more income/white sorting.  
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% group_by(CBSA) %>% mutate(rank_Whiteshare_CBSA = order(order(demeaned_log_White_share, decreasing = FALSE))/max(order(order(demeaned_log_White_share, decreasing = FALSE))))#Have to run order twice for some reason for it to work. Dividing by max_order to make plot manageable
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% group_by(CBSA) %>% mutate(rank_Income_CBSA = order(order(demeaned_log_Income, decreasing = FALSE))/max(order(order(demeaned_log_Income, decreasing = FALSE))))#Have to run order twice for some reason for it to work. Dividing by max_order to make plot manageable

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_Whiteshare_CBSA, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_Whiteshare_CBSA, y=demeaned_log_White_share), color = 'blue')
#Same Idea!

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_Income_CBSA, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_Income_CBSA, y=demeaned_log_Income), color = 'blue')
#Same idea (slightly more income segregation)

#For house prices (these are top-censored at 1m--this an issue?)
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue), color = 'red') +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue), color = 'blue')

ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red")) +
  ggsave("housingprice.png")

#For residualized housing prices
#Rising real estate density gradient in superstars after adjustment.
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_adjusted_housevalue,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_adjusted_housevalue, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("adjusted_housevalues.png", width = 16, height = 10, units = "cm")

#Rent? Sheds light on a different story!
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_gross_rent,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_gross_rent, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

#Quality adjusted rents follow same idea (though standard errors are large that we can't rule it out)
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_adjusted_rent,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_adjusted_rent, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("adjusted_rents.png", width = 16, height = 10, units = "cm")

#So this implies a clear pattern for housing quality (rent - quality adjusted rent)
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_gross_rent - demeaned_log_adjusted_rent,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_gross_rent - demeaned_log_adjusted_rent, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue - demeaned_log_adjusted_housevalue,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue - demeaned_log_adjusted_housevalue, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

#________________________________________________________________________________________________________________
#FACT 3: Housing consumption at all tracts in superstars appear to be more income constrained!
#
#Superstar sample FE regression
US_TRACT_2010_JOINED_naf3 <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$log_income_to_rent)) &
                                                    !(is.na(US_TRACT_2010_JOINED$Average_HH_size)) &
                                                    !(is.na(US_TRACT_2010_JOINED$Share_household_type2)), ]

#Regressing y on x1, x2
constrained_reg_rent_top25_y <- lm_robust(log_income_to_rent ~ rank_density_CBSA + Average_HH_size + Share_household_type2, 
                                          data = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], fixed_effects = CBSA)

y_top25_resid <- as.vector(US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$log_income_to_rent -
                             constrained_reg_rent_top25_y$fitted.values + constrained_reg_rent_top25_y$coefficients[1]*US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$rank_density_CBSA)

#adjusting y upward by average fixed effect in sample
y_top25_resid <- y_top25_resid + (mean(as.vector(constrained_reg_rent_top25_y$fixed_effects)) - mean(y_top25_resid))*rep(1, length(y_top25_resid))

#For bottom
constrained_reg_rent_bot25_y <- lm_robust(log_income_to_rent ~ rank_density_CBSA + Average_HH_size + Share_household_type2, 
                                          data = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], fixed_effects = CBSA)

y_bot25_resid <- as.vector(US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$log_income_to_rent -
                             constrained_reg_rent_bot25_y$fitted.values + constrained_reg_rent_bot25_y$coefficients[1]*US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$rank_density_CBSA)

#adjusting y upward by average fixed effect in sample
y_bot25_resid <- y_bot25_resid + (mean(as.vector(constrained_reg_rent_bot25_y$fixed_effects)) - mean(y_bot25_resid))*rep(1, length(y_bot25_resid))

#Time to plot both!

#We Got em!!!!
ggplot() +
  geom_smooth(method = 'lm', aes(x = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$rank_density_CBSA, y = y_top25_resid, color = 'Top 25%')) +
  geom_smooth(method = 'lm', aes(x = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$rank_density_CBSA, y = y_bot25_resid, color = 'Bottom 25%')) +
  scale_colour_manual(name = "legend", values = c("blue", "red")) +
  labs(x = "Ranked CBSA density", y = "-log(spending share on rent)")
ggsave("rent_stringency.png", width = 16, height = 10, units = "cm")

#Do the same for housing prices
US_TRACT_2010_JOINED_naf3 <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$log_income_to_value)) &
                                                    !(is.na(US_TRACT_2010_JOINED$Average_HH_size)) &
                                                    !(is.na(US_TRACT_2010_JOINED$Share_household_type2)), ]

#Regressing y on x1, x2
constrained_reg_rent_top25_y <- lm_robust(log_income_to_value ~ rank_density_CBSA + Average_HH_size + Share_household_type2, 
                                          data = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], fixed_effects = CBSA)

y_top25_resid <- as.vector(US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$log_income_to_value -
                             constrained_reg_rent_top25_y$fitted.values + constrained_reg_rent_top25_y$coefficients[1]*US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$rank_density_CBSA)

#adjusting y upward by average fixed effect in sample
y_top25_resid <- y_top25_resid + (mean(as.vector(constrained_reg_rent_top25_y$fixed_effects)) - mean(y_top25_resid))*rep(1, length(y_top25_resid))

#For bottom
constrained_reg_rent_bot25_y <- lm_robust(log_income_to_value ~ rank_density_CBSA + Average_HH_size + Share_household_type2, 
                                          data = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], fixed_effects = CBSA)

y_bot25_resid <- as.vector(US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$log_income_to_value -
                             constrained_reg_rent_bot25_y$fitted.values + constrained_reg_rent_bot25_y$coefficients[1]*US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$rank_density_CBSA)

#adjusting y upward by average fixed effect in sample
y_bot25_resid <- y_bot25_resid + (mean(as.vector(constrained_reg_rent_bot25_y$fixed_effects)) - mean(y_bot25_resid))*rep(1, length(y_bot25_resid))

#Time to plot both!

#We Got em!!!!
ggplot() +
  geom_smooth(method = 'lm', aes(x = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$rank_density_CBSA, y = y_top25_resid, color = 'Top 25%')) +
  geom_smooth(method = 'lm', aes(x = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$rank_density_CBSA, y = y_bot25_resid, color = 'Bottom 25%')) +
  scale_colour_manual(name = "legend", values = c("blue", "red")) + 
  labs(x = "Ranked CBSA density", y = "log(Income / HouseValue)")
ggsave("value_stringency.png", width = 16, height = 10, units = "cm")

#_____________________________________________________________________________________________________________________________

#_______________________________________________________________________________________________________________________
#Fact 4: Superstars have flatter housing supply elasticity gradients. Low density housing has lower supply elasticities.
#_______________________________________________________________________________________________________________________
#Housing supply elasticities?
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_BSH_Elasticity, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_BSH_Elasticity, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("supplyelasticity.png", width = 16, height = 10, units = "cm")

#non-demeaned. missing middle have the smallest supply elasticities!
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=BSH_2020_Elasticity, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=BSH_2020_Elasticity, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

#What is the relationship between demeaned income and supply elasticities? 
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_Income_CBSA, y=BSH_2020_Elasticity, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_Income_CBSA, y=BSH_2020_Elasticity, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
#very interesting... Higher income tracts have lower supply elasticities

#__________________________________________________________________________________________________
#PART 3###_________________________________________________________________________________________
#Q: Do facts replicate with distance to CBD instead of housing density rank? 
#__________________________________________________________________________________________________

#Housing density
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Housing_density), color = 'red') +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Housing_density), color = 'blue')

#Prices
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_adjusted_housevalue), color = 'red') +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_adjusted_housevalue), color = 'blue') 


#income does not follow pattern

#(not clear it follows!)
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Income), color = 'blue')

#White (follows roughly same pattern)
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_White_share), color = 'blue')

##################PART 4: ROBUSTNESS#####################################################
#1) Sorting replicates after removing california? Yes!! California does not drive the results 

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_JOINED_INC$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_JOINED_INC$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_P$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Top 25%'))  +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_P$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Bottom 25%')) + 
  scale_colour_manual(name="legend", values = c("blue", "red"))

#What about density and BSH elasticities? Yes

ggplot() + 
  geom_smooth(method = 'loess', span= 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_JOINED$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_JOINED$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_JOINED_PRICE$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_housevalue,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_JOINED_PRICE$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_housevalue, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

#Do housing density units replicate across census and ACS from NHGIS and Planning database? Yes

ggplot() +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_NHGIS_density_CBSA, y=demeaned_log_NHGIS_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_NHGIS_density_CBSA, y=demeaned_log_NHGIS_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red")) #SAME RESULTS.


