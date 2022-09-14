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

#Facts on lot size stringency. 
#Requires output from block_construct.R and MergeDataForCounterfactuals.do

load(file = "Data/US_Data/Output/Constructed_2010_Tract.Rdata")
load(file = "Data/US_Data/Output/CBSA_med_house_price.Rdata")
load(file = "Data/US_Data/Output/CBSA_quantiles.Rdata")

lot_size_stringency <- read_dta("Data/Counterfactuals/JoinedDataForCounterfactuals.dta")
lot_size_stringency <- lot_size_stringency %>% select(State, County, Tract, BlockGroup, DensityRestriction, HousingValueDensity, 
                                                      IncomeStringencyofRegulation, hedonicPrice)

#Joining the data 
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, lot_size_stringency, by = c("State", "County", "Tract", "BlockGroup"))
rm(lot_size_stringency)

#Demeaning income stringency measures by MSA
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED %>% group_by(CBSA) %>% 
                        mutate(demeaned_stringency = IncomeStringencyofRegulation/mean(IncomeStringencyofRegulation, na.rm = TRUE))


#Splitting data by superstar/non-superstars
#Taking random sample of top25 due to memory issues
set.seed = 1234 #To reproduce random sample
top25 <- US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]
top25_sample <- top25[sample(nrow(top25), floor(nrow(top25)/3)),] #take approx 33% of the sample, 30k observations

bot25 <- US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]

#GGplot

#For this plot, I use a span of 1 to help smooth the average. Data are a bit noisier.
#omit standard errors due to memory issues.
ggplot() + 
  geom_smooth(method = 'loess' , data = top25,
              aes(x=rank_density_CBSA, y=demeaned_stringency, colour = 'Top 25%'), se = FALSE,
              span = 1) + 
  geom_smooth(method = 'loess', data = bot25,
              aes(x=rank_density_CBSA, y=demeaned_stringency, colour = 'Bottom 25%'), se = FALSE,
              span = 1) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block level)") +
  ylab("Income Stringency of Density Restrictions (MSA Average = 1)")


#Checking standard errors on subset of the data, due to memory issues
ggplot() + 
  geom_smooth(method = 'loess' , data = top25_sample,
              aes(x=rank_density_CBSA, y=demeaned_stringency, colour = 'Top 25%'), se = TRUE,
              span = 1) + 
  geom_smooth(method = 'loess', data = bot25,
              aes(x=rank_density_CBSA, y=demeaned_stringency, colour = 'Bottom 25%'), se = TRUE,
              span = 1) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block level)") +
  ylab("Income Stringency of Density Restrictions (MSA Average = 1)")
ggsave("Data/US_Data/Output/stringencyofDensityRestrictions.png", width = 20, height = 12, units = "cm") 

#Looking at regressions when intercept allowed to vary by sample
top25 <- top25 %>% group_by() %>% mutate(avg_stringency = demeaned_stringency*mean(IncomeStringencyofRegulation, na.rm = TRUE))
bot25 <- bot25 %>% group_by() %>% mutate(avg_stringency = demeaned_stringency*mean(IncomeStringencyofRegulation, na.rm = TRUE))

#
ggplot() + 
  geom_smooth(method = 'loess' , data = top25,
              aes(x=rank_density_CBSA, y=avg_stringency, colour = 'Top 25%'), se = FALSE,
              span = 1) + 
  geom_smooth(method = 'loess', data = bot25,
              aes(x=rank_density_CBSA, y=avg_stringency, colour = 'Bottom 25%'), se = FALSE,
              span = 1) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block level)") +
  ylab("Income Stringency of Density Restrictions (intercept varies by sample)")
ggsave("Data/US_Data/Output/stringencyofDensityRestrictionsInterceptVary.png", width = 20, height = 12, units = "cm")

#Regressions conditional on positive assignment of density restrictions-- both curves are (obviously?) upward sloping. #Interesting single crossing property. 
ggplot() + 
  geom_smooth(method = 'loess' , data = top25[top25$DensityRestriction > 0,],
              aes(x=rank_density_CBSA, y=demeaned_stringency, colour = 'Top 25%'), se = FALSE,
              span = 1) + 
  geom_smooth(method = 'loess', data = bot25[bot25$DensityRestriction > 0,],
              aes(x=rank_density_CBSA, y=demeaned_stringency, colour = 'Bottom 25%'), se = FALSE,
              span = 1) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block level)") +
  ylab("Income Stringency of Density Restrictions (MSA Average == 1, conditional on positive density)")


#Regressing log Income per capita on density restrictions within cities.
#What about across?

#predicting within-city variation in incomes (this prediction does well for most of the data)
summary(lm(demeaned_log_Income ~ demeaned_stringency, data = US_BLOCK_2010_JOINED))

#But has a hump shape for very stringent neighborhoods (that's interesting, but very weird-- note this is driven by bot25 sample)
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_BLOCK_2010_JOINED,
              aes(x=demeaned_stringency, y=demeaned_log_Income), se = FALSE)
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
              aes(x=demeaned_stringency, y=demeaned_log_Income), se = FALSE) 




US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED %>% group_by(CBSA) %>% 
  mutate(demeaned_hPrice = log(hedonicPrice) - mean(log(hedonicPrice), na.rm = TRUE))

summary(lm(demeaned_log_Income ~ demeaned_hPrice, data = US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$DensityRestriction > 0,]))


#Distributions of lot size regulation 
ggplot() + 
  geom_histogram(data = US_BLOCK_2010_JOINED, aes(x = DensityRestriction), bins = 1000, color = "blue") +
  xlab("Effective Minimum Lot Size l(i), Acres")
ggsave("Data/US_Data/Output/DistDensRegulation.png", width = 20, height = 12, units = "cm")

ggplot() + 
  stat_ecdf(data = US_BLOCK_2010_JOINED, aes(x = DensityRestriction), geom = "step", color = "red") +
  xlab("Effective Minimum Lot Size l(i), Acres") +
  ylab("Empirical CDF")
ggsave("Data/US_Data/Output/ECDFDensRegulation.png", width = 20, height = 12, units = "cm")


