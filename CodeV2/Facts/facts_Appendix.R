#Summary statistics for key empirical measures of stringency.

#Libraries
library(haven)
library(dplyr)
library(vtable) #for easy conditional tables
library(ggplot2)
library(collapse)
library(forcats)

#Importing quantiles that define superstar cities definition
#Quantiles of city distributions on various statistics for robustness
for (qtile in c("", "_dens", "_pop", "_wage")) {
  load(file = paste0("DataV2/US_Data/Output/CBSA_quantiles", qtile, ".Rdata"))
}

#Load facts dataset
load(file = "DataV2/US_Data/Output/Constructed_Block_V2.Rdata")


#Creating superstar city dummy
US_BLOCK["SuperStar"] <- rep("No", nrow(US_BLOCK))
US_BLOCK$SuperStar[US_BLOCK$PooledWage > as.numeric(quantile_CBSA_wage["75.0%"])] <- "Yes" #superstar dummy
US_BLOCK["log_Average_income"] <- log(US_BLOCK$Average_income)

US_BLOCK <- US_BLOCK %>% group_by(CBSA_NAME) %>% select(SuperStar, log_Average_income, UnitDensityRestriction_cl, IncomeStringency_cl,
                                                        LandValueDensity_matched, regulated_housingUnit_share, CBSA_med_house_value,
                                                        City_housing_density, City_housing_pop, PooledWage) 
#Changing units on certain variables
for (reunit in c("IncomeStringency_cl", "LandValueDensity_matched")) {
  US_BLOCK[[reunit]] <- US_BLOCK[[reunit]]/1000000
  
}

US_BLOCK_forBoxplot <- select(US_BLOCK, CBSA_NAME, SuperStar,
                              IncomeStringency_cl, CBSA_med_house_value, City_housing_density, PooledWage, City_housing_pop) 
US_BLOCK <- US_BLOCK %>% select(-CBSA_med_house_value, -City_housing_density, -PooledWage, -City_housing_pop)

#Changing column names
changeColnames <- c("MSA", "Superstar?",
                    "ln Average Income", 
                    "Unit Density Restriction (acres)",
                    "Stringency measure (2020 millions USD)",
                    "Land Value Density (2020 millions USD/acre)",
                    "Regulated housing units (share)")
colnames(US_BLOCK) <- changeColnames

#Creating aggregate value to put into column
US_BLOCK <- rbind(US_BLOCK, 
                        mutate(select(US_BLOCK, -contains("SuperStar")), `SuperStar city` = "Aggregate") )


#_______________________________________________________________________________
#Creating tables.
#_______________________________________________________________________________

#Disaggregated by sample
vtable::st(US_BLOCK,
           out = "latex",
           file = "DataV2/US_Data/Output/Facts_summarystats_SuperStar.tex",
           summ = c('notNA(x)','mean(x)','sd(x)', 'median(x)'),
           summ.names = c("N", "Mean", "Sd", "Median"),
           group = "Superstar?",
           digits = 3,
           fit.page = "0.9\\textwidth")




#_________________________________________________________
#_Collapsing by city and plotting wages against stringency 
#_________________________________________________________
US_BLOCK_forCollap <- collap(US_BLOCK_forBoxplot, SuperStar + PooledWage + 
                             IncomeStringency_cl + City_housing_pop ~ CBSA_NAME)

US_BLOCK_forCollap <- data.frame(US_BLOCK_forCollap)
ggplot() +    
  geom_point(data = US_BLOCK_forCollap, aes(y = IncomeStringency_cl, x = PooledWage, color = PooledWage, size = City_housing_pop/1000000),alpha = 0.5) +
  #geom_smooth(method = "lm", y = IncomeStringency_cl, x = PooledWage, data = US_BLOCK_forCollap) +
  geom_text(data = US_BLOCK_forCollap[US_BLOCK_forCollap$PooledWage > 1.15,], check_overlap = T, size = 4.5, nudge_y = 0.05,
            aes(x = PooledWage, y = IncomeStringency_cl, label = CBSA_NAME)) + 
  scale_color_gradient(low = "blue", high = "red", name = "Productivity") +
  xlab("Productivity (residualized city wages)") + 
  ylab("Stringency of Regulation (2020 Millions USD)") +
  coord_cartesian(clip = "off") + 
  labs(size = "Households (millions)")  + theme_gray(base_size = 15) + theme(legend.position = "bottom", 
                                                                              plot.title = element_text(hjust = 0.5)) #Setting ranges

ggsave(paste0("DataV2/US_Data/Output/ProductivityStringency.png"), width = 30, height = 20, units = "cm") 




#_________________________________________________
#Create ggplot bar plot for stringency measure____
#_________________________________________________
#Boxplot table by select cities at different points in the price/density distribution

#Select 8 cities in superstar sample:
#New York-Newark-Jersey City, NY-NJ-PAN
#Washington-Arlington-Alexandria, DC-VA-MD-WV
#Boston-Cambridge-Newton, MA-NH
#Minneapolis-St. Paul-Bloomington, MN-WI
#Philadelphia-Camden-Wilmington, PA-NJ-DE-MD
#San Jose-Sunnyvale-Santa Clara, CA
#Seattle-Tacoma-Bellevue, WA
#Denver-Aurora-Lakewood, CO


#Select 7 cities from non-superstar sample
#Phoenix-Mesa-Scottsdale, AZ
#Jacksonville, FL
#Tampa-St. Petersburg-Clearwater, FL
#Rochester, NY
#San Antonio-New Braunfels, TX
#St. Louis, MO-IL
#Tampa-St. Petersburg-Clearwater, FL
#Oklahoma City, OK

#Select these CBSAs
US_BLOCK_forBoxplot <- US_BLOCK_forBoxplot %>% filter(CBSA_NAME == 'New York-Newark-Jersey City, NY-NJ-PA' | 
                                                      CBSA_NAME == 'Washington-Arlington-Alexandria, DC-VA-MD-WV'| 
                                                      CBSA_NAME == 'Boston-Cambridge-Newton, MA-NH'| 
                                                      CBSA_NAME == 'Minneapolis-St. Paul-Bloomington, MN-WI'| 
                                                      CBSA_NAME == 'Philadelphia-Camden-Wilmington, PA-NJ-DE-MD'| 
                                                      CBSA_NAME == 'San Francisco-Oakland-Hayward, CA' | 
                                                      CBSA_NAME == 'San Jose-Sunnyvale-Santa Clara, CA' | 
                                                      CBSA_NAME == 'Seattle-Tacoma-Bellevue, WA'| 
                                                      CBSA_NAME == 'Denver-Aurora-Lakewood, CO'| 
                                                      CBSA_NAME == 'Phoenix-Mesa-Scottsdale, AZ'| 
                                                      CBSA_NAME == 'Jacksonville, FL'| 
                                                      CBSA_NAME == 'Tampa-St. Petersburg-Clearwater, FL'| 
                                                      CBSA_NAME == 'Rochester, NY'| 
                                                      CBSA_NAME == 'San Antonio-New Braunfels, TX'| 
                                                      CBSA_NAME == 'St. Louis, MO-IL'| 
                                                      CBSA_NAME == 'Tampa-St. Petersburg-Clearwater, FL'| 
                                                      CBSA_NAME == 'Oklahoma City, OK')

#Creating boxplot
ggplot(data = US_BLOCK_forBoxplot,
       aes(y = fct_reorder(CBSA_NAME, IncomeStringency_cl, .fun = median), x = IncomeStringency_cl,)) +
       geom_boxplot(aes(fill = SuperStar), alpha = 0.2,
                    outlier.shape = NA) + #set to 3 million limit    
       scale_x_continuous(limits=c(0,2.2)) + 
       xlab("Stringency measure (2020 Millions USD)") +
       ylab("MSA") 
ggsave("DataV2/US_Data/Output/Facts_incomeStringency_boxplot.png",  
       width = 18, height = 15, units = "cm") 
   

#CREATING TABLES: