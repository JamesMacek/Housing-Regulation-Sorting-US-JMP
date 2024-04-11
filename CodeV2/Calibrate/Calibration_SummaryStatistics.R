#Calibration summary statistics for calibrated moments in the paper.
#Calibrated (or estimated) housing prices, consumption values, amenity values by income

#Libraries
library(haven)
library(dplyr)
library(vtable) #for easy conditional tables

#Importing quantiles that define superstar cities definition
load(file = "DataV2/US_Data/Output/CBSA_quantiles_dens.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")


#Baseline specification for output
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE)
if (BASELINE_SPECIFICATION$bySkill_to_pass == TRUE) {
  error("Stop: does not work for bySkill == TRUE yet")
}
#(Does not work for bySkill == TRUE at current codebase)


#Importing calibrated data...
calibrated_dta <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill",
                                  BASELINE_SPECIFICATION$bySkill_to_pass, 
                                  "_pref_", BASELINE_SPECIFICATION$pref,
                                  "_amenities.dta"))
#Creating SuperStar city dummy for output tables
calibrated_dta["SuperStar"] <- rep("No", nrow(calibrated_dta))
calibrated_dta$SuperStar[calibrated_dta$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) &
                           calibrated_dta$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"])] <- "Yes" #superstar dummy

for (incomeType in 1:7) {
  calibrated_dta[paste0("log_Amenity_", incomeType)] <- log(calibrated_dta[[paste0("Amenity_", incomeType)]])
  calibrated_dta[is.infinite(calibrated_dta[[paste0("log_Amenity_", incomeType)]]), ][[paste0("log_Amenity_", incomeType)]] <- NA #not reporting zeroes
}

#Grouping data by city, selecting key variables we want in table
calibrated_dta <- calibrated_dta %>% group_by(CBSA_NAME) %>% select(SuperStar, starts_with("price"), starts_with("log_Amenity"),
                                                                    starts_with("consumption")) 
#Renaming columns for output to table (in order they appear)
changeColnames <- c("MSA", "SuperStar city",
                    "Housing price (Regulated zone)",
                    "Housing price (Unregulated zone)", 
                    "ln Amenity value (0-25k)",
                    "ln Amenity value (25-50k)",
                    "ln Amenity value (50-75k)",
                    "ln Amenity value (75-100k)",
                    "ln Amenity value (100-150k)",
                    "ln Amenity value (150-200k)",
                    "ln Amenity value (200k+)", 
                    "Consumption value (0-25k)",
                    "Consumption value (25-50k)",
                    "Consumption value (50-75k)",
                    "Consumption value (75-100k)",
                    "Consumption value (100-150k)",
                    "Consumption value (150-200k)",
                    "Consumption value (200k+)")

colnames(calibrated_dta) <- changeColnames



#_______________________________________________________________________________
#Creating tables.
#_______________________________________________________________________________

#Aggregate table:
  vtable::st(calibrated_dta[,!names(calibrated_dta) == "SuperStar city"],
             out = "latex",
             file = "DataV2/Counterfactuals/Calibration_Output/Calibration_summarystats_aggregate.tex",
             summ = c('notNA(x)','mean(x)','sd(x)', 'median(x)'),
             summ.names = c("N", "Mean", "Sd", "Median"),
             digits = 2)

  

#Table by superstar city
  vtable::st(calibrated_dta, out = "latex",
             file = "DataV2/Counterfactuals/Calibration_Output/Calibration_summarystats_SuperStar.tex",
             summ = c('notNA(x)','mean(x)','sd(x)', 'median(x)'),
             summ.names = c("N", "Mean", "Sd", "Median"),
             group = "SuperStar city",
             digits = 2) #specify wide format for this group


#Maybe a map (for later?)