*This file constructs the measured amount of vacant land. 
*Requires output from Construct_CoreLogic module. 

clear
cd $proj_filepath

*Importing vanilla geography
use "DataV2/US_Data/Output/SampleGeography.dta", clear

merge 1:m State County Tract BlockGroup using "DataV2/CoreLogic/output/vacant_master.dta"

*36,000 block groups do not have vacant land. This makes sense!
count if _merge == 1

bysort State County Tract BlockGroup: egen sum_vacant_land_acres = total(acres)

*Collapsing by acres (mean and sum)
collapse (mean) sum_vacant_land_acres, by(State County Tract BlockGroup)

sort State County Tract BlockGroup

save "DataV2/US_Data/Output/Vacant_land_acres.dta", replace