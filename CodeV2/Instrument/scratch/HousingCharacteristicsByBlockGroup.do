*This file constructs housing + other characteristics by block group for use in donut IV estimation.
*Requires output from merge_stringency.do, as well as the master assessments file.

clear
cd $proj_filepath


*Importing vanilla geography
use "DataV2/US_Data/Output/SampleGeography.dta", clear

*Merging with master assessments
merge 1:m State County Tract BlockGroup using "DataV2/CoreLogic/output/currentAssess_master.dta"


*Dropping nonresidential indicators
*only 4000 observations
drop if propertyindicatorcode >= 24 & propertyindicatorcode <= 80

*Keep desired variables for the donut instrument-- density, etc
keep State County Tract BlockGroup CBSA yearbuilt effectiveyearbuilt livingsquarefeetallbuildings buildingsquarefeet fireplaceindicator poolindicator bedroomsallbuildings totalroomsallbuildings totalbathroomsallbuildings fullbathsallbuildings

local toCollapse yearbuilt effectiveyearbuilt livingsquarefeetallbuildings buildingsquarefeet fireplaceindicator poolindicator bedroomsallbuildings totalroomsallbuildings totalbathroomsallbuildings fullbathsallbuildings

collapse (mean) `toCollapse', by(State County Tract BlockGroup CBSA)


*Merging on constructed_BlockGroup_V2.dta
merge 1:1 State County Tract BlockGroup using "DataV2/US_Data/Output/Constructed_Block_V2.dta"

drop _merge

*Keeping variables for donut instrument
keep State County Tract BlockGroup CBSA `toCollapse' demeaned_car_share demeaned_public_transport_share demeaned_bus_share demeaned_avg_travel_time demeaned_Housing_densitydemeaned_median_bage

*Saving
save "DataV2/US_Data/Instrument/Donut_variables_nonTopographic.dta", replace
clear
