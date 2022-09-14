cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*Estimates a simple IV model-- requires output from CalibrateAmenityValues.R
*Date edited: July 12th 2022

**_____________PART 1: CLEANING__________________________
use "Data/US_Data/LinLee/Supplementary/data/Lee_Lin_data.dta", clear
drop if year != 2010
drop year
rename geo2010 tract_fips

*NanDa landcover
merge 1:m tract_fips using "Data/US_Data/NANDA landcover/nanda_landcover_tract_2001-2016_02P.dta", keep(matched)
drop if year_intp != 2011
drop _merge
*keeping only 2010 measurements

rename statefips State
rename countyfips County
gen Tract = substr(tract_fips, 6, 6)
destring Tract, replace
drop tract_fips

*This is very rough-- just want a temporary estimate of Omega
merge 1:m State County Tract using "Data/Counterfactuals/CalibratedAmenityValues.dta", keep(matched)
drop _merge

*Average income last year, from Lin&Lee, note this only varies at the tract level (because our sattelite measures and other amenities measures only vary at the tract level.)
gen log_Average_income = log(tr_hh_avginc) 


*Importing NLCD Sattelite data to control for other natural amenities

*Use 2013 CBSA definitions, 
encode CBSA_NAME, generate(eCBSA_NAME) 


foreach var of varlist d2river d2lake d2shore annpre {
	*Inverse distance 
	gen inv`var' = 1/`var'
	
}



*Constructing view based  variables
foreach var of varlist invd2river invd2lake invd2shore nm_* invannpre maxjul minjan prop_value_3* prop_value_4* prop_value_5* prop_value_7* prop_value_9* {
	*supermodularities with average slopes (i.e. slopes make natural views easier to see)
	gen `var'_view = avgslope*`var' 
	
}

*Variable labels for output
la var log_Average_income "Log(Average Income)"
la var avgslope "Average Slope"

*generating log variables
foreach var of varlist withincity_amenity_* {
	gen l`var' = log(`var')
}

*Taking average amenity value across income groups
egen BlockAmenity = rowmean(lwithincity_amenity_*)


*aggregating commuting costs to tracts, etc
bysort State County Tract: egen log_Amenity = mean(BlockAmenity) 
bysort State County Tract: egen commute_minsTract = mean(avg_commuteMins)

*replacing log_Amenity with commuting adjusted values
*Semi-elasticity from Redding/Sturm commuting paper
local commuting_semielasticity 0.01

replace log_Amenity = log_Amenity + `commuting_semielasticity'*commute_minsTract

*TractAmenity only varies at the tract level

*Dropping block groups
duplicates drop State County Tract, force


*no controls
ivreghdfe log_Amenity (log_Average_income = avgslope), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) sfirst
ivreghdfe log_Amenity log_Average_income, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) sfirst


*storing local controls
local selected_ctrl "invd2* maxjul minjan* invannpre* invannpre_view validflood nm_* prop_value_3* prop_value_4* prop_value_5* prop_value_7* prop_value_9*"

ivreghdfe log_Amenity `selected_ctrl' (log_Average_income = avgslope), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')  sfirst
*Preferred specification = this one. 

*what does OLS say?
local selected_ctrl "invd2* maxjul minjan* invannpre* invannpre_view validflood nm_* prop_value_3* prop_value_4* prop_value_5* prop_value_7* prop_value_9*"
ivreghdfe log_Amenity log_Average_income `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
 