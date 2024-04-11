*This file constructs minimum lot sizes (unit density restrictions) given some definition of clusters. 
*Also provides a measure of the income stringency of regulation that is used in the empirical work.
*Requires output from cluster_ZoningDistricts.R 


*Date created: December 8 2022. This file takes many hours to run. 
*Updated October 20th, 2022 to be much faster.

*Importing our definitions 
*Setting filepaths in global
cd $proj_filepath
do "CodeV2/global_filepath.do"

cd "DataV2/CoreLogic"

*Importing specific columns from master current assessment file
use clip landusecode propertyindicatorcode numberofunits acres landsquarefootage yearbuilt effectiveyearbuilt State County Tract BlockGroup CBSA using "output/currentAssess_master.dta", clear

*We repeat some functions in collapse_stats_forClustering.do 
*In particular, labeling single family homes, 
replace acres = landsquarefootage/43560 if acres == . & landsquarefootage != .


*Taking properties that are located in regulated structures.
*Here, we define regulated structures to mean single family homes, duplexes, triplexes and fourplexes. We ignore condominiums because data to infer number of housing units on the property is very sparse. 

*Single family homes correspond to propertyindicatorcode == 10 or landusecode == 163.
*Duplexes correspond to propertyindicatorcode == 21 or landusecode == 115
*Triplexes are landusecode == 165 and quadraplexes are landusecode == 151

*Remove properties below ~~100 square feet from sample
drop if acres < 0.0023
*note: these parcels are probably not used for housing, or have some weird ownership structure.


*___________________________________________________________________________________________________________________________
*OCT 13 2023: 
*INCLUDE propertyindicatorcode == 10 as single family housing-- as many properties are dropped and this actually induces differences across locations that imply very different outcomes for the clustering algorithm. Dropping property indicator code classifications changes the outcome of the clustering algorithm by assigning much smaller, unrealistic clusters.

tab landusecode if propertyindicatorcode == 10
*propertyindicatorcode == 21 is a multifamily property
tab landusecode if propertyindicatorcode == 21
*most of the action includes NEC properties (landusecode == 100)
*___________________________________________________________________________________________________________________________

*We assign the "keep" indicator to regulated structures that were used in the construction of regulated housing. 
gen keepind=0
gen nonresidentialind = 0
gen singlefamilyind = 0
gen duplexind = 0
gen triplexind = 0
gen quadriplexind = 0
gen vacantind = 0

*Keeping if single family homes identified
replace keepind = 1 if propertyindicatorcode == 10 | landusecode == 163 
replace singlefamilyind = 1 if propertyindicatorcode == 10 | landusecode == 163 | numberofunits == 1

*Duplexes 
replace keepind = 1 if propertyindicatorcode == 21 | landusecode == 115 
replace duplexind = 1 if landusecode == 115 | numberofunits == 2

*Triplexes and quadriplexes (none directly identified in the data)
replace keepind = 1 if landusecode == 165 | landusecode == 151
replace triplexind = 1 if landusecode == 165 | numberofunits == 3
replace quadriplexind = 1 if landusecode == 151 | numberofunits == 4

*If property type identified off number of units
replace keepind = 1 if numberofunits <= 4 & missing(numberofunits) == 0

*Additional cleaning.
*Drop if mobile homes from the dataset (so they don't influence the analysis)
drop if 135 <= landusecode & landusecode <= 137

*NOTE: VACANT ind EMPTY IN DATA.
replace vacantind = 1 if propertyindicatorcode == 80

capture assert vacantind == 0
di _rc
if _rc == 0 {
	drop vacantind
}

*Dropping nonkeepindicators
drop if keepind != 1


*_______________________________SAVING AS TEMPORARY FILE WITH LOT SIZES AT PROPERTY LEVEL_______________________-
save "temp/temp_properties.dta", replace 



*_________PART 2____________________ CALCULATING BLOCK GROUP LEVEL MEDIAN/MODE LOT SIZES, etc. For use with imputation/robustness
local structureList "singlefamily duplex triplex quadriplex"
local structureListSize : word count `structureList'

forval i = 1/`structureListSize' {
	
	local curStructure: word `i' of `structureList'
	*This will be the average land associated with a housing unit, conditional on different types.
	cap bysort State County Tract BlockGroup: egen `curStructure'_med_eLotSize_tmp = median(acres) if `curStructure'ind == 1	
	
	*Oct 13th 2023: don't adjust lot size by unit density restriction -- most of time purchases are for entire lot with duplexes, etc. Very rarely do they work like condominiums where each unit has ownership. 
	*Making sure these modes are nonmissing for ALL observations, filling them in by structure type for properties without that structure 
	cap bysort State County Tract BlockGroup: egen `curStructure'_med_eLotSize = median(`curStructure'_med_eLotSize_tmp)
	drop `curStructure'_med_eLotSize_tmp
	

	*Calculating mode by block group (this will be to fill in all districts that are not assigned a zoning ID)
	cap bysort State County Tract BlockGroup: egen `curStructure'_blkgrp_m_tmp = mode(acres) if `curStructure'ind == 1, minmode 
	replace `curStructure'_blkgrp_m_tmp = `curStructure'_blkgrp_m_tmp/`i'
	
	cap bysort State County Tract BlockGroup: egen `curStructure'_blkgrp_m = mean(`curStructure'_blkgrp_m_tmp)
	drop `curStructure'_blkgrp_m_tmp
}

*Collapsing to block group level
collapse *_blkgrp_* *_med_eLotSize, by(State County Tract BlockGroup CBSA)

save "temp/temp_blockgroup_lotstats.dta", replace 


*_________________________________________________________________________________________________________________________________
*_____________PART 3: TRANSACTION FILES, ESTIMATE LAND VALUE DENSITIES USING REGRESSION APPROACH__________________________________
*_________________________________________________________________________________________________________________________________
*Re-importing property level data
use "temp/temp_properties.dta", clear
*Generating median lot sizes at block group level to estimate land value densities
bysort State County Tract BlockGroup: egen median_lotsize = median(acres)

*joining to transactions 
joinby clip using "output/transactions_master.dta"
*28 million transactions


*Any duplicate transactions? 
duplicates list ownertransfercompositetransactio if missing(ownertransfercompositetransactio) == 0
*Zero duplicates

*Subsample of all transactions pre 2020 (covid). Will be useful for robustness.
			gen PreCovidIndicator = (yearSold <= 2019)
			
			*duplicate clips ? take most recent transaction so we aren't double-counting properties
			bysort clip PreCovidIndicator: egen max_timeSold = max(100*yearSold + monthSold)
			keep if 100*yearSold + monthSold == max_timeSold | missing(max_timeSold)
			duplicates drop clip PreCovidIndicator, force
			*dropping remaining clips, only 175,000 of them that contain properties sold in the same year and month
	
			*Adjusting each homes sales value to inflation. Expressing in 2020 dollars. (Using official US CPI)
			replace saleamount = 1.0784*saleamount if yearSold == 2016
			replace saleamount = 1.0559*saleamount if yearSold == 2017
			replace saleamount = 1.03*saleamount if yearSold == 2018
			replace saleamount = 1.023*saleamount if yearSold == 2019
			replace saleamount = (1-0.0449)*saleamount if yearSold == 2021
			replace saleamount = (1-0.1307)*saleamount if yearSold == 2022
			
			label var saleamount "Sales amount, 2020 dollars using CPI"
	

*Estimating land value density gradient, which is a regression of lot size on transaction value with block group FE's
*Estimation to happen separately by CBSA
*creating block group id
egen BlockGroupID = group(State County Tract BlockGroup)
xtset BlockGroupID
sort State County Tract BlockGroup

*Generating median lot sizes and sale amounts at block group level to estimate land value densities
bysort State County Tract BlockGroup: egen median_saleamount = median(saleamount)
*Doing the same for pre-covid measures
bysort State County Tract BlockGroup: egen median_saleamount_precov = median(saleamount) if PreCovidIndicator == 1


*_______________________________________________________________________________
*Intercepts and slopes to predict land values for a given lot size______________
*_______________________________________________________________________________

*slope of land value to lot size estimated with regression with block group FE's
gen saleamount_slope = .

*Start regressions at CBSA level to get saleamount slops
levelsof CBSA, local(levels)

foreach i of local levels {
	*Estimating FE regression
	qui xtreg saleamount acres if CBSA == `i', fe robust
	*storing slope
	replace saleamount_slope = _b[acres] if CBSA == `i'	
}
*_______________________________________________________________________________
*Collapsing data down to block group level
keep State County Tract BlockGroup CBSA saleamount_* median_*
collapse saleamount_slope median_*, by(State County Tract BlockGroup CBSA)

*Slope for predicting value of minimal lots
sum saleamount_slope, detail

*Saving temporary file
save "temp/temp_est_landval_current.dta", replace


*Repeating above procedure for historical transactions
use "temp/temp_properties.dta", clear
bysort State County Tract BlockGroup: egen median_lotsize_tmp = median(acres)

joinby clip using "output/transactions_master_historical.dta"

bysort clip: egen max_timeSold = max(100*yearSold + monthSold)
keep if 100*yearSold + monthSold == max_timeSold | missing(max_timeSold)
duplicates drop clip, force

			*Adjusting each historical homes sales value to inflation. Expressing in 2010 dollars. (Using official US CPI) To do: express in 2012 dollars (though wont matter much)
			replace saleamount = 1.03*saleamount if yearSold == 2008
			replace saleamount = 1.03*saleamount if yearSold == 2009
			replace saleamount = 0.98*saleamount if yearSold == 2011
			replace saleamount = 0.96*saleamount if yearSold == 2012

*Historical median saleamounts
bysort State County Tract BlockGroup: egen median_saleamount_hist = median(saleamount)

*Collapsing
collapse median_saleamount_hist median_lotsize_tmp, by(State County Tract BlockGroup CBSA)

*Merging in slope estimates from above
merge 1:1 State County Tract BlockGroup CBSA using "temp/temp_est_landval_current.dta"
*completing coverage of block groups with no current coverage
replace median_lotsize = median_lotsize_tmp if missing(median_lotsize)
drop median_lotsize_tmp

*Imputing observations that are not in contemporary transactions sample with CBSA slopes
bysort CBSA: replace saleamount_slope = saleamount_slope[1] if CBSA == CBSA[1]

drop _merge

*Calculating saleamount_intercept_historical, assuming slope coefficients constant over time
save "temp/temp_est_landval_full.dta", replace 

*______________________________________________________________________________
****PART 3: Constructing ACS VARIABLES*******************
*______________________________________________________________________________
clear
		cd $proj_filepath

		import delimited using "DataV2/US_Data/NHGIS/nhgis_20205_blck_grp.csv", clear

		*Keeping certain variables to match on block groups
		keep amwbe* amwe* amu5e* statea countya tracta blkgrpa

		*Generating shares of households in specific structure types.
		*Shares of households in singlefamily - quadriplex, both single family and owner occupied
		gen regulated_housingUnit_share = (amu5e003 + amu5e004 + amu5e005 + amu5e006 + amu5e014 +  amu5e015 + amu5e016 + amu5e017)/(amu5e001)
			
		*generating self reported ACS mean and median housing values for additional spatial coverage
		rename amwbe001 median_ACS_houseval
		gen average_ACS_houseval = amwee001/amu5e001
			
		*Keeping variables for merge
		rename statea State
		rename countya County
		rename tracta Tract
		rename blkgrpa BlockGroup
		rename amu5e001 SampledHousingUnitsACS

		keep State County Tract BlockGroup average* median* *_share SampledHousingUnitsACS

cd "DataV2/CoreLogic"
save "temp/temp_ACS.dta", replace
*Saving...
clear


*___________________________________________________________________________
*________________________________THE FINAL PART_____________________________

*_____________________START LOOP OVER DIFFERENT DEFINITIONS OF CLUSTERING ALGORITHMS______
	*Loop over various cluster definitions.
	*Change to full grid outputting from clusterZoningDistricts.R if you want to test all clustering definitions-- this is the optimal from 
local alpha_grid "0.5 0.75 0.95"
local alpha_grid_size : word count `alpha_grid'
local min_Clust_grid "5 15 25 100 250"
local min_Clust_grid_size : word count `min_Clust_grid'
local cluster_type_grid "CORELOGIC GEOCODED"
local cluster_type_grid_size : word count `cluster_type_grid'

forval alpha_l = 1/`alpha_grid_size' {
	forval min_Clust_l = 1/`min_Clust_grid_size' {  
		forval cluster_type_l = 1/`cluster_type_grid_size' {
		
			local alpha_choice: word `alpha_l' of `alpha_grid'
			local min_Clust: word `min_Clust_l' of `min_Clust_grid'
			local cluster_type: word `cluster_type_l' of `cluster_type_grid'
			
			*Importing
			use "temp/temp_properties.dta", replace
			
			*Merging in definition of Zoning districts based on algorithm
			merge m:1 State County Tract BlockGroup CBSA using "output/ConstructedZoningDistricts_alpha_`alpha_choice'_minClust`min_Clust'_`cluster_type'.dta", keepusing(CBSA_NAME ZoningDistrictID Assigned_municipality Assigned_geocoded_municipality Assigned_zoningcode)
			drop _merge
			*note: unmatched using comes from dropping small lots at beggining of this do file
			
			*capturing definition of lot sizes 
			if "`cluster_type'" == "CORELOGIC" {
				local municipality_dfn = "Assigned_municipality"
			}
			if "`cluster_type'" == "GEOCODED" {
			local municipality_dfn = "Assigned_geocoded_municipality"
			}
			
			*merging in own blockgroup modes by structure
			merge m:1 State County Tract BlockGroup using "temp/temp_blockgroup_lotstats.dta", keepusing(*blkgrp_m)
					
			local structureList "singlefamily duplex triplex quadriplex"
			local structureListSize : word count `structureList'
			
			*SOLVING FOR MODES...
			forval i = 1/`structureListSize' {
	
			*Creating definition of mode conditional on clustering algorithm
			*Note: zoningDistrictID only unique within municipality definition and assigned zoning code to block group
	
				local curStructure : word `i' of `structureList'
				bysort CBSA `municipality_dfn' Assigned_zoningcode ZoningDistrictID: egen `curStructure'_mode = mode(acres) if `curStructure'ind == 1, minmode
				*Note: local i is the implied number of housing units per lot. Correcting mode for number of allowed housing units per lot.
				replace `curStructure'_mode = `curStructure'_mode/`i' 
				*Replacing some block groups not assigned to a zoning district but have some properties in sample.
				replace `curStructure'_mode = `curStructure'_blkgrp_m if missing(ZoningDistrictID)
			}
			
			
			*Collapsing to block group level
			collapse (mean) *_mode, by(State County Tract BlockGroup CBSA CBSA_NAME Assigned* ZoningDistrictID)
				
			*Taking the minumum of all singlefamily-quadplexes in the data
			egen UnitDensityRestriction = rowmin(*_mode)
			
		
			
			*MERGING IN TEMP DATASETS
			*Additional median lot size stuff
			merge 1:1 State County Tract BlockGroup using "temp/temp_blockgroup_lotstats.dta", keepusing(*_eLotSize)
			drop _merge
			*For land value estimation
			merge 1:1 State County Tract BlockGroup using "temp/temp_est_landval_full.dta"
			drop _merge
			*ACS variables
			merge 1:1 State County Tract BlockGroup using "temp/temp_ACS.dta"
			drop if _merge == 2
			drop _merge
			
			*Finding median lot size for structure types associated with unit density restriction (this will be used for other programs/robustness/ to check observations where the algorithm fails)
			gen MedLotSize_minimum_struct = .
			*Dummy telling us which UDR is associated with 
			gen structure_of_UDR = .
			
			local structureList "singlefamily duplex triplex quadriplex"
			local structureListSize : word count `structureList'
			
			forval i = 1/`structureListSize' {
				local curStructure : word `i' of `structureList'
				
				replace structure_of_UDR = `i' if abs(`curStructure'_mode - UnitDensityRestriction) <= 0.001 & missing(`curStructure'_mode) == 0
				replace MedLotSize_minimum_struct = `curStructure'_med_eLotSize/`i' if structure_of_UDR == `i'
				*need rounding because equality check will not work otherwise.
				*Note: this is now in terms of unit density restrictions. 
				
			}
			
			
			
			*________STRINGENCY MEASURES_______________________________________________________
			*__________________________________________________________________________________
			
			*CONTROLLING OUTLIERS ON SALE AMOUNTS USING THE FOLLOWING PROCEDURE________________
		
			*Loop over each sale measure
			foreach var of varlist median_saleamount median_saleamount_precov median_saleamount_hist {
				
				*CBSA median sale amount across block groups
				cap bysort CBSA: egen `var'_CBSA = median(`var')
			
				*Flag for median sale amounts > 1million
				gen saleamount_1m_flag = (`var' > 1000000 & !missing(`var'))
		
				*Flagging CBSAs that have a median saleamount < 300,000 USD
				gen cheap_market_flag = (`var'_CBSA <= 300000)
			
				*replacing sales values with CBSA median sales values if both flags are true
				*this helps control massive measured prices in some block groups where very few transactions exist
				*and the transactions that are there are for outlier luxury properties. 
			
				*789 block groups that fall under both of these flags, replacing with median saleamounts
				replace `var' = `var'_CBSA if saleamount_1m_flag == 1 & cheap_market_flag == 1
				cap gen `var'_impind = (saleamount_1m_flag == 1 & cheap_market_flag == 1)
				drop cheap_market_flag saleamount_1m_flag
			}
			
			*CONSTRUCTING_______________________________________________________________________
			*Intercepts in prediction model to predict value of minimal lot
			gen saleamount_intercept = median_saleamount - saleamount_slope*median_lotsize
			gen saleamount_intercept_precov = median_saleamount_precov - saleamount_slope*median_lotsize
			gen saleamount_intercept_hist = median_saleamount_hist - saleamount_slope*median_lotsize
			
			*Generating measures of land value density
			*1. Contemporary measure
			gen LandValueDensity_matched = (saleamount_intercept + saleamount_slope*(structure_of_UDR)*UnitDensityRestriction)/(UnitDensityRestriction*structure_of_UDR)
			*2. PreCovid measure
			gen LandValueDensity_preCovid = (saleamount_intercept_precov + saleamount_slope*UnitDensityRestriction*structure_of_UDR)/(UnitDensityRestriction*structure_of_UDR)
			*Historical measure
			gen LandValueDensity_hist = (saleamount_intercept_hist + saleamount_slope*UnitDensityRestriction*structure_of_UDR)/(UnitDensityRestriction*structure_of_UDR)
			*Note, we adjust land value density downward by implied unit density restriction
			
			
			*Censoring negative predictions for land values at zero (only 97 observations)
			foreach var of varlist LandValueDensity* {
				replace `var' = 0 if `var' < 0
			}
			
			*Summarized land value density
			sum LandValueDensity_matched, detail
			
			*Tab of multifamily structures assigned to different block groups
			tab structure_of_UDR
			
			
			*Genning our measure of stringency for empirical work
			gen IncomeStringency = LandValueDensity_matched*UnitDensityRestriction*regulated_housingUnit_share if missing(LandValueDensity_matched) == 0
			*Note: locations with missing land value density mean that they don't have very many regulated structures. This means Income Stringency == 0.
			replace IncomeStringency = 0 if regulated_housingUnit_share == 0

			*Doing the same procedure for pre-covid measures
			gen IncomeStringency_preCovid = LandValueDensity_preCovid*UnitDensityRestriction*regulated_housingUnit_share if missing(LandValueDensity_preCovid) == 0
			replace IncomeStringency_preCovid = 0 if regulated_housingUnit_share == 0
			
	
			*Sorting and saving data frame
			sort State County Tract BlockGroup CBSA
			
			
			*Removing sample observations where we observe no contemporary transactions
			drop if missing(median_saleamount_CBSA)
			*Farmington, NM and Gadsden, AL
	
			save "output/Regulation_measurements_alpha_`alpha_choice'_minClust`min_Clust'_`cluster_type'.dta", replace

			clear
		}
	}
}
*End loop over all clustering grid sizes 
	





