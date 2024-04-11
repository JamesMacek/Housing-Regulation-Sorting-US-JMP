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

*Calculating average lot size and mode of lot size within block groups. 
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



*_________________________________________________________________
*Importing transactions file to construct land value densities
*_________________________________________________________________
joinby clip using "output/transactions_master.dta", unmatched(both)

*remove observations without geocodes, etc. 
count if _merge == 2 
*11 million potential transactions unmatched-- this makes sense, we consider only regulated structures within MSAs
drop if _merge == 2
drop _merge
*Take latest transaction of property (so we are not double-counting properties)


*Any duplicate transactions? 
duplicates list ownertransfercompositetransactio if missing(ownertransfercompositetransactio) == 0
*Zero duplicates

*duplicate clips ? take most recent transaction so we aren't double-counting properties
bysort clip: egen max_timeSold = max(100*yearSold + monthSold)
keep if 100*yearSold + monthSold == max_timeSold | missing(max_timeSold)
duplicates drop clip, force
*dropping remaining clips, only 175,000 of them that contain properties sold in the same year and month

			*Adjusting each homes sales value to inflation. Expressing in 2020 dollars. (Using official US CPI)
			replace saleamount = 1.0784*saleamount if yearSold == 2016
			replace saleamount = 1.0559*saleamount if yearSold == 2017
			replace saleamount = 1.03*saleamount if yearSold == 2018
			replace saleamount = 1.023*saleamount if yearSold == 2019
			replace saleamount = (1-0.0449)*saleamount if yearSold == 2021
			replace saleamount = (1-0.1307)*saleamount if yearSold == 2022
			
			label var saleamount "Sales amount, 2020 dollars using CPI"
			
			*Subsample of all transactions pre 2020 (covid). Will be useful for robustness.
			gen PreCovidIndicator = 1 if yearSold <= 2019
			
			
			*Generating land value densities by quantile in each location p25, p50, p75 for inspection
			
			*Individual property value densities
			gen LandValueDensity_ind = saleamount/acres
			gen LandValueDensity_ind_preCovid = saleamount/acres if PreCovidIndicator == 1
			
			*Block group distribution of lot sizes to calculate conditional land value densities
			foreach q in  25 50 75 {
					bysort State County Tract BlockGroup: egen acres_dist_p`q' = pctile(acres), p(`q')
			}
			
			*Indicators for which bin in the density distribution of lot sizes
			gen acres_dist_p25_ind = (acres < acres_dist_p25)
			gen acres_dist_p50_ind = (acres >= acres_dist_p25 & acres < acres_dist_p50)
			gen acres_dist_p75_ind = (acres >= acres_dist_p50 & acres < acres_dist_p75)
			gen acres_dist_p100_ind = (acres >= acres_dist_p75)
			
			*Land value densities conditional on each bin
			foreach q in  25 50 75 100 {
					bysort State County Tract BlockGroup: egen LandValueDensity_p`q' = median(LandValueDensity_ind) if acres_dist_p`q'_ind == 1
			}
			
			*For robustness, median land value density for pre covid only
			bysort State County Tract BlockGroup: egen LandValueDensity_med = median(LandValueDensity_ind)
			bysort State County Tract BlockGroup: egen LandValueDensity_preCovid = median(LandValueDensity_ind_preCovid)
			
			*Variables we dont need (keep memory clean)
			drop saleamount max_timeSold yearSold monthSold daySold *code ownertransfer* ownershiptr* fipscode*
			
*Doing the same for historical transactions values to construct panel of stringency over time
joinby clip using "output/transactions_master_historical.dta", unmatched(both)
count if _merge == 2
drop if _merge == 2
drop _merge

bysort clip: egen max_timeSold = max(100*yearSold + monthSold)
keep if 100*yearSold + monthSold == max_timeSold | missing(max_timeSold)
duplicates drop clip, force

			*Adjusting each historical homes sales value to inflation. Expressing in 2010 dollars. (Using official US CPI)
			replace saleamount = 1.03*saleamount if yearSold == 2008
			replace saleamount = 1.03*saleamount if yearSold == 2009
			replace saleamount = 0.98*saleamount if yearSold == 2011
			replace saleamount = 0.96*saleamount if yearSold == 2012
			
			*Doing the same for historical transactions
			gen LandValueDensity_ind_hist = saleamount/acres
			
			*Generating median land value density 
			bysort State County Tract BlockGroup: egen LandValueDensity_med_hist = median(LandValueDensity_ind_hist)
			
			drop saleamount max_timeSold yearSold monthSold daySold *code ownertransfer* ownershiptr* fipscode* 
	
			*Saving temporary file for use to follow
			*Dropping additional unneeded variables to save space
			
			save "temp/temp_lotModes.dta", replace 
			
			
*_______IMPORTING ACS VARIABLES (I.e. Regulated Structure Fractions, etc)_____________
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
			
			*Merging with temp_assignRegulation.
			cd "DataV2/CoreLogic"
			merge 1:m State County Tract BlockGroup using "temp/temp_lotModes.dta"
			drop if _merge == 1
			drop _merge
			*Note: some unmatched block groups that are outside MSAs, etc. 
			
			*cleaning up some more variables we dont need on save 
			drop *indicator LandValueDensity_ind* acres_dist_*_ind
			
	*Importing municipality defns
	merge m:1 State County Tract BlockGroup using "output/blkgrpstats_forClustering.dta", keepusing(Assigned*)
	drop _merge
	*Saving temporary file again-- this will be loaded at every iteration.
	*Do this 1 by 1 to save RAM.
	save "temp/temp_lotModes.dta", replace 
	
	clear
	*_____________________START LOOP OVER DIFFERENT DEFINITIONS OF CLUSTERING ALGORITHMS______
	*Loop over various cluster definitions.
	*Change to full grid outputting from clusterZoningDistricts.R if you want to test all clustering definitions-- this is the optimal from 
local alpha_grid "0.95"
local alpha_grid_size : word count `alpha_grid'
local min_Clust_grid "250"
local min_Clust_grid_size : word count `min_Clust_grid'
local cluster_type_grid "CORELOGIC"
local cluster_type_grid_size : word count `cluster_type_grid'

forval alpha_l = 1/`alpha_grid_size' {
	forval min_Clust_l = 1/`min_Clust_grid_size' {  
		forval cluster_type_l = 1/`cluster_type_grid_size' {
		
			local alpha_choice: word `alpha_l' of `alpha_grid'
			local min_Clust: word `min_Clust_l' of `min_Clust_grid'
			local cluster_type: word `cluster_type_l' of `cluster_type_grid'
			
			*Importing
			use "temp/temp_lotModes.dta", replace 
			
			*Merging in definition of Zoning districts based on algorithm
			merge m:1 State County Tract BlockGroup using "output/ConstructedZoningDistricts_alpha_`alpha_choice'_minClust`min_Clust'_`cluster_type'.dta", keepusing(ZoningDistrictID)
			drop _merge
			
			*capturing definition of lot sizes 
			if "`cluster_type'" == "CORELOGIC" {
				local municipality_dfn = "Assigned_municipality"
			}
			if "`cluster_type'" == "GEOCODED" {
			local municipality_dfn = "Assigned_geocoded_municipality"
			}
			
					
			local structureList "singlefamily duplex triplex quadriplex"
			local structureListSize : word count `structureList'
			
			*SOLVING FOR MODES...
			forval i = 1/`structureListSize' {
	
			*Creating definition of mode conditional on clustering algorithm
			*Note: zoningDistrictID only unique within municipality definition and assigned zoning code to block group
	
				local curStructure : word `i' of `structureList'
				cap bysort CBSA `municipality_dfn' Assigned_zoningcode ZoningDistrictID: egen `curStructure'_mode = mode(acres) if `curStructure'ind == 1, minmode
				*Note: local i is the implied number of housing units per lot. Correcting mode for number of allowed housing units per lot.
				replace `curStructure'_mode = `curStructure'_mode/`i' 
				*Replacing some block groups not assigned to a zoning district but have some properties in sample.
				replace `curStructure'_mode = `curStructure'_blkgrp_m if missing(ZoningDistrictID)
			}
			
			
			*Collapsing to block group level
			collapse (mean) *_mode *eLotSize LandValueDensity* acres_dist_p* regulated_housingUnit_share, by(State County Tract BlockGroup CBSA Assigned* ZoningDistrictID)
				
			*Taking the minumum of all singlefamily-quadplexes in the data
			egen UnitDensityRestriction = rowmin(*_mode)
		
			*Finding median lot size for structure types associated with unit density restriction (this will be used for other programs/robustness/ to check observations where the algorithm fails)
			gen MedLotSize_minimum_struct = .
			*Dummy telling us which UDR is associated with 
			gen structure_of_UDR = .
			
			forval i = 1/`structureListSize' {
				local curStructure : word `i' of `structureList'
				
				replace structure_of_UDR = `i' if abs(`curStructure'_mode - UnitDensityRestriction) <= 0.001 & missing(`curStructure'_mode) == 0
				replace MedLotSize_minimum_struct = `curStructure'_med_eLotSize/`i' if structure_of_UDR == `i'
				*need rounding because equality check will not work otherwise.
				*Note: this is now in terms of unit density restrictions. 
				
			}
			
			*Matching land value density conditional on quantile to the quantile associated with the unit density restriction
			*This is because land value density depends heavily on lot size, and we want to correct for this. Otherwise, many outliers in the model
			gen LandValueDensity_matched = .
			replace LandValueDensity_matched = LandValueDensity_p25 if UnitDensityRestriction*structure_of_UDR < acres_dist_p25 
			replace LandValueDensity_matched = LandValueDensity_p50 if acres_dist_p25 <= UnitDensityRestriction*structure_of_UDR & UnitDensityRestriction*structure_of_UDR < acres_dist_p50 
			replace LandValueDensity_matched = LandValueDensity_p75 if acres_dist_p50 <= UnitDensityRestriction*structure_of_UDR & UnitDensityRestriction*structure_of_UDR < acres_dist_p75 
			replace LandValueDensity_matched = LandValueDensity_p100 if UnitDensityRestriction*structure_of_UDR >= acres_dist_p75 
			
			*Windsorizing all land value density at CBSA level, controlling for transaction level outliers that cause problems in the model
			cap ssc install winsor2
			winsor2 LandValueDensity*, replace cuts(0.02 99.98) by(CBSA)
			
			*Genning our measure of stringency for empirical work
			gen IncomeStringency = LandValueDensity_matched*UnitDensityRestriction*regulated_housingUnit_share if missing(LandValueDensity_matched) == 0
			*Note: locations with missing land value density mean that they don't have very many regulated structures. This means Income Stringency == 0.
			replace IncomeStringency = 0 if regulated_housingUnit_share == 0

			*Doing the same procedure for pre-covid measures
			gen IncomeStringency_preCovid = LandValueDensity_preCovid*UnitDensityRestriction*regulated_housingUnit_share if missing(LandValueDensity_preCovid) == 0
			replace IncomeStringency_preCovid = 0 if regulated_housingUnit_share == 0
			
	
			*Sorting and saving data frame
			sort State County Tract BlockGroup CBSA
	
			save "output/Regulation_measurements_alpha_`alpha_choice'_minClust`min_Clust'_`cluster_type'.dta", replace

			clear
		}
	}
}
*End loop over all clustering grid sizes 
	