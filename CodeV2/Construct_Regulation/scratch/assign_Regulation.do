*This file constructs minimum lot sizes (unit density restrictions) given some definition of clusters. 
*Also provides a measure of the income stringency of regulation that is used in the empirical work.
*Requires output from cluster_ZoningDistricts.R 

*Date created: December 8 2022. This file takes many hours to run. 

*Importing our definitions 

*Setting filepaths
cd $proj_filepath

cd "DataV2/CoreLogic"

*Importing specific columns from master current assessment file
use clip landusecode propertyindicatorcode numberofunits acres landsquarefootage State County Tract BlockGroup CBSA using "output/currentAssess_master.dta", clear

*We repeat some functions in collapse_stats_forClustering.do 
*In particular, labeling single family homes, 
replace acres = landsquarefootage/43560 if acres == . & landsquarefootage != .


*Taking properties that are located in regulated structures.
*Here, we define regulated structures to mean single family homes, duplexes, triplexes and fourplexes. We ignore condominiums because data to infer number of housing units on the property is very sparse. 

*Single family homes correspond to propertyindicatorcode == 10 or landusecode == 163.
*Duplexes correspond to propertyindicatorcode == 21 or landusecode == 115
*Triplexes are landusecode == 165 and quadraplexes are landusecode == 151

*____________________________________________________________
*OCT 13 2023: DO NOT ASSIGN USING PROPERTYINDICATORCODE!
*____________________________________________________________

*We assign the "keep" indicator to regulated structures that were used in the construction of regulated housing. 
gen keepindicator=0
gen nonresidentialindicator=0
gen singlefamilyindicator = 0
gen duplexindicator = 0
gen triplexindicator = 0
gen quadriplexindicator = 0
gen vacantindicator = 0

*Keeping if single family homes identified
replace keepindicator = 1 if propertyindicatorcode == 10 | landusecode == 163
replace singlefamilyindicator = 1 if propertyindicatorcode == 10 | landusecode == 163 | numberofunits == 1

*Duplexes 
replace keepindicator = 1 if propertyindicatorcode == 21 | landusecode == 115 
replace duplexindicator = 1 if landusecode == 115 | numberofunits == 2

*NOTE: TRIPLEXES AND FOURPLEXES GET LUMPED INTO PROPERTYINDICATORCODE 21.
*We need to correct for this. 

*Triplexes and quadriplexes (none directly identified in the data)
replace keepindicator = 1 if landusecode == 165 | landusecode == 151
replace triplexindicator = 1 if landusecode == 165 | numberofunits == 3
replace quadriplexindicator = 1 if landusecode == 151 | numberofunits == 4

*If property type identified off number of units
replace keepindicator = 1 if numberofunits <= 4 & missing(numberofunits) == 0

*Additional cleaning.
*Drop if mobile homes from the dataset (so they don't influence the analysis)
drop if 135 <= landusecode & landusecode <= 137

*Creating vacant land and commercial indicators
replace nonresidentialindicator = 1 if propertyindicatorcode >= 24 & propertyindicatorcode <= 80

*Drop if non residential
drop if nonresidentialindicator == 1

*NOTE: VACANT INDICATOR EMPTY IN DATA.
replace vacantindicator = 1 if propertyindicatorcode == 80

capture assert vacantindicator == 0
di _rc
if _rc == 0 {
	drop vacantindicator
}

*Calculating average lot size and mode of lot size within block groups-- will be used for later for locations not matched to zones
local structureList "singlefamily duplex triplex quadriplex"
local structureListSize : word count `structureList'

forval i = 1/`structureListSize' {
	
	local curStructure: word `i' of `structureList'
	*This will be the average land associated with a housing unit, conditional on different types.
	cap bysort State County Tract BlockGroup: egen `curStructure'_avg_eLotSize = mean(acres) if `curStructure'indicator == 1
	*replace `curStructure'_avg_eLotSize = `curStructure'_avg_eLotSize/`i' *Oct 13th 2023: don't adjust lot size -- most of time purchases are for entire lot with duplexes, etc
	
	*Calculating mode by block group (this will be to fill in all districts that are not assigned a zoning ID)
	cap bysort State County Tract BlockGroup: egen `curStructure'_blkgrp_m = mode(acres) if `curStructure'indicator == 1, minmode 
	replace `curStructure'_blkgrp_m = `curStructure'_blkgrp_m/`i'
}

*Saving temporary file to use for other programs
save "temp/temp_lotModes.dta", replace 


*



*START LOOP OVER CLUSTERING DEFINITIONS HERE.______________________
*Matching on State County Tract BlockGroup with choice of clustering dataset.
*Check DataV2/CoreLogic/output for list of availible clustering parameters from cluster_ZoningDistricts.R

*Loop over various cluster definitions.
local alpha_grid "0.25 0.5 0.75"
local alpha_grid_size : word count `alpha_grid'
local min_Clust_grid "5 10 15 20 25"
local min_Clust_grid_size : word count `min_Clust_grid'
local cluster_type_grid "CORELOGIC GEOCODED"
local cluster_type_grid_size : word count `cluster_type_grid'


forval alpha_l = 1/`alpha_grid_size' {
	forval min_Clust_l = 1/`min_Clust_grid_size' {  
		forval cluster_type_l = 1/`cluster_type_grid_size' {
			
			use "temp/temp_lotModes.dta", clear
			
			local alpha_choice: word `alpha_l' of `alpha_grid'
			local min_Clust: word `min_Clust_l' of `min_Clust_grid'
			local cluster_type: word `cluster_type_l' of `cluster_type_grid'
		
		
			merge m:1 State County Tract BlockGroup CBSA using "output/ConstructedZoningDistricts_alpha_`alpha_choice'_minClust`min_Clust'_`cluster_type'.dta"
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

			forval i = 1/`structureListSize' {
	
			*Creating definition conditional on clustering algorithm
	
				local curStructure : word `i' of `structureList'
				cap bysort `municipality_dfn' Assigned_zoningcode ZoningDistrictID: egen `curStructure'_mode = mode(acres) if `curStructure'indicator == 1, minmode
				*Note: local i is the implied number of housing units per lot. Correcting mode for number of allowed housing units per lot.
				replace `curStructure'_mode = `curStructure'_mode/`i' 
				*Replacing some block groups not assigned to a zoning district but have some properties in sample.
				replace `curStructure'_mode = `curStructure'_blkgrp_m if missing(ZoningDistrictID)
			}

*Generating a measure for average effective lot size of regulated structures.
*This gives us a rough measure of the value density for regulated structures, though the average value of structure is assumed to be over all residential units in the sample.


*Part 2:____________merging with transactions data______________

			*Merging with transactions, keeping only matched assessments to transactions.
			merge m:m clip using "output/transactions_master.dta", keep(using match)
			*NOTE: m:m merge comes from two observations having identical clips in the data (out of 84 million). 
			*Must be a read-delimited error. 
			*About 30 million observations are matched correctly. So it can't be an error. Why the identical clip?

			*7 million transactions not matched from using data. Makes sense as we don't keep geocoded transactions.
			drop if _merge ==2
			drop _merge

			*Adjusting each homes sales value to inflation. Expressing in 2020 dollars. (Using official US CPI)
			replace saleamount = 1.0784*saleamount if yearSold == 2016
			replace saleamount = 1.0559*saleamount if yearSold == 2017
			replace saleamount = 1.03*saleamount if yearSold == 2018
			replace saleamount = 1.023*saleamount if yearSold == 2019
			replace saleamount = (1-0.0449)*saleamount if yearSold == 2021
			replace saleamount = (1-0.1307)*saleamount if yearSold == 2022

			label var saleamount "Sales amount, 2020 dollars using CPI"

			*Dropping all observations that have duplicates under the same sale amount and transaction
			*duplicates list ownertransfercompositetransactio saleamount
			*No observations are duplicates, suggesting transaction amounts are at the individual parcel level. Treat each as a separate transaction.

			*Subsample of all transactions pre 2020 (covid). Will be useful for robustness.
			gen PreCovidIndicator = 1 if yearSold <= 2019

			*generating medians
			bysort State County Tract BlockGroup: egen CoreLogic_med_houseprice = median(saleamount)
			bysort State County Tract BlockGroup: egen CoreLogic_med_houseprice_PreCov = median(saleamount) if PreCovidIndicator == 1

			*Collapsing by State County Tract BlockGroup
			collapse (mean) *_mode *_avg_eLotSize CoreLogic*, by(State County Tract BlockGroup CBSA CBSA_NAME Assigned* ZoningDistrictID)

			*Saving temporary file
			save "temp/temp_assignRegulation.dta", replace

			*Part 3:___________merging with ACS data on number of housing units in each structure type__________
			clear
			cd $proj_filepath

			import delimited using "DataV2/US_Data/NHGIS/nhgis_20205_blck_grp.csv", clear

			*Keeping certain variables to match on block groups
			keep amwbe* amwe* amu5e* statea countya tracta blck_grpa

			*Generating shares of households in specific structure types.
			*Shares of households in singlefamily - quadriplex, both single family and owner occupied
			gen regulated_housingUnit_share = (amu5e003 + amu5e004 + amu5e005 + amu5e006 + amu5e014 +  amu5e015 + amu5e016 + amu5e017)/(amu5e001)
	
			gen singlefamily_housingUnit_share = (amu5e003 + amu5e004 + amu5e014 +  amu5e015)/(amu5e001)

			gen duplex_housingUnit_share = (amu5e005 + amu5e016)/amu5e001

			gen tri_quadplex_housingUnit_share = (amu5e006 + amu5e017)/amu5e001

			*generating self reported mean and median housing values for additional spatial coverage
			rename amwbe001 median_ACS_houseval
			gen average_ACS_houseval = amwee001/amu5e001

			rename statea State
			rename countya County
			rename tracta Tract
			rename blck_grpa BlockGroup
			rename amu5e001 SampledHousingUnitsACS

			keep State County Tract BlockGroup average* median* *_share SampledHousingUnitsACS

			*Merging with temp_assignRegulation.
			cd "DataV2/CoreLogic"
			merge 1:m State County Tract BlockGroup using "temp/temp_assignRegulation.dta"

			*All matched from using. Not master (we didn't subset block groups in sample geography)
			drop if _merge == 1
			drop _merge

			*Taking housing unit weighted average of effective land per (regulated) housing unit to arrive at a measure of housing value density
			gen singlefamily_housingUnit_w  = singlefamily_housingUnit_share*singlefamily_avg_eLotSize/regulated_housingUnit_share
			gen duplex_housingUnit_w  = duplex_housingUnit_share*duplex_avg_eLotSize/regulated_housingUnit_share
			gen triplex_housingUnit_w  = tri_quadplex_housingUnit_share*triplex_avg_eLotSize/regulated_housingUnit_share
			gen quadriplex_housingUnit_w  = tri_quadplex_housingUnit_share*quadriplex_avg_eLotSize/regulated_housingUnit_share
			*Note: effective lot size is corrected for housing units from above

			*Getting EffectiveLandPerHousingUnit in regulated structures
			egen EffectiveLandPerHousingUnit = rowtotal(*_housingUnit_w)

			*Getting Value Per Unit of Land
			gen LandValueDensity = CoreLogic_med_houseprice/EffectiveLandPerHousingUnit
			gen LandValueDensity_preCovid = CoreLogic_med_houseprice_PreCov/EffectiveLandPerHousingUnit

			*Assigning final physical density restriction, multiplying that by the value density of housing

			*Taking the minumum of all singlefamily-quadplexes in the data
			egen UnitDensityRestriction = rowmin(*_mode)
			
			*Finding average lot size associated with unit density restriction (this will be used for other programs)
			gen AverageLotSize_minimum_struct = .
			
			local structureList "singlefamily duplex triplex quadriplex"
			local structureListSize : word count `structureList'
			forval i = 1/`structureListSize' {
				local curStructure : word `i' of `structureList'
				replace AverageLotSize_minimum_struct = `curStructure'_avg_eLotSize/`i' if round(`curStructure'_mode, .01) == round(UnitDensityRestriction, 0.01) & missing(`curStructure'_mode) == 0
				*need rounding because equality check will not work otherwise.
				*Note: this is now in terms of unit density restrictions. 
				
				
			}
			
			*Note: observations with missing land value density tend to come from locations with very little regulated housing from the ACS. Safely assign unit density restrictions to 0. Note: landvalue density is missing if there are 0 regulated structures in the ACS. 
			
			*Flagging observations with average effective lot size greater than average amount of land per housing unit. This is not used anywhere, just for physical inspection. 
			gen Flag_mode = 0
			replace Flag_mode = 1 if AverageLotSize_minimum_struct < UnitDensityRestriction

			*Genning our measure of stringency for empirical work
			gen IncomeStringency= LandValueDensity*UnitDensityRestriction*regulated_housingUnit_share if missing(LandValueDensity) == 0

			*Note: locations with missing land value density mean that they don't have very many regulated structures. This means Income Stringency == 0.
			replace IncomeStringency = 0 if regulated_housingUnit_share == 0

			*Doing the same procedure for pre-covid measures
			gen IncomeStringency_preCovid = LandValueDensity_preCovid*UnitDensityRestriction*regulated_housingUnit_share if missing(LandValueDensity_preCovid) == 0
			replace IncomeStringency_preCovid = 0 if regulated_housingUnit_share == 0
			
			*Sorting
			sort State County Tract BlockGroup CBSA

			*Saving
			save "output/Regulation_measurements_alpha_`alpha_choice'_minClust`min_Clust'_`cluster_type'.dta", replace

			clear
		}
	}
}