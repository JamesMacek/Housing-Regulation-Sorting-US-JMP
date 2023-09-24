*This file constructs the measured amount of land in regulated vs unregulated structures using CoreLogic up to date assessments.
*Requires output from Construct_CoreLogic module. 

cd $proj_filepath

*Importing vanilla geography
use "DataV2/US_Data/Output/SampleGeography.dta", clear

merge 1:1 State County Tract BlockGroup using "DataV2/US_Data/Output/raw_Regulation_ByBlockGroup.dta"
drop _merge

merge 1:m State County Tract BlockGroup using "DataV2/CoreLogic/output/currentAssess_master.dta"

keep clip State County Tract BlockGroup acres landsquarefootage parcellevellatitude parcellevellongitude numberofunits landusecode propertyindicatorcode UnitDensityRestriction

*temporary keep indicator to extract as many housing units as possible
gen regulated_Structure = 0
gen singlefamilyindicator = 0
gen duplexindicator = 0
gen triplexindicator = 0
gen quadriplexindicator = 0

*Keeping if single family homes identified
replace regulated_Structure = 1 if propertyindicatorcode == 10 | landusecode == 163 
replace singlefamilyindicator = 1 if propertyindicatorcode == 10 | landusecode == 163 | numberofunits == 1

*Duplexes 
replace regulated_Structure = 1 if propertyindicatorcode == 21 | landusecode == 115 
replace duplexindicator = 1 if landusecode == 115 | numberofunits == 2

*NOTE: TRIPLEXES AND FOURPLEXES GET LUMPED INTO PROPERTYINDICATORCODE 21.
*We need to correct for this by using landusecode. 

*Triplexes and quadriplexes (none directly identified in the data)
replace regulated_Structure = 1 if landusecode == 165 | landusecode == 151
replace triplexindicator = 1 if landusecode == 165 | numberofunits == 3
replace quadriplexindicator = 1 if landusecode == 151 | numberofunits == 4

*If property type identified off number of units
replace regulated_Structure = 1 if numberofunits <= 4 & missing(numberofunits) == 0 

*Replacing regulated structure with missing if nonresidential property or miscellaneous (will be deleted later)
drop if (propertyindicatorcode >= 23 & propertyindicatorcode <= 80) | propertyindicatorcode == 0



*Generating effective land per housing unit. This will be used to construct an average amount of land per housing units
*in each tract
local structureList "singlefamily duplex triplex quadriplex"
local structureListSize : word count `structureList'

replace acres = landsquarefootage/43560 if acres == . & landsquarefootage != .

forval i = 1/`structureListSize' {
	local curStructure: word `i' of `structureList'
	replace acres = acres/`i' if `curStructure'indicator == 1 
}

*Finally, set "regulated" structures with land below the minimum lot size to code 2. These are lumped in with unregulated structures.
replace regulated_Structure = 2 if acres < UnitDensityRestriction & regulated_Structure == 1


*Identifying land use in multifamily structures.
*Another way--identify assessments in the same building (checking to see if building has same land in acres)
egen buildingID = group(State County Tract BlockGroup parcellevellatitude parcellevellongitude acres) if regulated_Structure == 0

*gen observation count per building 
bysort buildingID: egen propertyCountBuilding = count(acres) if missing(buildingID) == 0

*temporary sum of all 
gen tmp_land = acres/propertyCountBuilding if missing(buildingID) == 0

*Both ways to measure land in unregulated structures
bysort State County Tract BlockGroup: egen sum_land_acres_unreg= sum(tmp_land)

*sum and mean land in regulated structures
bysort State County Tract BlockGroup: egen sum_land_reg_acres = total(acres) if regulated_Structure == 1
bysort State County Tract BlockGroup: egen sum_land_reg_acres_belowLot = total(acres) if regulated_Structure == 2

*Collapsing by acres (mean and sum)
collapse (mean) sum_land_reg_acres sum_land_reg_acres_belowLot sum_land_acres_unreg, by(State County Tract BlockGroup)


sort State County Tract BlockGroup


save "DataV2/US_Data/Output/Regulated_land_acres_V2.dta", replace

clear

*We now have a measure of regulated land. 