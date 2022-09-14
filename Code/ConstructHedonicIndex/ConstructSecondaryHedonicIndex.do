cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*Construct the hedonic index for block groups which cannot be constructed from Zillow data.
*requires output from ConstructNHGIS and from the ConstructZoningDistricts to get census blocks in sample (pre 2012 changes to 2010 census tracts)

use "Data/ZillowData/Output/ConstructedZoningDistricts.dta", replace

foreach var of varlist State County Tract BlockGroup {
	destring `var', replace
}

*merging with NHGIS data, note some unmatched in master because of very minor tract changes from 2010-2012
merge 1:1 State County Tract BlockGroup using "Data/US_Data/NHGIS/BlockGroup/nhgis_blkgrp.dta"
drop if _merge != 3
drop _merge

*Refer to codebook for variable names

*Creating controls to construct index

*1: structure types distribution
forval i = 3/12 {
	if `i' < 10 {
		cap gen StructureType_`i' = qyze00`i'/qyze002
	}
	
	if `i' >= 10 {
		cap gen StructureType_`i' = qyze0`i'/qyze002
	}
	
}

*Replacing if no data on tenured households with renter

forval i = 3/12 {
	
local shift_i = `i' + 11
	
replace StructureType_`i' = qyze0`shift_i'/qyze013 if missing(StructureType_`i')
	
}


*2: Number of roooms
forval i = 3/11 {
	
	if `i' < 10 {
		cap gen RoomShare_`i' = qyue00`i'/qyue002
	}
	
	if `i' >= 10 {
		cap gen RoomShare_`i' = qyue0`i'/qyue002
	}
}

forval i = 3/11 {
	
local shift_i = `i' + 10
	
replace RoomShare_`i' = qyue0`shift_i'/qyue012 if missing(RoomShare_`i')
	
}

*Year structure built
forval i = 3/11 {
	
		
	if `i' < 10 {
		cap gen YearBuilt_`i' = qy3e00`i'/qy3e002
	}
	
	if `i' >= 10 {
		cap gen YearBuilt_`i' = qy3e0`i'/qy3e002
	}
}

forval i = 3/11 {
	
local shift_i = `i' + 10
	
replace YearBuilt_`i' = qy3e0`shift_i'/qy3e012 if missing(YearBuilt_`i')
	
}

*Bedrooms
forval i = 3/8 {
	
		
	if `i' < 10 {
		cap gen Bedroom_`i' = qy9e00`i'/qy9e002
	}
	
	if `i' >= 10 {
		cap gen Bedroom_`i' = qy9e0`i'/qy9e002
	}
}

forval i = 3/8 {
	
local shift_i = `i' + 7
	
replace Bedroom_`i' = qy9e0`shift_i'/qy9e009 if missing(Bedroom_`i')
	
}

*Heating (note, only for occupied units) 
forval i = 2/10 {
	if `i' < 10 {
		cap gen Heating_`i' = qy7e00`i'/qy7e001
	}
	
	if `i' >= 10 {
		cap gen Heating_`i' = qy7e0`i'/qy7e001
	}
}

*Plumbing
gen Plumbing = qzfe003/qzfe002
replace Plumbing = qzfe006/qzfe005 if missing(Plumbing)

*Kitchen
gen Kitchen = qzie003/qzie002
replace Kitchen = qzie006/qzie005 if missing(Kitchen)

*Running regression
destring CBSA, replace 
xtset CBSA
gen log_med_value = log(qz6e001)

xtreg log_med_value StructureType_* RoomShare_* YearBuilt_* Bedroom_* Plumbing Kitchen, fe robust
predict Secondary_hedonicPrice, ue

replace Secondary_hedonicPrice = exp(Secondary_hedonicPrice)

*Creating average home value statistic for the secondary index
*Taking aggregate home value and dividing by number of occupied housing units in the ACS
gen Secondary_avgHomePrice = qz8e001/qx7e002
gen Secondary_medHomePrice = qz6e001

*keeping relevant variables
keep CBSA CBSA_NAME State County Tract BlockGroup Secondary_*

save "Data/US_Data/Output/SecondaryHedonicIndex.dta", replace
	