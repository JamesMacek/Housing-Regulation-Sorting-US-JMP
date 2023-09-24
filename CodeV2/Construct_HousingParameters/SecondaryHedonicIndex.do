*This file constructs ACS hedonic indices used in the counterfactual exercises. This is to supplement CoreLogic hedonic indices and increase spatial coverage.
*Requires sample geography from currentAssess_construct.R. Also merges hedonic indices here with the CoreLogic hedonic indices. This requires output from HedonicIndex.do.

cd $proj_filepath

import delimited using "DataV2/US_Data/NHGIS/nhgis_20205_blck_grp.csv", clear


*Renaming data to our standard convention
rename statea State
rename countya County
rename tracta Tract
rename blck_grpa BlockGroup

*Refer to codebook for variable names

*house values (for regression)
rename amwbe001 med_houseval

*Creating controls to construct index. Use only owner occupied housing units

*1: structure types distribution
forval i = 3/12 {
    
	if `i' < 10 {
		cap gen StructureType_`i' = amu5e00`i'/amu5e002
	}
	
	if `i' >= 10 {
		cap gen StructureType_`i' = amu5e0`i'/amu5e002
	}
	
}

forval i = 3/12 {
	
local shift_i = `i' + 11
	
replace StructureType_`i' = amu5e0`shift_i'/amu5e013 if missing(StructureType_`i')
	
}

*2: Number of rooms
forval i = 3/11 {
	
	if `i' < 10 {
		cap gen RoomShare_`i' = amu1e00`i'/amu1e002
	}
	
	if `i' >= 10 {
		cap gen RoomShare_`i' = amu1e0`i'/amu1e002
	}
}

*replace by renter if missing
forval i = 3/11 {
	
local shift_i = `i' + 10
	
replace RoomShare_`i' = amu1e0`shift_i'/amu1e002 if missing(RoomShare_`i')
	
}


*Year structure built shares
forval i = 3/12 {
	
		
	if `i' < 10 {
		cap gen YearBuilt_`i' = amu9e00`i'/amu9e002
	}
	
	if `i' >= 10 {
		cap gen YearBuilt_`i' = amu9e0`i'/amu9e002
	}
}

forval i = 3/12 {
	
local shift_i = `i' + 11
	
replace YearBuilt_`i' = amu9e0`shift_i'/amu9e013 if missing(YearBuilt_`i')
	
}

*Bedrooms
forval i = 3/8 {
	
		
	if `i' < 10 {
		cap gen Bedroom_`i' = amvfe00`i'/amvfe002
	}
	
	if `i' >= 10 {
		cap gen Bedroom_`i' = amvfe0`i'/amvfe002
	}
}

forval i = 3/8 {
	
local shift_i = `i' + 7
	
replace Bedroom_`i' = amvfe0`shift_i'/amvfe009 if missing(Bedroom_`i')
	
}

*Heating (note, only for occupied units) 
forval i = 2/10 {
	if `i' < 10 {
		cap gen Heating_`i' = amvde00`i'/amvde001
	}
	
	if `i' >= 10 {
		cap gen Heating_`i' = amvde0`i'/amvde001
	}
}

*Plumbing
gen Plumbing = amvle003/amvle002
replace Plumbing = amvle006/amvle005 if missing(Plumbing)

*Kitchen
gen Kitchen = amvoe003/amvoe002
replace Kitchen = amvoe006/amvoe005 if missing(Kitchen)

*keeping variables
keep State County Tract BlockGroup med_houseval StructureType* RoomShare* YearBuilt* Bedroom* Heating* Plumbing* Kitchen*

*Should be 197,062 block groups matched
merge 1:1 State County Tract BlockGroup using "DataV2/US_Data/Output/SampleGeography.dta", keep(matched)
drop ALAND

*Running secondary hedonic regression
xtset CBSA
gen log_med_houseval = log(med_houseval)

xtreg log_med_houseval StructureType_* RoomShare_* YearBuilt_* Bedroom_* Plumbing Kitchen, fe robust
predict Secondary_hedonicPrice, ue

replace Secondary_hedonicPrice = exp(Secondary_hedonicPrice)

*Merging hedonic price with corelogic by regulated and unregulated structures
drop _merge
merge 1:1 State County Tract BlockGroup using "DataV2/US_Data/Output/CoreLogicHedonicIndex.dta"

*Corelogic index is constructed for about something like 175,000 out of 197,000 block groups. 
*Constructing a final index for regulated and unregulated sturctures that maximizes spatial coverage
drop _merge

sort State County Tract BlockGroup
*Rescaling the secondary hedonic index so that it has the same mean and log variance as the CoreLogic indices.


*rescaling Secondary_hedonicPrice now
*Getting mean and sd of log hedonicPrice
cap gen log_hedonicPrice = log(hedonicPrice_total)
sum log_hedonicPrice 
local log_hedonicPrice_mean = `r(mean)'
local log_hedonicPrice_sd = `r(sd)'

cap gen log_Secondary_hedonicPrice = log(Secondary_hedonicPrice)
sum log_Secondary_hedonicPrice 
replace log_Secondary_hedonicPrice = (log_Secondary_hedonicPrice - `r(mean)')/`r(sd)'
replace log_Secondary_hedonicPrice = `log_hedonicPrice_mean' + log_Secondary_hedonicPrice*`log_hedonicPrice_sd'

*Generating adjusted hedonic index
gen adjusted_Secondary_hedonicPrice = exp(log_Secondary_hedonicPrice)

*Replacin regulated and unregulated Hedonic prices with this when missing (as a supplementary index)
replace hedonicPrice_regulated = adjusted_Secondary_hedonicPrice if missing(hedonicPrice_regulated)
replace hedonicPrice_total = adjusted_Secondary_hedonicPrice if missing(hedonicPrice_total)

count if missing(hedonicPrice_regulated)
count if missing(hedonicPrice_total)

*Only about 11,000 neighborhoods (~5600 block groups) remain where we cannot construct a hedonic index (7000 for regulated locations). Replacing with CBSA values where we cannot impute. This is to make the sample size as large as possible
bysort CBSA : egen CBSA_hedonicPrice_regulated = mean(hedonicPrice_regulated)
bysort CBSA : egen CBSA_hedonicPrice_total = mean(hedonicPrice_total)

replace hedonicPrice_regulated = CBSA_hedonicPrice_regulated if missing(hedonicPrice_regulated)
replace hedonicPrice_total = CBSA_hedonicPrice_total if missing(hedonicPrice_total)

*We now have a hedonic price for all observations in sample (ignoring ~800 empty block groups that will be dropped later!)
keep State County Tract BlockGroup CBSA hedonicPrice* CBSA_hedonicPrice*

save "DataV2/US_Data/Output/FullHedonicIndex_complete.dta", replace
