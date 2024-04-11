*This do file looks at (the city and national level) price to rents. 
cd $proj_filepath 

use "DataV2/US_Data/ACS_Individual/acs_2015_2019.dta", clear

****ASSIGNING METRO TO HOUSEHOLDS IN SAMPLE****
*dropping observations that are for sure not in 2013 MSAs
drop if metro == 1

*Matching all observations to 2010 PUMAs

*merging m:m, only duplicates are for Corvalis and Midland, Michigan
merge m:m statefip puma using "DataV2/US_Data/ACS_Individual/MSA2013_PUMA2010_crosswalk.dta"
sort _merge
cap count if _merge == 0
di "There are `r(N)' unmatched households. "

drop _merge

*Generating indicator that metro was in IPUMS sample, so no replacements happen for cities that were in IPUMS metro sample
gen IPUMS_metro = 0

levelsof met2013, local(levels)
foreach i of local levels {
	if `i' != 0{
		replace IPUMS_metro = 1 if met2013_from_crosswalk_2010 == `i'
	}
}

*Constructing new met2013 variables.
replace met2013 = met2013_from_crosswalk_2010 if met2013 == 0 & IPUMS_metro == 0

*Finally drop if not in met2013 metro after procedure, or in not in contiguous US
drop if met2013 == 0
drop if statefip == 15 | statefip == 2 

*Checking how many metros survive
levelsof met2013, local(levels)
local cnt = 0
foreach i of local levels {
    local cnt = `cnt' + 1
}
********************************************************************************


replace valueh = . if valueh==0 | valueh >= 9999999

*converting rent to yearly payments
gen yrlyrent = rent*12 
*Lots of zeros in the data (owneroccupied)
replace yrlyrent = . if yrlyrent == 0

rename valueh  med_housevalue
rename yrlyrent  med_rent_yrly

*Price to rents = median houseprice/median rents
rename met2013 CBSA

*Calculating aggregate price to rent
cap ssc install egenmore
bysort multyear: egen National_housevalue = wpctile(med_housevalue), p(50) weight(hhwt)
bysort multyear: egen National_rent_yrly = wpctile(med_rent_yrly), p(50) weight(hhwt)

gen price_to_rent_national = National_housevalue/National_rent_yrly

collapse (median) med_housevalue med_rent_yrly price_to_rent_national [pw = hhwt], by(CBSA multyear)

gen price_to_rent = med_housevalue/med_rent_yrly

drop med_*

sort CBSA multyear

save "DataV2/US_Data/Output/price_to_rent_by_city.dta", replace
