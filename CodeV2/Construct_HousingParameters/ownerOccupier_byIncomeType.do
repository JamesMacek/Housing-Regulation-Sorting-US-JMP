*This file calculates owner occupier shares by income type  (to better compare landlords to households) + 
*Shares of yearly income from labour vs implicit rent on owned housing 

cd $proj_filepath 

use "DataV2/US_Data/ACS_Individual/acs_2015_2019.dta", clear

****ASSIGNING METRO TO HOUSEHOLDS IN SAMPLE*************************************
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
*Generating income-type-by-CBSA expenditure shares 
rename met2013 CBSA
merge m:1 CBSA using "DataV2/US_Data/Output/CityProd_individual.dta"

*Dropping if income censored + unemployed (i.e. temporary income)
drop if hhincome >= 9999998 | hhincome <= 0

*adjusting household income for inflation using BoLS Data
replace hhincome = hhincome*1.10 if multyear == 2015 
replace hhincome = hhincome*1.09 if multyear == 2016 
replace hhincome = hhincome*1.05 if multyear == 2017
replace hhincome = hhincome*1.04 if multyear == 2018
replace hhincome = hhincome*1.02 if multyear == 2019


*Cleaning house and rent values
duplicates drop serial, force

*Ability_distribution
*These are city deflated wages-- measure of individual and not city productivity/labour supply
gen Ability = hhincome/PooledWage

*TARGET 3: DISCRETIZED ABILITY DISTRIBUTIONS 
*Bins defined by 0-25k, 25k-50kth, 50-75kth, 75k-100kth, 100k-150kth, 150k-200kth, 200k+
*Note: these are the ranges for the income distributions in the 2015-2019 ACS. 
gen ability_ind = 0

*Ability group bins
replace ability_ind = 1 if 0 <= Ability & Ability < 25000
replace ability_ind = 2 if 25000 <= Ability & Ability < 50000
replace ability_ind = 3 if 50000 <= Ability & Ability < 75000
replace ability_ind = 4 if 75000 <= Ability & Ability < 100000
replace ability_ind = 5 if 100000 <= Ability & Ability < 150000
replace ability_ind = 6 if 150000 <= Ability & Ability < 200000
replace ability_ind = 7 if 200000 <= Ability 

*Dummy if owneroccupier
gen owneroccupier = (valueh != 9999999)

*Dropping house values that arent imputed moving forward
replace valueh = . if valueh==0 | valueh >= 9999999

*Owner occupier rates by income type
cap ssc install _gwtmean
bysort ability_ind: egen ownerOccupier_rate = wtmean(owneroccupier), weight(hhwt)

*Share of income from implicit rent on housing services by income type
*I.e. share of income from rent on housing services vs share of income earned from all other sources
*This is to calculate the change in welfare comparing the variation ($ measure) arising from fall in rents + amenity value against variation arising from fall in housing wealth. 

*For spending shares on housing, payments for mortgages assume price to rent ratio by year from PriceRentRatios.do
*Weird-- this is consistently lower than rents. Why?
drop _merge
merge m:1 CBSA multyear using "DataV2/US_Data/Output/price_to_rent_by_city.dta"
drop _merge

*Adjust yearly payment as owner cost of housing (to net of saving effects)
*to weight against income 
gen yrlywealth_equivalent = valueh/price_to_rent_national
replace yrlywealth_equivalent = yrlywealth_equivalent*1.02 if multyear == 2019
replace yrlywealth_equivalent = yrlywealth_equivalent*1.04 if multyear == 2018
replace yrlywealth_equivalent = yrlywealth_equivalent*1.05 if multyear == 2017
replace yrlywealth_equivalent = yrlywealth_equivalent*1.09 if multyear == 2016 
replace yrlywealth_equivalent = yrlywealth_equivalent*1.10 if multyear == 2015
*(adjusting for inflation as we did with hhincome)

*Summing all hhincome of owner occupiers, assuming they only own housing wealth and renters do not
bysort ability_ind: egen sum_hhincome = total(hhincome*hhwt)
bysort ability_ind: egen sum_yrlywealth_equivalent = total(yrlywealth_equivalent*hhwt)
gen housing_sh_of_total_wealth = sum_yrlywealth_equivalent/(sum_yrlywealth_equivalent + sum_hhincome)
*Does this make sense? Yes-- this is implicit rent earned on housing services by owner occupiers!
*May need to adjust this later

*Note: some dividends earned on housing wealth will show up in household income (returns in firms that hold RE portfolios, personal investment properties, etc). So this will understate fraction of income from dividends on housing services, though probably not by much. 


keep ability_ind ownerOccupier_rate housing_sh_of_total_wealth
duplicates drop

*Saving file
save "DataV2/US_Data/Output/ownerOccupier_rate.dta", replace

