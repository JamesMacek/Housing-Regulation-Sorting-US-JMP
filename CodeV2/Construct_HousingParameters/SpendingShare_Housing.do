*This do file constructs spending shares on housing using measures of after-tax income. 
*Requires crosswalk output. This is to calibrate housing demand parameters after accounting for regulation.
*This file also calculates shares of housing wealth by income type for an extended welfare analysis.  

cd $proj_filepath 

use "DataV2/US_Data/ACS_Individual/acs_2015_2019.dta", clear

****ASSIGNING METRO TO HOUSEHOLDS IN SAMPLE*************************************
*dropping observations that are for sure not in 2013 MSAs
drop if metro == 1

*Matching all observations to 2010 PUMAs

*merging m:m, only duplicates are for Corvalis and Midland, Michigan
joinby statefip puma using "DataV2/US_Data/ACS_Individual/MSA2013_PUMA2010_crosswalk.dta", unmatched(master)
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

*Tax progressivity parameter to identify after tax/transfer income to better calculate spending shares
*This is an approach taken in Finlay and Williams (2022) and is similar to Heathcote, Storesletten and Violante (2017) 
local TaxProgressivityParameter 0.174

bysort multyear: egen total_income = sum(hhincome*hhwt)
bysort multyear: egen total_tax_income = sum((hhincome^(1 -`TaxProgressivityParameter'))*hhwt)
gen adj_factor = total_income/total_tax_income

gen after_tax_income = adj_factor*(hhincome^(1- `TaxProgressivityParameter'))

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

*Dummy if owner occupier
gen owneroccupier = (valueh != 9999999)

*Dropping house values that arent imputed moving forward
replace valueh = . if valueh==0 | valueh >= 9999999

*For spending shares on housing, payments for mortgages assume price to rent ratio by year from PriceRentRatios.do
*Weird-- this is consistently lower than rents. Why?
drop _merge
merge m:1 CBSA multyear using "DataV2/US_Data/Output/price_to_rent_by_city.dta"
drop _merge

*Adjust yearly payment as owner cost of housing (to net of saving effects)
gen yrlymortgagepayment = valueh/price_to_rent_national

*Removing pro-bono rents
replace rent = . if rent==0

*converting to yearly payments
gen yrlyrent = rent*12 

*taking household income to housepmt
gen house_pmt = yrlymortgagepayment
replace house_pmt = yrlyrent if owneroccupier != 1

*adjusting housing payments to 2020 dollars (this does not matter for any calculations below)
replace house_pmt = house_pmt*1.02 if multyear == 2019
replace house_pmt = house_pmt*1.04 if multyear == 2018
replace house_pmt = house_pmt*1.05 if multyear == 2017
replace house_pmt = house_pmt*1.09 if multyear == 2016 
replace house_pmt = house_pmt*1.10 if multyear == 2015

*generating house spendshare
gen house_spendshare = house_pmt/after_tax_income
gen house_spendshare_notax = house_pmt/hhincome

*requires Egenmore package
cap ssc install egenmore

*Aggregate spending share by ability, + by owner/renter status, etc
bysort ability_ind: egen Income_spendshare_ag = wpctile(house_spendshare), p(50) weight(hhwt)
bysort ability_ind owneroccupier: egen Income_spendshare_owner = wpctile(house_spendshare), p(50) weight(hhwt)
egen spendshare_ag = wpctile(house_spendshare), p(50) weight(hhwt)
egen spendshare_ag_notax = wpctile(house_spendshare_notax), p(50) weight(hhwt)
bysort owneroccupier: egen spendshare_ag_owner = wpctile(house_spendshare_notax), p(50) weight(hhwt)


*Collapsing by CBSA and income type, reshaping
collapse (median) Income_spendshare* house_spendshare spendshare_ag* [pw = hhwt], by(CBSA ability_ind owneroccupier)

rename house_spendshare CBSA_spendshare

reshape wide CBSA_spendshare Income_spendshare*, i(CBSA owneroccupier) j(ability_ind)
reshape wide CBSA_spendshare* Income_spendshare_owner* spendshare_ag_owner, i(CBSA) j(owneroccupier)

save "DataV2/US_Data/Output/HousingSpendshare_byCity.dta", replace



