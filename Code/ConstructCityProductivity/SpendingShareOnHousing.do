cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*Measures the percentage of income spent on housing to target Beta in the counterfactua;
*Raw IPUMS extract
use "Data/US_Data/ACSIndividual/ACS20082012IND.dta", replace

*matching metros to PUMAs for both pre 2012 and 2012
gen d2012 = 1 if multyear == 2012
replace d2012 = 0 if missing(d2012)

*merging m:m, only duplicates are for Corvalis and Midland, Michigan
merge m:m statefip puma using "Data/US_Data/ACSIndividual/MSA2013_PUMA2010_crosswalk.dta"

*Only puerto rico PUMAs unmatched from using data
drop if _merge == 2 
drop _merge
replace met2013_from_crosswalk_2010 = 0 if multyear != 2012 | missing(met2013_from_crosswalk_2010)

*Matching for other years, as 2010 PUMAs only for 2012 ACS sample
merge m:m statefip puma using "Data/US_Data/ACSIndividual/MSA2013_PUMA2000_crosswalk.dta"
*3 unmatched in Louisiana, something to do with Katrina

drop if _merge == 2
drop _merge
replace met2013_from_crosswalk_2000 = 0 if multyear == 2012 | missing(met2013_from_crosswalk_2000)

*Generating indicator that metro was in IPUMS sample, so no replacements happen for cities that were in IPUMS metro sample
gen IPUMS_metro = 0

levelsof met2013, local(levels)
foreach i of local levels {
	if `i' != 0{
		replace IPUMS_metro = 1 if met2013_from_crosswalk_2000 == `i' | met2013_from_crosswalk_2010 == `i'
	}
}


*Constructing new met2013 variables.
replace met2013 = met2013_from_crosswalk_2010 if met2013 == 0 & multyear == 2012 & IPUMS_metro == 0

replace met2013 = met2013_from_crosswalk_2000 if met2013 == 0 & multyear != 2012 & IPUMS_metro == 0

*Finally drop if not in met2013 metro, or in Hawaii
drop if met2013 == 0
drop if statefip == 15 

*Dropping if income censored + unemployed (i.e. temporary income)
drop if hhincome >= 9999998 | hhincome <= 0

*adjusting household income for inflation using BoLS Data
replace hhincome = hhincome*1.07 if multyear == 2008 | multyear == 2009 
replace hhincome = hhincome*(1.07/1.01) if multyear == 2010
replace hhincome = hhincome*(1.07/1.04) if multyear == 2011

*Cleaning house and rent values
duplicates drop serial, force
replace valueh = . if valueh==0 | valueh >= 9999999

*Yearly payments for mortgages assume price to rent ratio of 12. 
local interest 0.05
gen yrlymortgagepayment = valueh/12 

replace rent = . if rent==0
*converting to yearly payments
gen yrlyrent = rent*12 

*taking household income to housepmt

gen house_pmt = yrlymortgagepayment
replace house_pmt = yrlyrent if missing(house_pmt)
*adjusting housing payments to 2012 dollars (this does not matter for any calculations below)
replace house_pmt = house_pmt*1.07 if multyear == 2008 | multyear == 2009 
replace house_pmt = house_pmt*(1.07/1.01) if multyear == 2010
replace house_pmt = house_pmt*(1.07/1.04) if multyear == 2011

*generating weighted average by household, dividing
egen ag_house_pmt = wtmean(house_pmt), weight(hhwt)
egen ag_income = wtmean(hhincome), weight(hhwt)

*generating weighted percentile of housing spending shares to check
gen house_spendshare = house_pmt/hhincome

gen target_spend_share = ag_house_pmt/ag_income
*requires Egenmore package
egen target_spend_share2 = wpctile(house_spendshare), p(50) weight(hhwt)

*Target spending share is almost EXACTLY 0.2-- these yeild the same thing, on an average income of 79,607 in sample.
