*This file extracts college shares for each of the 7 income bins by city. 
*This is used in the calibration of the robust model.
*Requires crosswalk output.
cd $proj_filepath

use "DataV2/US_Data/ACS_Individual/acs_2015_2019.dta", clear

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


*Part 2: Constructing entire college shares
*NOT INCOME EARNED BY LABOUR. 
*Dropping if income censored, or household making loss
drop if hhincome >= 9999998 | hhincome <= 0

*adjusting household income for inflation using BoLS Data in 2020 dollars
replace hhincome = hhincome*1.10 if multyear == 2015 
replace hhincome = hhincome*1.09 if multyear == 2016 
replace hhincome = hhincome*1.05 if multyear == 2017
replace hhincome = hhincome*1.04 if multyear == 2018
replace hhincome = hhincome*1.02 if multyear == 2019

*college ID's
gen college = 1 if educd >= 101 
replace college = 0 if missing(college) == 1

*Drop non-adults 
drop if age<23

*College shares by household
bysort multyear serial: egen hh_college_ct = sum(college*perwt)
bysort multyear serial: egen hh_ct = sum(perwt)
gen hh_college_share = hh_college_ct/hh_ct
drop hh_ct hh_college_ct

*Constructing income groups for use with NHGIS data
gen income_group = 0
replace income_group = 1 if 0 <= hhincome & hhincome < 25000
replace income_group = 2 if 25000 <= hhincome & hhincome < 50000
replace income_group = 3 if 50000 <= hhincome & hhincome < 75000
replace income_group = 4 if 75000 <= hhincome & hhincome < 100000
replace income_group = 5 if 100000 <= hhincome & hhincome < 150000
replace income_group = 6 if 150000 <= hhincome & hhincome < 200000 
replace income_group = 7 if 200000 <= hhincome

*Calculating average income by income group to use with discretization process
bysort income_group met2013: egen hh_income_sum = sum(hhincome*hhwt)
bysort income_group met2013: egen hh_ct = sum(hhwt)
gen hhincome_bin = hh_income_sum/hh_ct
drop hh_income_sum hh_ct

*Calculating college share by MSA and income group
bysort income_group met2013: egen hh_college_share_sum = sum(hh_college_share*hhwt)
bysort income_group met2013: egen hh_ct = sum(hhwt)
gen income_bin_College_share = hh_college_share_sum/hh_ct
drop hh_college_share_sum hh_ct hhincome_bin

*collapsing by income group and assigning value labels
collapse (mean) income_bin_College_share, by(income_group met2013)

*Reshaping so that all variables lie on a row
reshape wide income_bin_College_share, i(met2013) j(income_group)

*rename for merge
rename met2013 CBSA

*Saving data
save "DataV2/US_Data/Output/CBSA_Income_bins_by_skill.dta", replace
