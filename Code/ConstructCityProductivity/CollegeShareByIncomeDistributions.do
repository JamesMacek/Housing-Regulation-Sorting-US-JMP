cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*This file constructs the MSA specific shares of college household members by income group.
*requires output from the PUMA-MSA crosswalk. 
*outputs average income by bin and college shares by bin for use with calibration. 

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

*Dropping if income censored
drop if hhincome >= 9999998 | hhincome <= 0

*adjusting household income for inflation using BoLS Data
replace hhincome = hhincome*1.07 if multyear == 2008 | multyear == 2009 
replace hhincome = hhincome*(1.07/1.01) if multyear == 2010
replace hhincome = hhincome*(1.07/1.04) if multyear == 2011

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
gen hhincome_bin_collegeshare = hh_college_share_sum/hh_ct
drop hh_college_share_sum hh_ct

*collapsing by income group and assigning value labels
collapse (mean) hhincome_bin hhincome_bin_collegeshare, by(income_group met2013)

*Reshaping so that all variables lie on a row
reshape wide hhincome_bin hhincome_bin_collegeshare, i(met2013) j(income_group)

*rename for merge
rename met2013 CBSA

save "Data/US_Data/Output/Income_bins.dta", replace
