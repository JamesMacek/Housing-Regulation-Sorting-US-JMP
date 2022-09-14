cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*This file measures a lower bound for self-reported house values in each CBSA.
*This will be used to flag low-priced transactions in the Zillow data for better cleaning.

*Requires output from

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


*PART 1:
*Generating lower bound on house values by MSA
*dropping missing house values
drop if valueh >= 9999999
drop if valueh == 0
drop if missing(valueh)

*keeping one observation per household
duplicates drop serial, force

*adjusting for inflation
replace valueh = valueh*1.07 if multyear == 2008 | multyear == 2009 
replace valueh = valueh*(1.07/1.01) if multyear == 2010
replace valueh = valueh*(1.07/1.04) if multyear == 2011


*Generating bottom .5% percent of house values to clean arms length transactions
bysort met2013: egen bottom_pct = pctile(valueh), p(1)
*Also top-cleaning top 0.5%, as some sales are clear outliers for whatever reason (transfers?)
bysort met2013: egen top_pct = pctile(valueh), p(99.9)

collapse (mean) bottom_pct top_pct, by(met2013)

rename met2013 CBSA

save "Data/ZillowData/Output/pct_houseval_ACS.dta", replace