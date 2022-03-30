cd "Z:/Dropbox/Schoolfolder/Projects/Zoning/Data/US_Data"

use "ACS PUMS/ACS_data.dta", clear

*File does some preliminary cleaning for superstar unit table. 
*Note: Using 2013 metro area definitions in this.


*PART1: household income + household size sample 
drop if metro == 0 | metro == 1 | met2013 == 0

*Dropping if in group quarter (i.e. not in a private household)
keep if gq == 1 | gq == 2

*Drop mobile homes/etc 
drop if unitsstr == 1 | unitsstr == 2

*2012-2008 sample
keep if year<=2012

*only using household level variables
duplicates drop serial, force

*MSA median house prices 
replace valueh = . if valueh==9999999 |  valueh==0
levelsof met2013
gen msa_med_houseval =.

foreach lev in `r(levels)' {
	quietly summarize valueh [w=hhwt] if met2013 == `lev', detail
	replace msa_med_houseval = r(p50) if met2013 == `lev'
	
}

*Superstar sample
summarize msa_med_houseval, detail
gen top25 = 0
replace top25 = 1 if msa_med_houseval >= r(p75) 
gen bot25 = 0
replace bot25 = 1 if msa_med_houseval <= r(p25)

*labelling San Francisco and New York

*MSA household count + household/unit count
bysort met2013: egen methhwt = sum(hhwt)

*Generating distributions by unit type by MSA
bysort met2013 unitsstr: egen units_share = sum(hhwt) 
replace units_share = units_share/methhwt

*collapsing data at the unit_type-MSA level, demeaning all variables
drop if top25==0 & bot25==0
collapse (mean) hhincome methhwt units_share top25 [pw = hhwt], by(met2013 unitsstr)


*Save: 
save "ACS PUMS/ACS_data_cleaned_household.dta", replace

*Race variables to join with cleaned household data 
use "ACS PUMS/ACS_data.dta", clear

drop if metro == 0 | metro == 1 | met2013 == 0

*Dropping if in group quarter (i.e. not in a private household)
keep if gq == 1 | gq == 2

*Drop mobile homes/etc 
drop if unitsstr == 1 | unitsstr == 2

*2012-2008 sample
keep if year<=2012

gen white=0
replace white = 1 if race==1
gen black=0
replace black = 1 if race==2
gen minority=0
replace minority=1 if race != 1

*Generating MSA weights
bysort met2013: egen metpwt = sum(perwt)

collapse (mean) white black minority metpwt [pw = perwt], by(met2013 unitsstr)

save "ACS PUMS/ACS_data_race.dta", replace

use "ACS PUMS/ACS_data_cleaned_household.dta", replace 

joinby met2013 unitsstr using "ACS PUMS/ACS_data_race.dta", unmatched(both)

drop if top25==.

drop _merge
save "ACS PUMS/ACS_data_cleaned_household.dta", replace


*Collapse further into two datasets
collapse (mean) hhincome units_share [pw = methhwt], by(top25 unitsstr)
save "ACS PUMS/ACS_data_cleaned.dta", replace

use "ACS PUMS/ACS_data_cleaned_household.dta", clear
collapse (mean) white black minority [pw = metpwt], by(top25 unitsstr)

joinby top25 unitsstr using "ACS PUMS/ACS_data_cleaned.dta", unmatched(both)
drop _merge

*Demeaning at sample level
by top25: egen s_avg_income = mean(hhincome)
gen demeaned_hhincome = hhincome - s_avg_income
by top25: egen s_avg_whiteshare = mean(white)
gen demeaned_white = white - s_avg_whiteshare
by top25: egen s_avg_blackshare = mean(black)
gen demeaned_black = black - s_avg_blackshare
by top25: egen s_avg_minorityshare = mean(minority)
gen demeaned_minority = minority - s_avg_minorityshare


save "ACS PUMS/ACS_data_cleaned.dta", replace

*______________________________________________UNWEIGHTED STATISTICS
use "ACS PUMS/ACS_data.dta", clear

*File does some preliminary cleaning for superstar unit table. 
*Note: Using 2013 metro area definitions in this.


*PART1: household income + household size sample 
drop if metro == 0 | metro == 1 | met2013 == 0

*Dropping if in group quarter (i.e. not in a private household)
keep if gq == 1 | gq == 2

*Drop mobile homes/etc 
drop if unitsstr == 1 | unitsstr == 2

*2012-2008 sample
keep if year<=2012

*only using household level variables
duplicates drop serial, force

*MSA median house prices 
replace valueh = . if valueh==9999999 |  valueh==0
levelsof met2013
gen msa_med_houseval =.

foreach lev in `r(levels)' {
	quietly summarize valueh [w=hhwt] if met2013 == `lev', detail
	replace msa_med_houseval = r(p50) if met2013 == `lev'
	
}

*Superstar sample
summarize msa_med_houseval, detail
gen top25 = 0
replace top25 = 1 if msa_med_houseval >= r(p75) 
gen bot25 = 0
replace bot25 = 1 if msa_med_houseval <= r(p25)

*labelling San Francisco and New York

*MSA household count + household/unit count
bysort met2013: egen methhwt = sum(hhwt)

*Generating distributions by unit type by MSA
bysort met2013 unitsstr: egen units_share = sum(hhwt) 
replace units_share = units_share/methhwt

*collapsing data at the unit_type-MSA level, demeaning all variables
drop if top25==0 & bot25==0
collapse (mean) hhincome methhwt units_share top25 [pw = hhwt], by(met2013 unitsstr)


*Save: 
save "ACS PUMS/ACS_data_cleaned_household.dta", replace

*Race variables to join with cleaned household data 
use "ACS PUMS/ACS_data.dta", clear

drop if metro == 0 | metro == 1 | met2013 == 0

*Dropping if in group quarter (i.e. not in a private household)
keep if gq == 1 | gq == 2

*Drop mobile homes/etc 
drop if unitsstr == 1 | unitsstr == 2

*2012-2008 sample
keep if year<=2012

gen white=0
replace white = 1 if race==1
gen black=0
replace black = 1 if race==2
gen minority=0
replace minority=1 if race != 1

*Generating MSA weights
bysort met2013: egen metpwt = sum(perwt)

collapse (mean) white black minority metpwt [pw = perwt], by(met2013 unitsstr)

save "ACS PUMS/ACS_data_race.dta", replace

use "ACS PUMS/ACS_data_cleaned_household.dta", replace 

joinby met2013 unitsstr using "ACS PUMS/ACS_data_race.dta", unmatched(both)

drop if top25==.

drop _merge
save "ACS PUMS/ACS_data_cleaned_household.dta", replace


*Collapse further into two datasets
collapse (mean) hhincome units_share, by(top25 unitsstr)
save "ACS PUMS/ACS_data_cleaned_unweight.dta", replace

use "ACS PUMS/ACS_data_cleaned_household.dta", clear
collapse (mean) white black minority, by(top25 unitsstr)

joinby top25 unitsstr using "ACS PUMS/ACS_data_cleaned_unweight.dta", unmatched(both)
drop _merge

*Demeaning at sample level
by top25: egen s_avg_income = mean(hhincome)
gen demeaned_hhincome = hhincome - s_avg_income
by top25: egen s_avg_whiteshare = mean(white)
gen demeaned_white = white - s_avg_whiteshare
by top25: egen s_avg_blackshare = mean(black)
gen demeaned_black = black - s_avg_blackshare
by top25: egen s_avg_minorityshare = mean(minority)
gen demeaned_minority = minority - s_avg_minorityshare


save "ACS PUMS/ACS_data_cleaned_unweight.dta", replace

