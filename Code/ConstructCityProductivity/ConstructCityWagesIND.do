cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*This file performs a Mincer type regression to yeild city specific productivities by skill group.
*Following Baum-Snow and Pavan 2017

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

*_______________________________________________________________________________________
*PART 2, wages and regressions
*Constructing gross hourly wages

drop if empstat == 0 | empstat == 2 | empstat == 3
*Drop missing wage data
drop if incwage >= 999998 | incwage == 0

	*Weeks worked, intervalled in ACS
	drop if wkswork2 == 0
	gen wkswork =.
	replace wkswork = 51 if wkswork2 == 6
	replace wkswork = 48.5 if wkswork2 == 5
	replace wkswork = 43.5 if wkswork2 == 4
	replace wkswork = 33 if wkswork2 == 3
	replace wkswork = 20 if wkswork2 == 2
	replace wkswork = 6 if wkswork2 == 1
	
	*Hours worked
	
gen log_hr_wage = log((incwage/wkswork)/uhrswork)

	*dropping if 75 percent below minimum wage
	drop if exp(log_hr_wage) < 0.75*7.25

*Constructing education indicators
gen college = 1 if educd >= 101 
replace college = 0 if missing(college) == 1

*PART 2.2: regressions	
*Squared term to fit life cycle
gen agesq = age^2

*Create year dummies to express productivity in 2012 terms
*I.e in terms of the 2012 time fixed effect, accounts for time variation we may not care about.
gen multyear_2008 = 1 if multyear == 2008
replace multyear_2008 = 0 if multyear != 2008
gen multyear_2009 = 1 if multyear == 2009
replace multyear_2009 = 0 if multyear != 2009
gen multyear_2010 = 1 if multyear == 2010
replace multyear_2010 = 0 if multyear != 2010
gen multyear_2011 = 1 if multyear == 2011
replace multyear_2011 = 0 if multyear != 2011


*local containing control variables
local ctrl_vars i.occ2010 i.sex i.race i.ancestr1 i.educd multyear_* i.citizen age agesq

*xtsetting for metarea2013 FE's
xtset met2013

*regressions by college group
xtreg log_hr_wage `ctrl_vars' if college == 1, fe robust
predict CollegeWage, u 
replace CollegeWage = exp(CollegeWage)

xtreg log_hr_wage `ctrl_vars' if college == 0, fe robust 
predict NoCollegeWage, u
replace NoCollegeWage = exp(NoCollegeWage)


*demeaning wages by MSA
bysort met2013: egen CollegeObsCount = count(CollegeWage) if missing(CollegeWage) == 0
replace CollegeObsCount = 1/CollegeObsCount
bysort met2013: egen NoCollegeObsCount = count(NoCollegeWage) if missing(NoCollegeWage) == 0
replace NoCollegeObsCount = 1/NoCollegeObsCount

egen MeanCollegeWage = wtmean(CollegeWage), weight(CollegeObsCount)
egen MeanNoCollegeWage = wtmean(NoCollegeWage), weight(NoCollegeObsCount)

*Demeaned by average productivity weighted equally by MSA
replace CollegeWage = CollegeWage/MeanCollegeWage
replace NoCollegeWage = NoCollegeWage/MeanNoCollegeWage

*Part Three: Generating an ability distributions by education. Note-- we USE HOUSEHOLD INCOME HERE as a measure of affordability. 
*NOT INCOME EARNED BY LABOUR. 
*Dropping if income censored, or household making loss
*Very few observations dropped. 
drop if hhincome >= 9999998 | hhincome <= 0

*adjusting household income for inflation using BoLS Data
replace hhincome = hhincome*1.07 if multyear == 2008 | multyear == 2009 
replace hhincome = hhincome*(1.07/1.01) if multyear == 2010
replace hhincome = hhincome*(1.07/1.04) if multyear == 2011


*Note we use individual data, but hhincome only varies at household level, and education varies by individual. Using the following aggregation procedure
gen Ability = hhincome/CollegeWage if missing(CollegeWage) == 0
replace Ability = hhincome/NoCollegeWage if missing(NoCollegeWage) == 0

*TARGET 3: DISCRETIZED ABILITY DISTRIBUTIONS 
*Bins defined by 0-25k, 25k-50kth, 50-75kth, 75k-100kth, 100k-150kth, 150k-200kth, 200k+
*Calculating measure of center for each bin.
bysort serial: egen hh_count = total(perwt)
gen wt_scheme = hhwt/hh_count


forval i = 1/7 {
	cap gen share_pct`i' = .
}


replace share_pct1 = Ability if 0 <= Ability & Ability < 25000 
replace share_pct2 = Ability if  25000 <= Ability & Ability < 50000 
replace share_pct3 = Ability if 50000 <= Ability & Ability < 75000 
replace share_pct4 = Ability if 75000 <= Ability & Ability < 100000 
replace share_pct5 = Ability if 100000 <= Ability & Ability < 150000 
replace share_pct6 = Ability if 150000 <= Ability & Ability < 200000 
replace share_pct7 = Ability if 200000 <= Ability 



forval i=1/7 {
egen ability_grp`i' = wtmean(share_pct`i'), weight(wt_scheme)
}
*Not much difference between averages for college vs non-college in each bin. 



*Collapsing and saving
collapse (mean) CollegeWage NoCollegeWage ability_grp*, by(met2013)

*renaming for other datasets
rename met2013 CBSA


save "Data/US_Data/Output/CityProd_individual", replace
