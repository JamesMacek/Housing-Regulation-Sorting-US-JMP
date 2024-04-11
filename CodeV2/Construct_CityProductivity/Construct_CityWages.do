
*This do file constructs wage distributions by city. Requires crosswalk output.
*Calculates for 2010/2022 etc
cd $proj_filepath 


*Start loop over samples
foreach sampl in "current" "historical" {
	
	if "`sampl'" == "current" {
		local sampleName  ""
		local ACS_name "2015_2019"
	}
	
	if "`sampl'" == "historical" {
		local sampleName  "_historical"
		local ACS_name "2008_2012"
	}

	use "DataV2/US_Data/ACS_Individual/acs_`ACS_name'.dta", clear

	*dropping observations that are for sure not in 2013 MSAs
	drop if metro == 1

	*Matching all observations to 2010 PUMAs

	*merging m:m, only duplicates are for Corvalis and Midland, Michigan
	joinby statefip puma using "DataV2/US_Data/ACS_Individual/MSA2013_PUMA2010_crosswalk.dta", unmatched(master)
	drop if _merge == 1
	drop _merge
	*no unmatched

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

	di "There are `cnt' cities in the new sample"
	*374 out of 377 cities remain. Dropped cities are very small, with tons of overlap with others. 
	*These are Corvalis, OR, 
	*_______________________________________________________________________________________
	*PART 2, wages and regressions
	*Constructing gross hourly wages

	drop if empstat == 0 | empstat == 2 | empstat == 3
	*Drop missing wage data
	drop if incwage >= 999998 | incwage == 0

	*Weeks worked, intervalled in ACS, use midpoint of range
	drop if wkswork2 == 0
	gen wkswork =.
	replace wkswork = 51 if wkswork2 == 6
	replace wkswork = 48.5 if wkswork2 == 5
	replace wkswork = 43.5 if wkswork2 == 4
	replace wkswork = 33 if wkswork2 == 3
	replace wkswork = 20 if wkswork2 == 2
	replace wkswork = 6 if wkswork2 == 1
	
	*Hours worked (we want productivity to not relate to systematic differences in time use across cities)
	gen log_hr_wage = log((incwage/wkswork)/uhrswork)
	
	*dropping if 75 percent below national minimum wage in 2015
	drop if exp(log_hr_wage) < 0.75*7.25
	
	*Constructing education indicators
	gen college = 1 if educd >= 101 
	replace college = 0 if missing(college) == 1

	*PART 2.2: regressions	
	*Squared term to fit life cycle
	gen agesq = age^2

	*Year dummies to rule out time variation
	levelsof multyear, local(levels)
	foreach yr of local levels {
		gen multyear_`yr' = (multyear == `yr')
		
	}
	

	*local containing control variables
	local ctrl_vars i.occ2010 i.sex i.race i.ancestr1 i.educd multyear_* i.citizen age agesq

	*xtsetting for metarea2013 FE's
	xtset met2013

	*Want productivity distributions by skill (for robustness) and pooled (assuming everyone is the same skill level)
	*regressions by college group

	xtreg log_hr_wage `ctrl_vars' if college == 1, fe robust
	*predict fixed effect + residual
	predict CollegeWage, u 
	replace CollegeWage = exp(CollegeWage)

	xtreg log_hr_wage `ctrl_vars' if college == 0, fe robust 
	predict NoCollegeWage, u
	replace NoCollegeWage = exp(NoCollegeWage)

	xtreg log_hr_wage `ctrl_vars', fe robust
	predict PooledWage, u
	replace PooledWage = exp(PooledWage)


	*Calculating unweighted average city productivity
	cap ssc install _gwtmean
	local wageGroups "College NoCollege Pooled"
	local wageGroupSize: word count `wageGroups'

	forval i = 1/`wageGroupSize' {
	
		local curWage: word `i' of `wageGroups'
	
		bysort met2013: egen `curWage'_obs_cnt = count(`curWage'Wage) if missing(`curWage'Wage) == 0
		*inverse weights
		replace `curWage'_obs_cnt = 1/`curWage'_obs_cnt
	
		*Generating productivity under the normalization that an average city has wage == 1.
		egen Mean`curWage'Wage = wtmean(`curWage'Wage), weight(`curWage'_obs_cnt)
		replace `curWage'Wage = `curWage'Wage/Mean`curWage'Wage
	}

	
	*Part Three: Generating an ability distributions by education. Note-- we USE HOUSEHOLD INCOME, NOT INCOME EARNED BY LABOUR. 
	*Only do this for contemporary sample
	
	if "`sampl'" == "current" {

		*Dropping if income censored, or household making loss
		*Very few observations dropped. 
		drop if hhincome >= 9999998 | hhincome <= 0

		*adjusting household income for inflation using BoLS Data in 2020 dollars	
		replace hhincome = hhincome*1.10 if multyear == 2015 
		replace hhincome = hhincome*1.09 if multyear == 2016 
		replace hhincome = hhincome*1.05 if multyear == 2017
		replace hhincome = hhincome*1.04 if multyear == 2018
		replace hhincome = hhincome*1.02 if multyear == 2019

		*Note we use individual data, but hhincome only varies at household level, and education varies by individual. Using the 	 following aggregation procedure that assumes the fraction of households who are of a particular 

		*These are city deflated wages-- measure of individual and not city productivity/labour supply
		gen Ability = hhincome/PooledWage

		*TARGET 3: DISCRETIZED ABILITY DISTRIBUTIONS 
		*Bins defined by 0-25k, 25k-50kth, 50-75kth, 75k-100kth, 100k-150kth, 150k-200kth, 200k+

		*Calculating measure of center for each bin. (one that weighs households evenly.)	
		bysort serial: egen hh_count = total(perwt)
		gen wt_scheme = hhwt/hh_count


		forval i = 1/7 {
			cap gen share_pct`i' = .
		}


		*Note: these are the ranges for the income distributions in the 2015-2020 ACS (unadjusted)
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
		*This will be our support for the ability distribution (we keep the same support for the extended model with skill group differences)
	
	*end construction of ability distribution
	} 

	*Collapsing by MSA and and saving for use with other programs.
	if "`sampl'" == "current" {
		collapse (mean) PooledWage CollegeWage NoCollegeWage ability_grp*, by(met2013)
	}
	
	if "`sampl'" == "historical" {
		collapse (mean) PooledWage CollegeWage NoCollegeWage, by(met2013)
	}
	
	sort PooledWage

	*renaming for other datasets
	rename met2013 CBSA
	save "DataV2/US_Data/Output/CityProd_individual`sampleName'.dta", replace

}

