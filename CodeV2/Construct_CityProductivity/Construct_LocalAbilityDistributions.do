*This file constructed ability distributions by block group for pooled, college and noncollege workers. 
*Requires IncomeDist_ByCollegeShare and Construct_CityWages.do
*Requires crosswalk output.
*Also gets some other variables that will be useful later.

*______________________________________________________________
* PARAMETERS: 
* Population smoothing by low, medium and high types where zero populations by bin are observed
*
* This addresses granularity issues in the model by smoothing out zeros
local smoothPop_para 0.5
*Weighted average between current measured population and population levels in low, medium and high groups
*(number between 0 and 1). 
*Results are robust to any parameter here.
*Example: 0.5 means we impute zero observations at 50% of group mean. 
*_______________________________________________________________


*_________FOR THE CURRENT SAMPLE 2020__________________________

cd $proj_filepath
clear

*Reading NHGIS 2020 census 
import delimited using "DataV2/US_Data/NHGIS/nhgis_2020_blck_grp.csv", clear

*Constructing census housing unit counts from sample
rename u7g001 total_housing_units_cen
rename statea State
rename countya County
rename tracta Tract
rename blkgrpa BlockGroup
keep State County Tract BlockGroup total_housing_units_cen

*Merging with sample geography
merge 1:1 State County Tract BlockGroup using "DataV2/US_Data/Output/SampleGeography.dta", keep(using matched)
drop _merge
drop ALAND

*Saving temporary file
save "DataV2/US_Data/Output/tempgeo.dta", replace

*ACS 2016-2020 sample
import delimited using "DataV2/US_Data/NHGIS/nhgis_20205_blck_grp.csv", clear
rename statea State
rename countya County
rename tracta Tract
rename blkgrpa BlockGroup

*Merging with temp file
merge 1:1 State County Tract BlockGroup using "DataV2/US_Data/Output/tempgeo.dta", keep(using matched)

*Removing temp file from disk
erase "DataV2/US_Data/Output/tempgeo.dta"
drop _merge

*Merging with college bins from IncomeDist_ByCollegeShare.do
merge m:1 CBSA using "DataV2/US_Data/output/CBSA_Income_bins_by_skill.dta"

*Note: 3 Small MSAs dropped (Corvalis, OR, Midland, MI and Weirton-Steubenville, WV-OH)

*Using the seven income types, construct distributions for each block group.
*For both Pooled Model and for Skill-based model. 
cap gen Pooled_Ability_bin_1 = (amr7e002 + amr7e003 + amr7e004 + amr7e005)/amr7e001
cap gen Pooled_Ability_bin_2 = (amr7e006 + amr7e007 + amr7e008 + amr7e009 + amr7e010)/amr7e001
cap gen Pooled_Ability_bin_3 = (amr7e011 + amr7e012)/amr7e001
cap gen Pooled_Ability_bin_4 = (amr7e013)/amr7e001
cap gen Pooled_Ability_bin_5 = (amr7e014 + amr7e015)/amr7e001
cap gen Pooled_Ability_bin_6 = (amr7e016)/amr7e001
cap gen Pooled_Ability_bin_7 = (amr7e017)/amr7e001

*Note: we assume the nominal income bin == ability bin. Distributions are heavily aggregated anyways that this adjustment does not matter.  

*Calculating income distributions by skill under the independence assumption within cities

forval i = 1/7 {
	gen income_bin_NoCollege_share`i' = 1 - income_bin_College_share`i' 
}

forval i = 1/7 {
	
	foreach skillGroup in College NoCollege {
	*College + NoCollege (via Bayes Rule)
		gen `skillGroup'_Ability_bin_`i' = Pooled_Ability_bin_`i'*income_bin_`skillGroup'_share`i'
	}
}

*generating implied college share in each neighborhood (using MSA level variation only)
foreach skillGroup in College NoCollege {
	egen implied_`skillGroup'_share = rowtotal(`skillGroup'_Ability_bin_*)
}

*Each sums to 1.

*____________________________________
*Generating population distributions (ability bin * tot housing units)
*___________________________________
forval i = 1/7 {
	
	*Pooled
	gen Population_type_`i' = Pooled_Ability_bin_`i'*total_housing_units_cen
	
	*bySkill
	foreach skillGroup in College NoCollege {
	
		gen Population_type_`skillGroup'_`i' = `skillGroup'_Ability_bin_`i'*total_housing_units_cen
	
	
	}
	
}

*TESTING________________________________________________________________________________
*Generating smoothed population distributions to deal with granularity issues
*(No need to do this for the historical dataset, 
*		since we sum up with low-medium-high groups for Omega estimation anyways, inconsequential).
*________________________________________________________________________________

** low (l) = types 1, 2 ; medium (m) = types 3, 4 ; high (h) = types 5, 6, 7
*This aggregation limits the presence of zeros in each neighborhood!

*Generating imputation flag if smoothing required because of presence of zeros 
*Note: smooth similarly for college and noncollege estimates
foreach smoothedType in l m h {
    
	gen smooth_flag_`smoothedType' = 0
	
}
	
forval i = 1/7 {
	
	*low type imputation
	if `i' <= 2 {
	    replace smooth_flag_l = 1 if Population_type_`i' == 0
	}
	
	*medium type
	if `i'==3 | `i' ==4 {
	    replace smooth_flag_m = 1 if Population_type_`i' == 0
	}
	
	*high type
	if `i' >= 5 {
		replace smooth_flag_h = 1 if Population_type_`i' == 0
	}
	
}


*Generating averages by group for imputation
egen Population_avg_l = rowmean(Population_type_1 Population_type_2)
egen Population_avg_m = rowmean(Population_type_3 Population_type_4)
egen Population_avg_h = rowmean(Population_type_5 Population_type_6 Population_type_7)

foreach skillGroup in College NoCollege {
    egen Population_avg_`skillGroup'_l = rowmean(Population_type_`skillGroup'_1 Population_type_`skillGroup'_2)
	egen Population_avg_`skillGroup'_m = rowmean(Population_type_`skillGroup'_3 Population_type_`skillGroup'_4)
	egen Population_avg_`skillGroup'_h = rowmean(Population_type_`skillGroup'_5 Population_type_`skillGroup'_6 Population_type_`skillGroup'_7)
}

*Imputing if smooth_flag == 1
forval i = 1/7 {
    
	*low type imputation
	if `i' <= 2 {
	    replace Population_type_`i' = `smoothPop_para'*Population_avg_l + (1 - `smoothPop_para')*Population_type_`i' if smooth_flag_l == 1
	}
	
	*medium type imputation
	if `i'==3 | `i' ==4 {
	    replace Population_type_`i' = `smoothPop_para'*Population_avg_m + (1 - `smoothPop_para')*Population_type_`i' if smooth_flag_m == 1
	}
	
	*high type imputation
	if `i' >= 5 {
		replace Population_type_`i' = `smoothPop_para'*Population_avg_h + (1 - `smoothPop_para')*Population_type_`i' if smooth_flag_h == 1
	}
	
	*Doing the same by skill
	foreach skillGroup in College NoCollege {
	   
		*low type imputation
		if `i' <= 2 {
			replace Population_type_`skillGroup'_`i' = `smoothPop_para'*Population_avg_`skillGroup'_l + (1 - `smoothPop_para')*Population_type_`skillGroup'_`i' if smooth_flag_l == 1
		}
	
		*medium type imputation
		if `i'==3 | `i' ==4 {
			replace Population_type_`skillGroup'_`i' = `smoothPop_para'*Population_avg_`skillGroup'_m + (1 - `smoothPop_para')*Population_type_`skillGroup'_`i' if smooth_flag_m == 1
		}
	
		*high type imputation
		if `i' >= 5 {
			replace Population_type_`skillGroup'_`i' = `smoothPop_para'*Population_avg_`skillGroup'_h + (1 - `smoothPop_para')*Population_type_`skillGroup'_`i' if smooth_flag_h == 1
		}
			
	}

}
*Note: total population by group is preserved. 

*_______________________________________________________________________________

*Lastly, generating shares of households in regulated structures for use with other programs
gen regulated_housingUnit_share = (amu5e003 + amu5e004 + amu5e005 + amu5e006 + amu5e014 +  amu5e015 + amu5e016 + amu5e017)/(amu5e001)

*Fraction of households who report zero earnings
*(Wage + Salary)
gen noEarnings_share = amsme003/amsme001

*(Interest + Dividends)
gen noCapIncome_share = amsoe003/amsoe001

*(Social Security share)
gen noSocialSecurity_share = amsqe003/amsqe001

*Shares of renting households 
gen OwnerOccupier_share = amufe002/amufe001


*Selecting variables we need + saving
keep State County Tract BlockGroup CBSA *_Ability_bin_* implied_*_share Population_type* total_housing_units_cen regulated_housingUnit_share no*_share OwnerOccupier_share

save "DataV2/US_Data/Output/LocalAbilityDistributions.dta", replace



*_____________________DOING THE SAME FOR THE HISTORICAL SAMPLE_____________________
*Reading NHGIS 2020 census 
import delimited using "DataV2/US_Data/NHGIS/nhgis_2010_blck_grp.csv", clear

rename ifc001 total_housing_units_cen
rename statea State
rename countya County
rename tracta Tract
rename blkgrpa BlockGroup
keep State County Tract BlockGroup gisjoin total_housing_units_cen


*Saving temporary file
save "DataV2/US_Data/Output/tempgeo.dta", replace

*ACS 2008-2012 sample
import delimited using "DataV2/US_Data/NHGIS/nhgis_20105_blck_grp.csv", clear
rename statea State
rename countya County
rename tracta Tract
rename blkgrpa BlockGroup

*Merging with temp file
merge 1:1 State County Tract BlockGroup using "DataV2/US_Data/Output/tempgeo.dta", keep(using matched)


*erasing tempfiles
erase "DataV2/US_Data/Output/tempgeo.dta"
drop _merge

*Generating pooled ability bins (note: only do pooled for placebo test)
cap gen Pooled_Ability_bin_1 = (qu0e002 + qu0e003 + qu0e004 + qu0e005)/qu0e001
cap gen Pooled_Ability_bin_2 = (qu0e006 + qu0e007 + qu0e008 + qu0e009 + qu0e010)/qu0e001
cap gen Pooled_Ability_bin_3 = (qu0e011 + qu0e012)/qu0e001
cap gen Pooled_Ability_bin_4 = (qu0e013)/qu0e001
cap gen Pooled_Ability_bin_5 = (qu0e014 + qu0e015)/qu0e001
cap gen Pooled_Ability_bin_6 = (qu0e016)/qu0e001
cap gen Pooled_Ability_bin_7 = (qu0e017)/qu0e001

*Lastly, generating shares of households in regulated structures for use with other programs
gen regulated_housingUnit_share = (qyze003 + qyze004 + qyze005 + qyze006 + qyze014 +  qyze015 + qyze016 + qyze017)/(qyze001)

*Fraction of households who report zero earnings
*(Wage + Salary)
gen noEarnings_share = qvoe003/qvoe001

*(Interest + Dividends)
gen noCapIncome_share = qvre003/qvre001

*(Social Security share)
gen noSocialSecurity_share = qvse003/qvse001

*Keeping
keep State County Tract BlockGroup gisjoin *_Ability_bin_* Pooled* total_housing_units_cen regulated_housingUnit_share no*_share


*Matching to 2020 block group crosswalk
*State 2 digits, county 3 digits, tract 6 digits, block group 1 digit
gen State_tmp = substr(gisjoin, 2, 2)
gen County_tmp = substr(gisjoin, 5, 3)
gen Tract_tmp = substr(gisjoin, 9, 6)
gen BlockGroup_tmp = substr(gisjoin, 15, 1)
gen GEOID_2012 = State_tmp + County_tmp + Tract_tmp + BlockGroup_tmp
drop *_tmp gisjoin

*merging crosswalk
merge 1:m GEOID_2012 using "DataV2/US_Data/Shapefiles/blkgrp_2010_2020_crosswalk.dta", keep(matched master)
*None unmatched from master

*Collapsing to 2020 GEOIDs
*Note: Population variables are count data, should be summed;
*Share variables are not counts, should be meaned with analytical weights
collapse (mean) (Pooled* *_share) (sum) (total_housing_units_cen) [pw = wt_hh], by(GEOID_2020)

*Calculating ability bins
forval i = 1/7 {
	gen Population_type_`i' = Pooled_Ability_bin_`i'*total_housing_units_cen
}

drop Pooled*

*dropping one unmatched obs (with no 2012 ACS info)
drop if missing(GEOID_2020)

*Generating State County Tract BlockGroup numerics
gen State = substr(GEOID_2020, 1, 2)
destring State, replace
gen County = substr(GEOID_2020, 3, 3)
destring County, replace 
gen Tract = substr(GEOID_2020, 6, 6)
destring Tract, replace 
gen BlockGroup = substr(GEOID_2020, 12, 1)
destring BlockGroup, replace 
drop GEOID_2020

save "DataV2/US_Data/Output/LocalAbilityDistributions_historical.dta", replace
