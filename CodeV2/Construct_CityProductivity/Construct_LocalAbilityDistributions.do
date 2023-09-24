*This file constructed ability distributions by block group for pooled, college and noncollege workers. 
*Requires IncomeDist_ByCollegeShare and Construct_CityWages.do
*Requires crosswalk output.
*Also gets some other variables that will be useful later.

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
rename blck_grpa BlockGroup

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
local skillGroups "College NoCollege"
local skillGroupsSize: word count `skillGroups'

forval i = 1/7 {
	gen income_bin_NoCollege_share`i' = 1 - income_bin_College_share`i' 
}

forval i = 1/7 {
	forval s = 1/`skillGroupsSize' {
	*College + NoCollege (via Bayes Rule)
	
	local skillGroup: word `s' of `skillGroups'
	gen `skillGroup'_Ability_bin_`i' = Pooled_Ability_bin_`i'*income_bin_`skillGroup'_share`i'
	
	
	}
}

*generating implied college share in each neighborhood
forval s = 1/`skillGroupsSize' {
	local skillGroup: word `s' of `skillGroups'
	egen implied_`skillGroup'_share = rowtotal(`skillGroup'_Ability_bin_*)
}

*Each sums to 1.


*Lastly, generating shares of households in regulated structures for use with other programs
gen regulated_housingUnit_share = (amu5e003 + amu5e004 + amu5e005 + amu5e006 + amu5e014 +  amu5e015 + amu5e016 + amu5e017)/(amu5e001)


*Selecting variables we need + saving
keep State County Tract BlockGroup CBSA *_Ability_bin_* implied_*_share total_housing_units_cen regulated_housingUnit_share

save "DataV2/US_Data/Output/LocalAbilityDistributions.dta", replace