
*This do file converts the raw CSV PUMA-MSA crosswalks into .dta files, so that we can assign as many observations as possible to an MSA to calculate productivity.

cd $proj_filepath

*2010 PUMA to 2013 MSA crosswalk from IPUMS.
import excel using "DataV2/US_Data/ACS_Individual/MSA2013_PUMA2010_crosswalk.xls", firstrow clear

*dropping PR
drop if StateFIPSCode == "72"

*Destringing to avoid differences in puma codes
destring MSACode, replace
destring PUMACode, replace
destring StateFIPSCode, replace

*matching puma to its largest MSA intersection
bysort StateFIPSCode PUMACode: egen max_int = max(PercentPUMAPopulation)

*Rounding each number to generate matches
replace max_int = round(max_int, 0.01)
replace PercentPUMAPopulation = round(PercentPUMAPopulation, 0.01)

*This dropping procedure doesn't work for Corvalis, Oregon (18700) or Midland, Michigan (33220) + Weirton-Steubenville (48260)
* PUMAs that overlap with these MSAs also have greater or equal overlap in other MSAs.

drop if abs(max_int - PercentPUMAPopulation) > 0.001 

keep MSACode StateFIPSCode PUMACode

rename MSACode met2013_from_crosswalk_2010
rename StateFIPSCode statefip
rename PUMACode puma

*Saving crosswalk
save "DataV2/US_Data/ACS_Individual/MSA2013_PUMA2010_crosswalk.dta", replace

