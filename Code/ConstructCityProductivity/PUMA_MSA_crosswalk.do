cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*This do file converts the raw CSV PUMA-MSA crosswalks into .dta files


*2010 PUMA to 2013 MSA
import excel using "Data/US_Data/ACSIndividual/MSA2013_PUMA2010_crosswalk.xls", firstrow clear

*dropping PR
drop if StateFIPSCode == "72"

*Destringing to avoid differences in puma codes
destring MSACode, replace
destring PUMACode, replace
destring StateFIPSCode, replace

*matching puma to its largest MSA intersection
bysort StateFIPSCode PUMACode: egen max_int = max(PercentPUMAPopulation)

replace max_int = round(max_int, 0.01)
replace PercentPUMAPopulation = round(PercentPUMAPopulation, 0.01)
*This dropping procedure doesn't work for Corvalis, Oregon or Midland, Michigan, so these MSAs are dropped from the sample

drop if abs(max_int - PercentPUMAPopulation) > 0.001 

keep MSACode StateFIPSCode PUMACode



rename MSACode met2013_from_crosswalk_2010
rename StateFIPSCode statefip
rename PUMACode puma

drop if missing(met2013) == 1

save "Data/US_Data/ACSIndividual/MSA2013_PUMA2010_crosswalk.dta", replace

*2000 PUMA to 2013 MSA
import excel using "Data/US_Data/ACSIndividual/MSA2013_PUMA2000_pop2000_crosswalk.xls", firstrow clear

*dropping PR
drop if StateFIPSCode == "72"

*destringing
destring MSACode, replace
destring PUMACode, replace
destring StateFIPSCode, replace

*matching puma to its largest MSA intersection
bysort StateFIPSCode PUMACode: egen max_int = max(PercentPUMAPopulation)

replace max_int = round(max_int, 0.01)
replace PercentPUMAPopulation = round(PercentPUMAPopulation, 0.01)
*This dropping procedure doesn't work for Corvalis, Oregon or Midland, Michigan, so these MSAs are dropped from the sample

drop if abs(max_int - PercentPUMAPopulation) > 0.001

keep MSACode StateFIPSCode PUMACode

rename MSACode met2013_from_crosswalk_2000
rename StateFIPSCode statefip
rename PUMACode puma

drop if missing(met2013) == 1

save "Data/US_Data/ACSIndividual/MSA2013_PUMA2000_crosswalk.dta", replace