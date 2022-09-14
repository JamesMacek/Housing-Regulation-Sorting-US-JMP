cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*This do file performs a crosswalk from the 2000 tract geographies to the 2010 tract geographies.
*Allows for the use of microgeographic variation in housing supplies. 

*

*Importing Brown's 2000-2010 crosswalk

use "Data/US_Data/BaumSnowHan_elasticities/2000-2010TractCrosswalk/crosswalk_2000_2010.dta", clear

rename trtid00 ctracts2000
rename trtid10 ctracts2010

merge m:m ctracts2000 using "Data/US_Data/BaumSnowHan_elasticities/gammas_hat_all.dta", keep(match)

*Taking weighted average of ctracts2000 elasticities by ctracts2010. 

bysort ctracts2010: egen converted_elasticities = wtmean(gamma01b_space_FMM), weight(weight)
collapse (mean) converted_elasticities, by(ctracts2010)
*57,000 2010 tracts for which we could assign elasticities. This may be pretty close to the near universe. 

gen State = substr(ctracts2010, 1, 2)
gen County = substr(ctracts2010, 3, 3)
gen Tract = substr(ctracts2010, 6, 6)

foreach var of varlist State County Tract {
	destring `var', replace
}

drop ctracts2010

save "Data/Counterfactuals/Elasticities.dta", replace