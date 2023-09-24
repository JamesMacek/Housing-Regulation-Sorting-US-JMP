
*This do file takes the chunked .dta files from "currentAssess_construct.R",
*compresses them, appends them, and stores string columns in a much more 
*smaller format.

*Date created: December 1, 2022

*Change directory from global filepath folder
cd $proj_filepath
cd "DataV2/CoreLogic"

local files_toCompress : dir "temp/dta/currentAssessments" files "*currentAssess_tmpchunk.dta", respectcase

*PART 1: Importing each dataset, compressing and saving in new format to append later.
*Loop for fileno
local i = 0
foreach file in `files_toCompress' {
	local i = `i' + 1
	
	use "temp/dta/currentAssessments/`file'", clear
	
	*destringing some variables that we want numeric that 
	*for some reason get assigned string by the "haven" package in R.
	cap destring propertyindicatorcode, replace
	
	compress
	save "temp/dta/currentAssessments/`i'cA_tmpchunk_compressed.dta", replace
	
}

clear

*Appending these compressed files in one go
cd "temp/dta/currentAssessments"

local files_toAppend : dir . files "*cA_tmpchunk_compressed.dta", respectcase

append using `files_toAppend', force
*NOTE: FORCE OPTION ONLY COVERTS EMPTY STRINGS (WHICH WERE SAVED AS BYTE) TO STRING. HAS NO BINDING IMPACT ON THE DATA!

*Assigning value labels to `code' variables, removing variables that we don't need. Ignoring jurisdiction, zoning code or town strings. 

foreach var of varlist _all {
	if strpos("`var'", "code") > 0 & "`var'" != "jurisdictioncountycode" & "`var'" != "towncode" & "`var'" != "zoningcode" & "`var'" != "Geocoded_municipality"  {
		
		cap encode `var', generate(temp_v) label(`var'_v)
		if _rc == 0 {
			drop `var'
			rename temp_v `var'
		}
		
	}
}

cd $proj_filepath
cd "DataV2/CoreLogic"

*Compressing and saving
compress
save "output/currentAssess_master.dta", replace
clear
*Saving as our main property level dataset to use with other programs. 

*Final: Do some cleanup. 

*Deleting compressed data. 
cd "temp/dta/currentAssessments"
local files_toErase : dir . files "*cA_tmpchunk_compressed.dta", respectcase
foreach file in `files_toErase' {
	erase `file'
}

