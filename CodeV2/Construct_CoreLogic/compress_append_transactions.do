*This do file takes the chunked .dta files from "transactions_construct.R",
*compresses them, appends them, and stores string columns in a much more 
*smaller format.

*Date created: December 9, 2022

*Change directory from global filepath folder
do "CodeV2/global_filepath.do"

cd $proj_filepath
cd "DataV2/CoreLogic"

*Appending all transactions files
cd "temp/dta/transactions"

local files_toAppend : dir . files "*transactions_tmpchunk.dta", respectcase

append using `files_toAppend'


*Converting some variables to value labels
foreach var of varlist _all {
	if strpos("`var'", "code") > 0 {
		
		cap encode `var', generate(temp_v) label(`var'_v)
		if _rc == 0 {
			drop `var'
			rename temp_v `var'
		}
		
	}
}


*Compressing data
compress

*Saving as master transactions file
cd $proj_filepath
cd "DataV2/CoreLogic"

save "output/transactions_master.dta", replace
clear


*Appending all historical transaction files
*Appending all transactions files
cd "temp/dta/transactions"

local files_toAppend : dir . files "*transactions_tmpchunk_hist.dta", respectcase

append using `files_toAppend'


*Converting some variables to value labels
foreach var of varlist _all {
	if strpos("`var'", "code") > 0 {
		
		cap encode `var', generate(temp_v) label(`var'_v)
		if _rc == 0 {
			drop `var'
			rename temp_v `var'
		}
		
	}
}


*Compressing data
compress

*Saving as master transactions file
cd $proj_filepath
cd "DataV2/CoreLogic"

save "output/transactions_master_historical.dta", replace
clear