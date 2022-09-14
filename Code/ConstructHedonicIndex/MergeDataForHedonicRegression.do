cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*This do file merges current assessments data with zillow transactions data, performs sample splitting
*to run regressions because this stuff doesn't really fit on a 32GB machine. 


*Date created: June 23rd, 2022

*Import transactions data, only keep 4 variables
use ImportParcelID DocumentDate SalesPriceAmount SalesPriceAmountStndCode using "Data/ZillowData/transactions_sample/transactions_zillow_region0.dta", replace 

append using "Data/ZillowData/transactions_sample/transactions_zillow_sample.dta", keep(ImportParcelID DocumentDate SalesPriceAmount SalesPriceAmountStndCode)

*Dropping obs with no transactions data
drop if DocumentDate == "" | SalesPriceAmount ==. | SalesPriceAmount==0  | ImportParcelID == ""

*Parsing year and month of sale
gen YearSold = substr(DocumentDate, 1, 4)
destring YearSold, replace

*keeping only observations in sample period
drop if YearSold < 2008 | YearSold > 2012

gen MonthSold = substr(DocumentDate, 6, 2)
destring MonthSold, replace
drop DocumentDate

*Creating time ID
gen TimeSold = 100*YearSold + MonthSold


merge m:1 ImportParcelID using "Data/ZillowData/Output/CurrentAssess_matched.dta", keep(matched)
drop _merge


foreach var of varlist State County Tract BlockGroup CBSA {
	destring `var', replace
}


save "Data/ZillowData/Output/HousingTransactions.dta", replace 
	


/*
*Need to split up the data and append later
local generate_sample = 1
*Scalar detailing how many subsamples you need
local number_of_splits = 3


	
*Generating random sample
set seed 12345678
cap noisily splitsample, generate(split_sample) nsplit(`number_of_splits')
di _rc
	
	*error code
if _rc == 110 {
	drop reg_sample
	splitsample, generate(split_sample) nsplit(`number_of_splits')
}
	
save "Data/ZillowData/Output/HousingTransactions.dta", replace 
	
local number_of_splits = 3
*Importing split
forval i = 1/`number_of_splits' {
use if split_sample == `i' using "Data/ZillowData/Output/HousingTransactions.dta", clear

*Merging on ImportParcelID to assessment data and saving to .dta
merge m:1 ImportParcelID using "Data/ZillowData/Output/CurrentAssess_matched.dta", keep(matched)
save "Data/ZillowData/Output/HousingTransactions_merged`i'.dta", replace
	
}


