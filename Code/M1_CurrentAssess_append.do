cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"


*Date created: April 27th 2022
*Date edited: June 17th, 2022

*Appends outputted geography-matched assessments, saves and deletes other files.
*Requires output from M1_CurrentAssess_Match_Tracts.R!
*Use stata because it is (a lot) faster. 

use "Data/ZillowData/Output/Region0_matched.dta", clear

forval i=1/20 {
	append using "Data/ZillowData/Output/CurrentAssess_matched`i'.dta"
	
}

*Imputes missing lot size (in acres) measurement.
replace LotSizeAcres = LotSizeSquareFeet/43650 if LotSizeAcres == .

*Saving
save "Data/ZillowData/Output/CurrentAssess_matched.dta", replace

*Selecting from other sets of variables to make a smaller dataset for use with other programs


