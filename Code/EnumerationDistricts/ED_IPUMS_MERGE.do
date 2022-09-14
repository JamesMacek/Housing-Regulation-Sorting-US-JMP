cap cd "Z:/Dropbox/Schoolfolder/Projects/Zoning"
*Output: "Data/US_Data/Historical/Merged_IPUMS_ED_cleaned.dta"

*This file merges data constructed by ConstructEnumerationDistrictPopulation.R, and requires output from that program

*Date created: June 21st, 2022
*Date edited: June 21st, 2022

use "Data/US_Data/Historical/1930HH_IPUMS.dta", clear

*Dropping if not in a metropolitan area
drop if metarea == 0

*Decoding value labels of metarea (Cannot do this in R)
decode metarea, generate(metarea_name)

*Dropping duplicate observations from within a household (all data does not vary within household)
duplicates drop serial, force

*CLEANING RENT VARIABLE
replace rent30=. if rent30 >= 9998 | rent30 == 0

*rent available usually whenever house values are not-- depends on whether owner occupied or not. 

*CLEANING HOUSE VALUE VARIABLE
replace valueh =. if valueh == 0 | valueh >= 999998

collapse (median) valueh rent30 [pw = hhwt], by(metarea metarea_name)


*Fuzzy matching the rent data with Enumeration_Dist_HH.dta
*requires "matchit" package

*Save this into a temp file
save "Data/US_Data/Historical/temp.dta", replace 


*Importing enumeration district data from .R code output
use "Data/US_Data/Historical/Enumeration_Dist_HH.dta", clear
sort City

gen ED_rowID = _n

save "Data/US_Data/Historical/temp2.dta", replace 

*Running fuzzy matchit on metro names, as they are not consistent across datasets
matchit ED_rowID City using "Data/US_Data/Historical/temp.dta", idusing(metarea) txtusing(metarea_name) override threshold(0.2)

*Cleaning up individual matches
*NOTE: cities from Brown project tend to be contained in metro areas from IPUMS

*Springfield MA needs to be done by hand since matching doesn't work well
gen springfield_temp = 0
replace springfield_temp = 0.5 if City == "SpringfieldMA"
replace springfield_temp = 1 if metarea_name == "springfield-holyoke-chicopee, ma" & City == "SpringfieldMA"



*Taking max similarity score
bysort City: egen max_similscore = max(similscore)
*need to round similscore in order for comparison to be made below
replace similscore = round(similscore, 0.0001)
replace max_similscore = round(max_similscore, 0.0001)

*to machine precision check if max_similscore == similscore
keep if round(max_similscore - similscore, 0.01) == 0 | springfield_temp == 1
drop if springfield_temp == 0.5

*Bad match to YonkersNY
drop if City == "YonkersNY"

*Roughly 47 cities matched from the Brown Historical Transition Project data. 
*Note: some cities were merged, including Oakland and San francisco to a joined metro. 

drop similscore max_similscore springfield_temp 

*Labels

rename metarea_name IPUMS_Metro

label var IPUMS_Metro "Metro names from IPUMS 1930 sample"
label var metarea "Metro code from IPUMS 1930 sample"

*Joining back with enumeration district datasets
merge 1:1 ED_rowID using "Data/US_Data/Historical/temp2.dta"
drop _merge

*merge Jersey City/Newark/Yonkers with New York MSA
replace IPUMS_Metro = "new york, ny-northeastern nj" if City == "JerseyCityNJ" | City == "NewarkNJ" | City == "YonkersNY"
replace metarea = 560 if City == "JerseyCityNJ" | City == "NewarkNJ" | City == "YonkersNY"

*Merge St Paul Minnesota with Minneapolis
replace IPUMS_Metro = "minneapolis-st. paul, mn" if City == "StPaulMN" 
replace metarea = 512 if City == "StPaulMN"

*and then merge IPUMS collapsed dataset
merge m:1 metarea using "Data/US_Data/Historical/temp.dta"

*Dropping metros that don't show up in Brown project
drop if _merge == 2
drop _merge metarea_name

*Final labels
rename City Brown_City
label var Brown_City "City names from the Urban Transition Historical GIS Project"

*Finally, saving dataset for analysis.
save "Data/US_Data/Historical/Merged_IPUMS_ED_cleaned.dta", replace

*Erasing temp files
erase "Data/US_Data/Historical/temp.dta"
erase "Data/US_Data/Historical/temp2.dta"