*Cleaning NHGIS data 
cd "z:/Dropbox/SchoolFolder/Projects/Zoning/Data/US_data/NHGIS"

*1st file 
import delimited "nhgis191.csv", clear

foreach v of varlist _all {
	label var `v' ""
}

*What variables do we want? In that order...
*Household family structure by race
*Median household income by White/Black/Aggregate
*Housing units
*Tenure by black/white -- not terribly important 
*Race of householder
*Tenure by household size
*Average household size by tenure status
*Units by number of rooms
*Median number of rooms
*Units in structure
*Units in structure for occupied housing
*Year structure built (vintage)
*Median year structure built
*Units by number of bedrooms
*Kitchen Facilities 
*Plumbing facilities
*House Heating fuel
*Gross rent
*Median gross rent
*Median gross rent
*Bedrooms by Gross Rent
*Gross rent as a percentage of HH income in the past 12 months
*Median gross rent as a percentage of HH income in the past 12 months
*Aggregate Gross Rent By Units in Structure
*Units in structure by Gross Rent as a Percentage of HH income 
*Self reported value of owner occupied units
*Median value owner occupied
*Aggregate value by units in structure (DIVIDE BY units in structure)




keep statea countya tracta qvbe* qt*e* qu1e* qu2e* qu3e* qx6e* qx7e* qx8e* qx9e* qyae* ///
qyje* qyme* qyne* qyre* qyue* qyse* qyye* qyze* qy0e* qy1e* qy3e* qy2e* qy8e* qy9e* qzse* qzxe* qzze* qz2e* qz4e* qz6e* ///
qzve* qzhe* qzie* qzee* qzfe* qz1e* qy7e* qzte* qz0e* 

rename statea State
rename countya County
rename tracta Tract


*Saving data
save "nhgis_191.dta", replace 


*2nd file 
import delimited "nhgis192.csv", clear

foreach v of varlist _all {
	label var `v' ""
}

*Household type (family versus nonfamily) by Units in Structure 
*Units in Structure by race
*Tenure by Household Size by Units in Structure *Tells us join distribution for household size

keep statea countya tracta q7ie* rf*e* rgwe*

rename statea State
rename countya County
rename tracta Tract


save "nhgis_192.dta", replace 