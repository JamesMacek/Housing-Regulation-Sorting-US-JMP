*Cleaning NHGIS data (block groups)
cd "z:/Dropbox/SchoolFolder/Projects/Zoning/Data/US_data/NHGIS/BlockGroup"



*importing raw IPUMS extract
import delimited "nhgis_blkgrp20082012.csv", clear

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
*Income distributions for each block group (censored)
*Educational attainment (college/nocollege)



keep statea countya tracta blkgrpa qvbe* qt*e* qu1e* qu2e* qu3e* qx6e* qx7e* qx8e* qx9e* qyae* ///
qyje* qyme* qyne* qyre* qyue* qyse* qyye* qyze* qy0e* qy1e* qy3e* qy2e* qy8e* qy9e* qzse* qzxe* qzze* qz2e* qz4e* qz6e* qz8e* ///
qzve* qzhe* qzie* qzee* qzfe* qz1e* qy7e* qzte* qz0e* quse* qu0e* qz3* qthe*

rename statea State
rename countya County
rename tracta Tract
rename blkgrpa BlockGroup

*Saving data
save "nhgis_blkgrp.dta", replace 
