
*This file takes the master currentAssessments file and collpses certain key statistics by 
*block group. This will be used in the clustering algorithm to construct "zoning districts". 

*Date created: December 2nd, 2022

*Setting filepaths
cd $proj_filepath

cd "DataV2/CoreLogic"

*Importing specific columns from master current assessment file
use clip municipalityname Geocoded_municipality zoningcode landusecode propertyindicatorcode numberofunits acres landsquarefootage State County Tract BlockGroup CBSA using "output/currentAssess_master.dta", clear

*Taking properties that have nonmissing lot sizes
drop if acres ==. & landsquarefootage==.
*measuring all in acres
replace acres = landsquarefootage/43560 if acres == . & landsquarefootage != .

*86 million assessments remain. 

*Taking properties that are located in regulated structures.
*Here, we define regulated structures to mean single family homes, duplexes, triplexes and fourplexes. We ignore condominiums because data to infer number of housing units on the property is very sparse. 

*Single family homes correspond to propertyindicatorcode == 10 or landusecode == 163.
*Duplexes correspond to propertyindicatorcode == 21 or landusecode == 115
*Triplexes are landusecode == 165 and quadraplexes are landusecode == 151

*temporary keep indicator to extract as many housing units as possible
gen keepindicator=0
gen nonresidentialindicator=0
gen singlefamilyindicator = 0
gen duplexindicator = 0
gen triplexindicator = 0
gen quadriplexindicator = 0
gen vacantindicator = 0

*Keeping if single family homes identified
replace keepindicator = 1 if propertyindicatorcode == 10 | landusecode == 163 
replace singlefamilyindicator = 1 if propertyindicatorcode == 10 | landusecode == 163 | numberofunits == 1

*Duplexes 
replace keepindicator = 1 if propertyindicatorcode == 21 | landusecode == 115 
replace duplexindicator = 1 if landusecode == 115 | numberofunits == 2

*NOTE: TRIPLEXES AND FOURPLEXES GET LUMPED INTO PROPERTYINDICATORCODE 21.
*We need to correct for this. 

*Triplexes and quadriplexes (none directly identified in the data)
replace keepindicator = 1 if landusecode == 165 | landusecode == 151
replace triplexindicator = 1 if landusecode == 165 | numberofunits == 3
replace quadriplexindicator = 1 if landusecode == 151 | numberofunits == 4

*If property type identified off number of units
replace keepindicator = 1 if numberofunits <= 4 & missing(numberofunits) == 0 

*Additional cleaning.
*Drop if mobile homes from the dataset (so they don't influence the analysis)
drop if 135 <= landusecode & landusecode <= 137

*Creating vacant land and commercial indicators
*Very few commercial properties. Note, we have the residential dataset. 
replace nonresidentialindicator = 1 if propertyindicatorcode >= 24 & propertyindicatorcode <= 80

*gen otherstructures indicator
gen otherStructures_indicator = 0
replace otherStructures_indicator = 1 if keepindicator != 1

*NOTE: VACANT INDICATOR EMPTY IN DATA.
replace vacantindicator = 1 if propertyindicatorcode == 80

capture assert vacantindicator == 0
di _rc
if _rc == 0{
	drop vacantindicator
}



*PART 1.5: Cleaning up additional zoning codes on case-by-case basis: ___________________________________________________________________________

*appears to be an issue with zoning codes in Chicago. Setting these all to missing (they are labelled as 10, 50, etc; these do not correspond to typical codes, such as R-1.)
replace zoningcode = "" if Geocoded_municipality == "Chicago city"




*PART 2: Collapsing by key variables_____________________________________________________________

*WE WANT MEASURED DISTRIBUTIONS OF LOT SIZES FROM SF-QUADPLEX.

*WE WANT A MEASURE OF ZONING SIMILARITY BETWEEN BLOCK GROUPS (within a county?)
*This is based on ZoningCode

*WE WANT SHARES OF LAND USE IN SFR-QUAD, SHARES OF LAND USE IN VACANT AND NONRESIDENTIAL ACROSS BLOCK GROUPS.

*1: within a block group, assign the block group to a municipality and zoning code using the mode (of a string variable). (Note: some block groups cross municipalities, but municipalities are generally much larger)

*Not very many block groups with multiple municipality modes. Minmode would take observation with highest alphabetical order. Use only parcels with the "keep" indicator. 
bysort State County Tract BlockGroup: egen Assigned_municipality = mode(municipalityname), minmode

bysort State County Tract BlockGroup: egen Assigned_zoningcode = mode(zoningcode), minmode

bysort State County Tract BlockGroup: egen Assigned_geocoded_municipality = mode(Geocoded_municipality), minmode

*Replacing the assigned municipality with the county if cannot assign municipality officially.
gen temp1 = (10^3)*State + County
tostring temp, replace
replace Assigned_municipality = temp if missing(Assigned_municipality)
replace Assigned_geocoded_municipality = temp if missing(Assigned_geocoded_municipality)
drop temp

*Generating mode, median, p(25) and p(75) of lot size distributions for each structure type (single family and pooled)
local structureList "singlefamily keep"
local structureListSize : word count `structureList'

forval i = 1/`structureListSize' {
	local curStructure : word `i' of `structureList'
	cap gen `curStructure'_lotsize = .
	replace `curStructure'_lotsize = acres if `curStructure'indicator == 1
}

forval i = 1/`structureListSize' {
	local curStructure : word `i' of `structureList'
	
	bysort State County Tract BlockGroup: egen `curStructure'_statp50 = pctile(`curStructure'_lotsize), p(50)
	bysort State County Tract BlockGroup: egen `curStructure'_statp25 = pctile(`curStructure'_lotsize), p(25)
	bysort State County Tract BlockGroup: egen `curStructure'_statp10 = pctile(`curStructure'_lotsize), p(10)
	bysort State County Tract BlockGroup: egen `curStructure'_statmean = mean(`curStructure'_lotsize)
	bysort State County Tract BlockGroup: egen `curStructure'_statmode = mode(`curStructure'_lotsize), minmode
}

*Aggregated statistics

cap ssc install _gwtmean

*Parcel mass weighted shares in different structure groups
bysort State County Tract BlockGroup: egen nonres_share = wtmean(nonresidentialindicator), weight(acres)
*non-regulated structure land share in assessments
bysort State County Tract BlockGroup: egen otherStruct_share = wtmean(otherStructures_indicator), weight(acres)


forval i = 1/`structureListSize' {
	local curStructure : word `i' of `structureList'
	bysort State County Tract BlockGroup: egen `curStructure'_share = wtmean(`curStructure'indicator), weight(acres)

}


*Reminder: these shares are taken over all parcels in the datset so far, and these include condominiums. Condominiums are not assigned into a regulated structure category because I cannot construct how many housing units should be on the parcel in most cases.
*So these should not be interpreted as true land use shares. These are only used for clustering--to construct our zoning districts. 
*Moreover, we use propertindicatorcode == 10 designator, which has some issues in the data (anecdotally).

*Dropping variables/observations we now do not need for collapse
keep if keepindicator == 1
drop nonresidentialindicator
drop keepindicator

*Calculating lot size distributions over each structure type. p(25), p(50), p(75), mean.


*Collapsing rest of data
collapse (mean) *_share *_stat*, by(State County Tract BlockGroup CBSA Assigned_municipality Assigned_zoningcode Assigned_geocoded_municipality)

save "output/blkgrpstats_forClustering.dta", replace
*Note: we get data on 190,000 block groups. Assign to the other ~ 197,000 block groups no regulation (i.e. they do not belong to a zoning district)
