*This file constructs the hedonic indices used in the counterfactual exercises. Requires master assessments and transactions, as well as a file with the unit density restriction from Merge_Stringency.R

*This file splits hedonic indices into "regulated" and "unregulated" structures.

*To increase coverage, we include all homes below the measured minimum lot size as "unregulated"-- but not necessarily in the model. 

cd $proj_filepath
clear
clear matrix
clear mata
set maxvar 120000



*Importing transactions
use "DataV2/CoreLogic/output/transactions_master.dta", clear
*Merging transactions with properties. These transactions are a snapshot for the years 2016-2022 (can exclude covid years)


*This matching requires a lot of ram. 
merge m:m clip using "DataV2/CoreLogic/output/currentAssess_master.dta", keep(master matched)
drop if _merge == 1
drop _merge
*5 million transactions in sample not matched to assessment on clip. This makes sense, we limit our assessment sample to be within the sample geography.

merge m:1 State County Tract BlockGroup using "DataV2/US_Data/Output/raw_Regulation_ByBlockGroup.dta"
drop _merge


*Requring YearBuilt 
drop if yearbuilt == .

*Dropping nonresidential indicators + mobile home + miscellaneous land
*only 4000 observations
drop if (propertyindicatorcode >= 23 & propertyindicatorcode <= 80) | propertyindicatorcode == 0 | propertyindicatorcode == 20

*Keep mobile homes for construction of indices
*370,000 observations
drop if 135 <= landusecode & landusecode <= 137


*Generating "missing" categories (code -996) for all code variables in the assessments.
*Converting some variables to value labels
foreach var of varlist _all {
		if strpos("`var'", "code") > 0 & "`var'" != "jurisdictioncountycode" & "`var'" != "towncode" & "`var'" != "zoningcode" & "`var'" != "Geocoded_municipality"  {
		
		replace `var' = 909090 if missing(`var')
		
	}
}


*GENERATING REGULATED STRUCTURE LABELS: ALL STRUCTURES BELOW MINIMUM LOT SIZE__________
*temporary keep indicator to extract as many housing units as possible
gen regulated_Structure = 0
gen singlefamilyindicator = 0
gen duplexindicator = 0
gen triplexindicator = 0
gen quadriplexindicator = 0

*Keeping if single family homes identified
replace regulated_Structure = 1 if propertyindicatorcode == 10 | landusecode == 163 
replace singlefamilyindicator = 1 if propertyindicatorcode == 10 | landusecode == 163 | numberofunits == 1


*Duplexes 
replace regulated_Structure = 1 if propertyindicatorcode == 21 | landusecode == 115 
replace duplexindicator = 1 if landusecode == 115 | numberofunits == 2

*NOTE: TRIPLEXES AND FOURPLEXES GET LUMPED INTO PROPERTYINDICATORCODE 21.
*We need to correct for this by using landusecode. 

*Triplexes and quadriplexes (none directly identified in the data)
replace regulated_Structure = 1 if landusecode == 165 | landusecode == 151
replace triplexindicator = 1 if landusecode == 165 | numberofunits == 3
replace quadriplexindicator = 1 if landusecode == 151 | numberofunits == 4

*If property type identified off number of units

replace regulated_Structure = 1 if numberofunits <= 4 & missing(numberofunits) == 0 

local structureList "singlefamily duplex triplex quadriplex"
local structureListSize : word count `structureList'

*Dividing lot sizes by implied unit density restriction
forval i = 1/`structureListSize' {
	local curStructure: word `i' of `structureList'
	replace acres = acres/`i' if `curStructure'indicator == 1 
}


*_________________________________________________________________________________________


*Additional assigning missing code to other non categorical variables. 
*We set missing to 0 and allow for a set of "missing fixed effects" to compensate for this imputation. 

*renaming those with long variable names
rename totalnumberofbathfixturesallbuil bathfixtures
rename bedroomsallbuildings bedrooms
rename totalroomsallbuildings rooms
rename totalbathroomsallbuildings bathrooms

foreach var of varlist bathfixtures bedrooms bathrooms rooms numberofparkingspaces storiesnumber {
    
	gen m_`var' = 0
	replace m_`var' = 1 if missing(`var')
	replace `var' = 0 if missing(`var')

	
}
replace numberoffireplaces = 0 if fireplaceindicator == 0


*generating floorspace measure per building on parcel
gen floorspace = livingsquarefeetallbuildings/numberofbuildings
drop if floorspace < 100

*___________________CLEANING______________________________________________________________

*FIRST: MATCHING TRANSACTIONS WITH MULTIPLE ROWS TO 1 TRANSACTION. ROWS ARE IN SAME TRANSACTION IF THEY ARE IDENTICAL ON SELLING VARIABLES WITHIN A COUNTY. (This is not necessarily the best way of doing things)

*NOTE: multiorsplitparcelcode == 1 or 2 means multiple parcels in sale--only need to check these.
egen inf_transactionID = group(fipscode_fromSale yearSold monthSold daySold saleamount cashpurchaseindicator investorpurchaseindicator resaleindicator) if multiorsplitparcelcode == 1 | multiorsplitparcelcode == 2 

*Generating number of transactions in sample
bysort inf_transactionID: egen Parcel_transaction_count = count(inf_transactionID)

*if one parcel in sample (but supposedly a multiparcel transaction) set to missing
replace inf_transactionID = . if Parcel_transaction_count == 1
replace Parcel_transaction_count = 0 if Parcel_transaction_count == 1

*Generating sale amount per parcel
gen saleamount_perparcel = saleamount
replace saleamount_perparcel = saleamount/Parcel_transaction_count if Parcel_transaction_count >= 2

*Take price divided by total floorspace
gen price_per_unit_floorspace = saleamount_perparcel/floorspace

*Drop if price per square foot exceeds 10000 -- clearly something wrong with this transaction. 10,000/sqft is the most expensive housing in Manhattan. 
*309,000 obs dropped
drop if price_per_unit_floorspace > 10000 | price_per_unit_floorspace < 2.5

sort price_per_unit_floorspace
sum price_per_unit_floorspace, detail
*median alligns almost exactly with US average, $150/sqft


*NEXT: INFERRING WHETHER FLOORSPACE MEASURES CORRESPOND TO UNIT IN MULTIFAMILY BUILDING OR ENTIRE BUILDING, AT ODDS WITH SALE OF PROPERTY.
*THIS WOULD MAKE THE PRICE PER UNIT OF FLOORSPACE SUPER LOW, AND LARGE MULTIFAMILY CONDOS SUPER INEXPENSIVE FROM A HEDONIC PERSPECTIVE
*Now, looking at the OTHER END -- units with very low transaciton values per square foot

*Indicator if unregulated condominium
gen condo_unregulated = 1 if (propertyindicatorcode == 11 | landusecode == 112) & regulated_Structure == 0

*Another way--identify assessments in the same building (checking to see if building has same land in acres)
egen parcelID = group(State County Tract BlockGroup parcellevellatitude parcellevellongitude acres) if regulated_Structure == 0


*STARTING HEDONIC REGRESSION
*log sales per identified parcel
gen log_salesprice = log(saleamount_perparcel)

*Xtset dummy variables 
egen geoid = group(State County Tract BlockGroup regulated_Structure)
xtset geoid

*log floorspace
gen log_floorspace = log(floorspace)
gen log_floorspace_to_lot_ratio = log(acres/floorspace)


*Note: these are not perfect controls!
local vars yearbuilt bedrooms m_bedrooms rooms m_rooms bathrooms m_bathrooms bathfixtures m_bathfixtures storiesnumber m_storiesnumber fireplaceindicator poolindicator

*Local variable groups for hedonic regression
local sales_indicators i.multiorsplitparcelcode cashpurchaseindicator resaleindicator foreclosurereoindicator i.regulatedStructure
local factor_vars i.yearSold i.monthSold 

*Checking coefficient on floorspace
xtreg log_salesprice log_floorspace log_floorspace_to_lot_ratio `sales_indicators' `factor_vars' `vars',  fe robust 

predict hedonicPrice, u

*Storing these into .dta files
collapse (mean) hedonicPrice, by(State County Tract BlockGroup CBSA regulated_Structure)

sort CBSA State County Tract BlockGroup regulated_Structure

*Correcting for log-linearity
replace hedonicPrice = exp(hedonicPrice)

*Setting average == 1 to normalize scale of prices  (neighborhoods weighted equally within block groups.) This is inconsequential but bounds scale of prices.
egen mean_hedonicPrice = mean(hedonicPrice)
replace hedonicPrice = hedonicPrice/mean_hedonicPrice
drop mean_hedonicPrice


*Reshaping dataset
reshape wide hedonicPrice, i(State County Tract BlockGroup) j(regulated_Structure)

*Replacing missing data with CBSA average
gen missing_hedonicPrice1 = 1 if missing(hedonicPrice1)
gen missing_hedonicPrice0 = 1 if missing(hedonicPrice0)
gen missing_either_hedonicPrice = 1 if missing_hedonicPrice1 == 1| missing_hedonicPrice0 == 1

*CBSA averages in locations where both indices could be constructed
bysort CBSA: egen CBSA_med_hedonicPrice0 = median(hedonicPrice0) if missing(missing_either_hedonicPrice)
bysort CBSA: egen CBSA_med_hedonicPrice1 = median(hedonicPrice1) if missing(missing_either_hedonicPrice)

bysort CBSA: egen CBSA_med_hedonicPrice0_final = median(CBSA_med_hedonicPrice0) 
bysort CBSA: egen CBSA_med_hedonicPrice1_final = median(CBSA_med_hedonicPrice1)


sum hedonicPrice0
sum hedonicPrice1

