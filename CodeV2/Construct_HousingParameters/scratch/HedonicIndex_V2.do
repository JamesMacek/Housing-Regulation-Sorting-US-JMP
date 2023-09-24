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
drop if (propertyindicatorcode >= 24 & propertyindicatorcode <= 80) | propertyindicatorcode == 0

*Keep mobile homes for construction of indices
*370,000 observations
*drop if 135 <= landusecode & landusecode <= 137


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



*log sales
gen log_saleamount = log(saleamount)

*log floorspace
gen log_floorspace = log()

*Rounding up all variables for the hedonic regression 
*Creating one block group code x structure type code for xtset
egen geoid = group(State County Tract BlockGroup regulated_Structure)
xtset geoid

*Note: we do not want to include all variables in hedonic index. This brings noise to the MANY fixed effects in this model.

*dropping other variables with "code" in name that we will not use for hedonic regression
drop jurisdictioncountycode towncode zoningcode Geocoded_municipality

*Fixed effects (categorical + integer) for select informative variables (if we include too much then the block group fixed effects are probably all error!)
local factor_vars i.yearSold i.monthSold i.airconditioningcode i.foundationtypecode i.heatingtypecode i.electricitywiringcode i.sewercode i.watercode i.fuelcode

*NOTE: DO NOT CONTROL FOR LANDUSECODE! WE ARE IDENTIFYING ALREADY THROUGH REGULATED_STRUCTURE FIXED EFFECTS

*Generating frontage variable (open space)
gen openspace = acres/livingsquarefeetallbuildings

local vars yearbuilt openspace livingsquarefeetallbuildings bedrooms m_bedrooms rooms m_rooms bathrooms m_bathrooms bathfixtures m_bathfixtures storiesnumber m_storiesnumber fireplaceindicator poolindicator

*investorpurchase is collinear with other variables...
local sales_indicators i.multiorsplitparcelcode cashpurchaseindicator resaleindicator foreclosurereoindicator

*Running hedonic regression (takes a very long time to run)
xtreg log_saleamount `factor_vars' `vars' `sales_indicators', fe robust
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

*Many observations have different hedonicPrice
sum hedonicPrice0
sum hedonicPrice1

*Average hedonicPrice for regulated structures is much lower. Is this due to selection? (Probably!)

*Replacing missing data with CBSA average
gen missing_hedonicPrice1 = 1 if missing(hedonicPrice1)
gen missing_hedonicPrice0 = 1 if missing(hedonicPrice0)
gen missing_either_hedonicPrice = 1 if missing_hedonicPrice1 == 1| missing_hedonicPrice0 == 1

*CBSA averages in locations where both indices could be constructed
bysort CBSA: egen CBSA_med_hedonicPrice0 = median(hedonicPrice0) if missing(missing_either_hedonicPrice)
bysort CBSA: egen CBSA_med_hedonicPrice1 = median(hedonicPrice1) if missing(missing_either_hedonicPrice)

bysort CBSA: egen CBSA_med_hedonicPrice0_final = median(CBSA_med_hedonicPrice0) 
bysort CBSA: egen CBSA_med_hedonicPrice1_final = median(CBSA_med_hedonicPrice1)



*Constructing CBSA average RELATIVE index for imputation
gen CBSA_relative_hedonicPrice = CBSA_med_hedonicPrice0_final/CBSA_med_hedonicPrice1_final

*generating imputed flags + imputed where missing
gen imputed_price_flag1 = 0
gen imputed_price_flag0 = 0

replace imputed_price_flag1 = 1 if missing(hedonicPrice1)
replace imputed_price_flag0 = 1 if missing(hedonicPrice0)

*Imputing with CBSA relative shares
replace hedonicPrice0 = CBSA_relative_hedonicPrice*hedonicPrice1 if missing(hedonicPrice0) & missing(hedonicPrice1) == 0

replace hedonicPrice1 = hedonicPrice0/CBSA_relative_hedonicPrice if missing(hedonicPrice1) & missing(hedonicPrice0) == 0


*Any missings left over? (just missing both --fixed effect was not identified)
*Imputing the remaining
replace hedonicPrice0 = hedonicPrice1 if missing(hedonicPrice0)

*Average hedonic price1 is 0.7, average for 0 is 1.13.
*Non regulated structures much more expensive per unit of housing services, reflecting a regulatory gap. 
sum hedonicPrice1
sum hedonicPrice0

*Generating CBSA distributions for cleaning (if needed)
bysort CBSA: egen CBSA_plo_hedonicPrice0 = pctile(hedonicPrice0), p(1) 
bysort CBSA: egen CBSA_phi_hedonicPrice0 = pctile(hedonicPrice0), p(99) 
bysort CBSA: egen CBSA_plo_hedonicPrice1 = pctile(hedonicPrice1), p(1) 
bysort CBSA: egen CBSA_phi_hedonicPrice1 = pctile(hedonicPrice1), p(99) 

*Winsorizing outlier observations
replace hedonicPrice0 = CBSA_plo_hedonicPrice0 if hedonicPrice0 < CBSA_plo_hedonicPrice0
replace hedonicPrice0 = CBSA_phi_hedonicPrice0 if hedonicPrice0 > CBSA_phi_hedonicPrice0
replace hedonicPrice1 = CBSA_plo_hedonicPrice1 if hedonicPrice1 < CBSA_plo_hedonicPrice1
replace hedonicPrice1 = CBSA_phi_hedonicPrice1 if hedonicPrice1 > CBSA_phi_hedonicPrice1



*Dropping hedonicPrices if missing (find a way to deal with missings later)
drop if missing(hedonicPrice0) & missing(hedonicPrice1)

*Final sample of 180,000 block groups. (roughly)

*Interesting--we have hedonic prices for 180,000 block groups. 

save "DataV2/US_Data/Output/CoreLogicHedonicIndex_V2.dta", replace
clear 
clear matrix
clear mata