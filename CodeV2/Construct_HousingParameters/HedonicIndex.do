*This file constructs the hedonic indices used in the counterfactual exercises. 
*Requires master assessments and transactions.
*Also creates a file with historical hedonic indices.
cd $proj_filepath

*Start loop over samples
foreach sampl in "current" "historical" {
	
	if "`sampl'" == "current" {
		local sampleName  ""
	}
	
	if "`sampl'" == "historical" {
		local sampleName  "_historical"
	}
	
	*clearing all memory
	clear
	clear matrix
	clear mata	
	set maxvar 120000

	*Importing transactions
	use "DataV2/CoreLogic/output/transactions_master`sampleName'.dta", clear

	*Merging transactions with properties.

	*This matching requires a lot of ram. 
	joinby clip using "DataV2/CoreLogic/output/currentAssess_master.dta", unmatched(none)
	
	*Requring YearBuilt 
	drop if yearbuilt == .
	
	*Dropping redeveloped properties if historical sample
	if "`sampl'" == "historical" {
		drop if yearbuilt  > 2012
	}
	
	
	*______________START CONSTRUCTING VARIABLES FOR HEDONIC REGRESSION_______________-
	
	*Dropping nonresidential indicators + mobile home
	*only 4000 observations
	drop if propertyindicatorcode >= 24 & propertyindicatorcode <= 80
	*370,000 observations
	drop if 135 <= landusecode & landusecode <= 137


	*Generating "missing" categories (code -996) for all code variables in the assessments.
	*Converting some variables to value labels
	foreach var of varlist _all {
			if strpos("`var'", "code") > 0 & "`var'" != "jurisdictioncountycode" & "`var'" != "towncode" & "`var'" != "zoningcode" & "`var'" != "Geocoded_municipality"  {
		
			replace `var' = 909090 if missing(`var')
		
		}
	}
	*Identifying regulated structures
	*Keeping if single family homes identified
	gen regulatedStructure = 0
	replace regulatedStructure = 1 if propertyindicatorcode == 10 | landusecode == 163 

	*Duplexes 
	replace regulatedStructure = 1 if propertyindicatorcode == 21 | landusecode == 115 

	*NOTE: TRIPLEXES AND FOURPLEXES GET LUMPED INTO PROPERTYINDICATORCODE 21.
	*We need to correct for this. 

	*Triplexes and quadriplexes (none directly identified in the data)
	replace regulatedStructure = 1 if landusecode == 165 | landusecode == 151

	*If property type identified off number of units
	replace regulatedStructure = 1 if numberofunits <= 4 & missing(numberofunits) == 0 


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

	*windsorizing saleamounts by CBSA at 1.5 percent and 98.5 percent level. Some transactions are clearly not market transactions, etc
	bysort CBSA: egen CBSA_low_saleamount = pctile(saleamount), p(1.5) 
	bysort CBSA: egen CBSA_high_saleamount = pctile(saleamount), p(98.5)

	replace saleamount = CBSA_low_saleamount if saleamount < CBSA_low_saleamount 
	replace saleamount = CBSA_high_saleamount if saleamount > CBSA_high_saleamount 
	
	drop CBSA_*_saleamount


	*log sales after conversion into rents using aggregate price to rent ratio
	gen log_saleamount_total = log(saleamount)
	gen log_saleamount_regulated = log_saleamount_total if regulatedStructure == 1

	*Rounding up all variables for the hedonic regression 
	*Creating one block group code x structure type code for xtset
	egen geoid = group(State County Tract BlockGroup)
	xtset geoid

	*dropping other variables with "code" in name that we will not use for hedonic regression
	drop jurisdictioncountycode towncode zoningcode Geocoded_municipality

	*Fixed effects (categorical + integer) for select informative variables (if we include too much then the block group fixed effects are probably all error!)
	local factor_vars i.yearSold i.monthSold i.airconditioningcode i.foundationtypecode i.heatingtypecode i.electricitywiringcode i.sewercode i.watercode i.landusecode

	*Generating frontage variable (open space)
	gen openspace = acres/livingsquarefeetallbuildings

	local vars yearbuilt openspace livingsquarefeetallbuildings bedrooms m_bedrooms rooms m_rooms bathrooms m_bathrooms bathfixtures m_bathfixtures storiesnumber m_storiesnumber fireplaceindicator poolindicator

	*investorpurchase is collinear with other variables...
	local sales_indicators i.multiorsplitparcelcode cashpurchaseindicator resaleindicator foreclosurereoindicator

	*Running hedonic regression (takes a very long time to run)
	xtreg log_saleamount_total `factor_vars' `vars' `sales_indicators' if regulatedStructure == 1, fe robust
	predict hedonicPrice_regulated, u
		
	xtreg log_saleamount_total `factor_vars' `vars' `sales_indicators', fe robust
	predict hedonicPrice_total, u

	*Storing these into .dta files
	collapse (median) hedonicPrice* log_saleamount*, by(State County Tract BlockGroup CBSA)
	
	
	*TESTING ISSUES DEC 3
	*further windsorizing of hedonic indices (there is still quite a bit of extreme variation in the tails that needs correcting, due to outliers on transactions)
	bysort CBSA: egen CBSA_low_hprice = pctile(hedonicPrice_regulated), p(2.5) 
	bysort CBSA: egen CBSA_high_hprice = pctile(hedonicPrice_regulated), p(97.5)
	
	replace hedonicPrice_regulated = CBSA_low_hprice if hedonicPrice_regulated < CBSA_low_hprice 
	replace hedonicPrice_regulated = CBSA_high_hprice if hedonicPrice_regulated > CBSA_high_hprice 
	
	drop CBSA_*_hprice
	
	
	bysort CBSA: egen CBSA_low_hprice = pctile(hedonicPrice_total), p(2.5) 
	bysort CBSA: egen CBSA_high_hprice = pctile(hedonicPrice_total), p(97.5)
	
	replace hedonicPrice_total = CBSA_low_hprice if hedonicPrice_total < CBSA_low_hprice 
	replace hedonicPrice_total = CBSA_high_hprice if hedonicPrice_total > CBSA_high_hprice 
	drop CBSA_*_hprice

	
	
	*sorting data for looks
	sort CBSA State County Tract BlockGroup

	*Correcting for log-linearity
	replace hedonicPrice_regulated = exp(hedonicPrice_regulated)
	replace hedonicPrice_total = exp(hedonicPrice_total)	
	
	*IMPORTANT: SUMMARIZING HEDONIC INDEX -- reasonable values
	sum hedonicPrice_regulated, detail
	sum hedonicPrice_total, detail


	save "DataV2/US_Data/Output/CoreLogicHedonicIndex`sampleName'.dta", replace
	clear 
	clear matrix
	clear mata

}