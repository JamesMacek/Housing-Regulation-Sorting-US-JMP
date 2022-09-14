cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*Construct the hedonic index for each block group
*requires output from M1_currentassess_append.do, MergeDataForHedonicRegression.do
*and requires simpleMode_constructMinLotSize.do

use "Data/ZillowData/Output/HousingTransactions.dta", replace

*importing data on minimum lot size
merge m:1 State County Tract BlockGroup using "Data/ZillowData/Output/LotSize_Master.dta"
drop _merge

*Importing data on bottom 1% of house values
merge m:1 CBSA using "Data/ZillowData/Output/pct_houseval_ACS.dta", keep(matched)
drop _merge

*Sorting by CBSA
sort CBSA ImportParcelID

*Dropping buildings sold long ago (try and aim for a ten year window 2005-2015 centered at 2010)
drop if missing(YearBuilt)

*First, removing observations associated with non-residential land use
keep if strpos(PropertyLandUseStndCode, "RR") == 1 | strpos(PropertyLandUseStndCode, "RI") == 1 
*Can't determine this one
drop if PropertyLandUseStndCode == "RR000"

*Dropping transactions in bot and top 1/2 percentile by CBSA -- these are probably wealth transfers. 
drop if SalesPriceAmount < bottom_pct
drop if SalesPriceAmount > top_pct
*NOTE: MORE THAN 1% of TRANSACTIONS ARE DROPPED -- probably about 2%. by and large most transactions dropped are on the low end. 

*Drop condominimums that do not have information on number of units 
drop if PropertyLandUseStndCode == "RR106" & NoOfUnits == 0

*Cleaning data for use with index
replace FireplaceNumber = 0 if missing(FireplaceNumber)
destring NoOfStories, replace

*If remodeling data missing, set remodel = built
replace YearRemodeled = YearBuilt if missing(YearRemodeled)


*Replace missing garage data to zero
replace GarageAreaSqFt = 0 if missing(GarageAreaSqFt)


*Dropping buildings/parcels for which we cannot determine number of units
drop if NoOfUnits == 0 & PropertyLandUseStndCode != "RI101" & PropertyLandUseStndCode != "RI102" & PropertyLandUseStndCode != "RI103" & strpos(PropertyLandUseStndCode, "RR") != 1 

replace maxareasqft = maxareasqft/2 if DuplexDummy == 1
replace maxareasqft = maxareasqft/3 if TriplexDummy == 1
replace maxareasqft = maxareasqft/4 if FourplexDummy == 1

*Encoding categorical variables
foreach var of varlist BuildingQualityStndCode PropertyLandUseStndCode RoofCoverStndCode RoofStructureTypeStndCode HeatingTypeorSystemStndCode ACTypeorSystemStndCode FoundationTypeStndCode WaterStndCode SewerStndCode TypeConstructionStndCode {
	
	replace `var' = "Missing" if missing(`var')
	cap rename `var' `var'_e
	cap encode `var'_e, gen(`var') label(`var')
	
}
*Destringing GEOID
destring GEOID10, replace

*Local variables
*Generating log prices
gen log_price = log(SalesPriceAmount)



*Some variables appear to be very imprecisely estimated and running into computational issues.
*I.e. FoundationType, RoofStructureType, RoofCoverStndCode, TypeConstructionStndCode, BuildingQualityStndCode are all leading to an extreme loss of precision because very no

*Categorical
local vars i.PropertyLandUseStndCode i.RoofCoverStndCode i.RoofStructureTypeStndCode i.HeatingTypeorSystemStndCode i.ACTypeorSystemStndCode i.FoundationTypeStndCode i.WaterStndCode i.SewerStndCode

*Continuous
local cts_vars YearBuilt YearRemodeled TotalRooms TotalBedrooms FullBath HalfBath maxareasqft GarageAreaSqFt FireplaceNumber LotSizeAcres

*Generating yearSold dummies to express in 2012 dollars/smooth out aggregate housing market
gen YearSold_2008 = 1 if YearSold == 2008
replace YearSold_2008 = 0 if YearSold != 2008
gen YearSold_2009 = 1 if YearSold == 2009
replace YearSold_2009 = 0 if YearSold != 2009
gen YearSold_2010 = 1 if YearSold == 2010
replace YearSold_2010 = 0 if YearSold != 2010
gen YearSold_2011 = 1 if YearSold == 2011
replace YearSold_2011 = 0 if YearSold != 2011


*Running Hedonic Regression
xtset GEOID10
xtreg log_price YearSold_* i.MonthSold `vars' `cts_vars', fe robust
predict hedonicPrice, u 

*Correcting sales prices to 2012 dollars using official inflation numbers
replace SalesPriceAmount = SalesPriceAmount*1.07 if YearSold == 2008 | YearSold == 2009 
replace SalesPriceAmount = SalesPriceAmount*(1.07/1.01) if YearSold == 2010
replace SalesPriceAmount = SalesPriceAmount*(1.07/1.04) if YearSold == 2011

*Storing these into .dta file
collapse (mean) hedonicPrice SalesPriceAmount, by(State County Tract BlockGroup CBSA CBSA_NAME)

sort CBSA

*Correcting for log-linearity
replace hedonicPrice = exp(hedonicPrice)

*Renaming for clarity
rename SalesPriceAmount averageHomePrice


save "Data/US_Data/Output/ZillowHedonicIndex.dta", replace

*Hedonic index for approximately 140,000 block groups, which is the large marjority of those in sample.


