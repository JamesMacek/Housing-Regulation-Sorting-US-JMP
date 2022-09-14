cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"


*Date created: April 27th 2022
*Date Edited: June 16th, 2022
*Requires output from M1_CurrentAssess_append.do ("CurrentAssess_matched.dta")

*Part 1
*Collapses to produce tract average single family land use FOR USE WITH MOTIVATING EVIDENCE ONLY

*Sample contains 86 million assessments!
use "Data/ZillowData/Output/CurrentAssess_matched.dta", clear 


*Keeping observations with definite single family use-- i.e. are coded RR101. 
*Includes semi detached houses?

*Retains 50 million observations. 
keep if PropertyLandUseStndCode == "RR101" | PropertyLandUseStndCode == "RR999" 


*Standardizing lot sizes to acres 
replace LotSizeAcres = LotSizeSquareFeet/43650 if LotSizeAcres == .

*Collapsing by BLOCK for descriptives. 
collapse (mean) LotSizeAcres, by(State County Tract BlockGroup) 
*Constructed means for 167000 Block Groups! (Note: this isn't the entire sample of ~177,000 because we only used single family homes)

*Saving output for use with other programs, namely to construct facts in Data_CensusACS_tractag_construct.R
save "Data/US_Data/Output/SingleFamilyLandConstructed.dta", replace



*Part 2 (Suppress for now)
*Constructing tract level distributions in lot size + land use to construct zoning districts
*NOTE: this is at the block group level. 
use "Data/ZillowData/Output/CurrentAssess_matched.dta", clear 

*Replacing in acres
replace LotSizeAcres = LotSizeSquareFeet/43650 if LotSizeAcres == .

*Single family identifier
gen SingleFamilyDummy = 1 if PropertyLandUseStndCode == "RR101" | PropertyLandUseStndCode == "RR999" 
replace SingleFamilyDummy = 0 if missing(SingleFamilyDummy) == 1

*Multifamily (duplexes to fourplexes only)
gen DuFourDummy = 1 if PropertyLandUseStndCode == "RI101" | PropertyLandUseStndCode == "RI102" |PropertyLandUseStndCode == "RI103"
replace DuFourDummy = 0 if missing(DuFourDummy) == 1


*Multifamily -- duplexes - high rise apartments(Duplexes to Quadplexes)
gen MultiFamilyDummy = 1 if strpos(PropertyLandUseStndCode, "RI") == 1
replace MultiFamilyDummy = 0 if missing(MultiFamilyDummy) == 1

*Commercial
gen CommercialDummy = 1 if strpos(PropertyLandUseStndCode, "CR") == 1 | strpos(PropertyLandUseStndCode, "CO") == 1 
replace CommercialDummy = 0 if missing(CommercialDummy) == 1

*Vacant/Conserved Land
gen VacantDummy = 1 if strpos(PropertyLandUseStndCode, "VL") == 1 
replace VacantDummy = 0 if missing(VacantDummy) == 1


*Generating lot size statistics for single family (want lower end of distribution)
bysort State County Tract BlockGroup: egen SingleF_p10 = pctile(LotSizeAcres) if SingleFamilyDummy == 1, p(10)

bysort State County Tract BlockGroup: egen SingleF_p25 = pctile(LotSizeAcres) if SingleFamilyDummy == 1, p(25)

bysort State County Tract BlockGroup: egen SingleF_p50 = pctile(LotSizeAcres) if SingleFamilyDummy == 1, p(50)

bysort State County Tract BlockGroup: egen SingleF_mean = mean(LotSizeAcres) if SingleFamilyDummy == 1

bysort State County Tract BlockGroup: egen SingleF_mode = mode(LotSizeAcres) if SingleFamilyDummy == 1, minmode

*Lot size statistics for single 
bysort State County Tract BlockGroup: egen MultiF_p10 = pctile(LotSizeAcres) if DuFourDummy == 1, p(10)

bysort State County Tract BlockGroup: egen MultiF_p25 = pctile(LotSizeAcres) if DuFourDummy == 1, p(25)

bysort State County Tract BlockGroup: egen MultiF_p50 = pctile(LotSizeAcres) if DuFourDummy == 1, p(50)

bysort State County Tract BlockGroup: egen MultiF_mean = mean(LotSizeAcres) if DuFourDummy == 1

bysort State County Tract BlockGroup: egen MultiF_mode = mode(LotSizeAcres) if DuFourDummy == 1, minmode



*Collapsing Data
collapse (mean) *Dummy SingleF_* MultiF_* [pw = LotSizeAcres], by(State County Tract BlockGroup CBSA CBSA_NAME)

*Imputing 0s in all of these 
 foreach var of varlist SingleF_* {
 	replace `var' = 0 if SingleFamilyDummy == 0
 }

 foreach var of varlist MultiF_* {
 	replace `var' = 0 if DuFourDummy == 0
 }

*Saving to output for use with other programs
save "Data/ZillowData/Output/BlockGroupLandUseLotSizeStats.dta", replace



