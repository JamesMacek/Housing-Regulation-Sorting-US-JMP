cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"


*Date created:June 20th 2022
*Date Edited: July 4th, 2022
*Requires output from M1_CurrentAssess_append.do ("CurrentAssess_matched.dta") +
*AlgorithmConstructZoningDistrictsV3.R (latest version)

*Constructs min lot sizes estimates using a simple mode procedure. 

*Part 1
*Sample contains 86 million assessments!
use "Data/ZillowData/Output/CurrentAssess_matched.dta", clear 

*Merging with zoning district definitions 
merge m:m State County Tract BlockGroup using "Data/ZillowData/Output/ConstructedZoningDistricts.dta"

*generating effective lot size variables
*Single Family Homes
gen SingleFamilyDummy = 1 if PropertyLandUseStndCode == "RR101" | PropertyLandUseStndCode == "RR999" 
replace SingleFamilyDummy = 0 if missing(SingleFamilyDummy) == 1

*Duplexes
gen DuplexDummy = 1 if PropertyLandUseStndCode == "RI101"
replace DuplexDummy = 0 if missing(DuplexDummy) == 1

*Triplexes
gen TriplexDummy = 1 if PropertyLandUseStndCode == "RI102"
replace TriplexDummy = 0 if missing(TriplexDummy) == 1

*Fourplexes
gen FourplexDummy = 1 if PropertyLandUseStndCode == "RI103"
replace FourplexDummy = 0 if missing(FourplexDummy) == 1

*Structures with => 5 housing units 
gen HighDensityStructureDummy = 1 if PropertyLandUseStndCode == "RI104" | PropertyLandUseStndCode == "RI105" | PropertyLandUseStndCode == "RI106" | PropertyLandUseStndCode == "RI107" | NoOfUnits > 4
replace HighDensityStructureDummy = 0 if missing(HighDensityStructureDummy)

*Effective lot size dummies
gen EffectiveLotSizeAcres = LotSizeAcres if SingleFamilyDummy == 1

*Calculating measures of average land per housing unit is on lot
replace EffectiveLotSizeAcres = LotSizeAcres/2 if DuplexDummy == 1
replace EffectiveLotSizeAcres = LotSizeAcres/3 if TriplexDummy == 1
replace EffectiveLotSizeAcres = LotSizeAcres/4 if FourplexDummy == 1

*Retaining variable with lot sizes for singlefamily - Fourplex (use this to construct observed measures of lot size stringency)
gen LotSizeAcres_r = LotSizeAcres if SingleFamilyDummy == 1 | DuplexDummy == 1 | TriplexDummy == 1 | FourplexDummy == 1

*Calculating modes for various types of structures (with inferred number of housing units)
bysort uZoningDistrictID: egen SingleF_mode = mode(EffectiveLotSizeAcres) if SingleFamilyDummy == 1, minmode

bysort uZoningDistrictID: egen Duplex_mode = mode(EffectiveLotSizeAcres) if DuplexDummy == 1, minmode

bysort uZoningDistrictID: egen Triplex_mode = mode(EffectiveLotSizeAcres) if TriplexDummy == 1, minmode

bysort uZoningDistrictID: egen Fourplex_mode = mode(EffectiveLotSizeAcres) if FourplexDummy == 1, minmode

*Calculating which (percent) of homes are below this mode for additional robustness

local Mode_threshold = 0.8

*Accounting for zoning variance, tagging lots that have a lot size < 0.8* the minimum
gen PctBelow_SF_mode = 1 if SingleFamilyDummy == 1 & EffectiveLotSizeAcres < `Mode_threshold'*SingleF_mode
replace PctBelow_SF_mode = 0 if SingleFamilyDummy == 1 & missing(PctBelow_SF_mode) 

gen PctBelow_Duplex_mode = 1 if DuplexDummy == 1 & EffectiveLotSizeAcres < `Mode_threshold'*Duplex_mode
replace PctBelow_Duplex_mode = 0 if DuplexDummy == 1 & missing(PctBelow_Duplex_mode) 

gen PctBelow_Triplex_mode = 1 if TriplexDummy == 1 & EffectiveLotSizeAcres < `Mode_threshold'*Triplex_mode
replace PctBelow_Triplex_mode = 0 if TriplexDummy == 1 & missing(PctBelow_Triplex_mode) 

gen PctBelow_Fourplex_mode = 1 if FourplexDummy == 1 & EffectiveLotSizeAcres < `Mode_threshold'*Fourplex_mode
replace PctBelow_Fourplex_mode = 0 if FourplexDummy == 1 & missing(PctBelow_Fourplex_mode) 

*Generating more dummies for detailed different types of land uses
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

*Agricultural land uses
gen AgriculturalDummy = 1 if strpos(PropertyLandUseStndCode, "AG") == 1
replace AgriculturalDummy = 0 if missing(AgriculturalDummy) == 1




*Collapsing and saving
*Collapsing Data (data are unweighted)
collapse (mean) *Dummy *_mode EffectiveLotSizeAcres LotSizeAcres_r, by(State County Tract BlockGroup CBSA CBSA_NAME uZoningDistrictID ZoningDistrictID)

foreach var of varlist State County Tract BlockGroup CBSA {
	destring `var', replace
}

*Saving dataset-- final data that will be used as input into the model
save "Data/ZillowData/Output/LotSize_Master.dta", replace