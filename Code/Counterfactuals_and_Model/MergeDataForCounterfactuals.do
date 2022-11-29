cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*This file pulls together all data required to perform the counterfactuals.
*Requires output from each Construct file + Housing Supply Elasticity Crosswalk.do
*Creates the final sample of block groups used in the counterfactual. 


*Creates the following objects in a data frame
*Ability distributions in each sample block
*Productivity (per unit of ability) in each sample block (does not vary within MSA)
*Used hedonic indices in each block

*Loading in zoning districts, from ConstructedZoningDistricts module
use "Data/ZillowData/Output/ConstructedZoningDistricts.dta", replace
*174823 block groups in sample so far that could be assigned to zoning districts via Zillow data.

*________Loading income distributions by block group
foreach var of varlist State County Tract BlockGroup CBSA {
	destring `var', replace
}

merge 1:1 State County Tract BlockGroup using "Data/US_Data/NHGIS/BlockGroup/nhgis_blkgrp.dta"
drop if _merge != 3
drop _merge
*174778 in sample, lost ~50 due to block group changes from 2010-2012, will correct later. 

*Generating fractions of homes in single family to fourplexes from ACS
gen SingleFamilyShare = (qyye002 + qyye003)/qyye001
gen DuplexShare = (qyye004)/qyye001
gen TriFourplexShare = (qyye005)/qyye001
gen RegulatedHousingUnitShare = SingleFamilyShare + DuplexShare + TriFourplexShare


*Income by group, aggregating into 7 bins 
*0-25k household income
gen Nominal_Income_bin_1 = (qu0e002 + qu0e003 + qu0e004 + qu0e005)/qu0e001
*25k-50k household income
gen Nominal_Income_bin_2 = (qu0e006 + qu0e007 + qu0e008 + qu0e009 + qu0e010)/qu0e001
*50k-75k
gen Nominal_Income_bin_3 = (qu0e011 + qu0e012)/qu0e001
*75k-100k
gen Nominal_Income_bin_4 = (qu0e013)/qu0e001
*100k-150k
gen Nominal_Income_bin_5 = (qu0e014 + qu0e015)/qu0e001
*150k-200k
gen Nominal_Income_bin_6 = (qu0e016)/qu0e001
*200k+
gen Nominal_Income_bin_7 = (qu0e017)/qu0e001


*667 block groups lost with no income data. 
drop if missing(Nominal_Income_bin_1) 

*adjusting income bin by type using city productivity data
merge m:1 CBSA using "Data/US_Data/Output/CityProd_individual.dta"
drop if _merge != 3
drop _merge
*126 block groups dropped from two small MSAs-- Corvalis, OR + Midland, MI


*merging on City-specific college-by-income distributions to construct labour supply per block group
merge m:1 CBSA using "Data/US_Data/Output/Income_bins.dta"
drop _merge

*Calculating income distributions by skill under the independence assumption
forval i = 1/7 {
	*College + NoCollege (via Bayes Rule)
	gen College_inc_bin_`i' = (Nominal_Income_bin_`i'*hhincome_bin_collegeshare`i')
	gen NoCollege_inc_bin_`i' = (Nominal_Income_bin_`i'*(1-hhincome_bin_collegeshare`i'))
}

*generating implied college share in each neighborhood
egen implied_college_share = rowtotal(College_inc_bin_*)
egen implied_nocollege_share = rowtotal(NoCollege_inc_bin_*)
*Each sums to 1.

*normalizing that quantity to 1 in each block group (i.e. distributions conditional on blockgroup + skill level)
forval i = 1/7 {
	replace College_inc_bin_`i' = College_inc_bin_`i'/implied_college_share
	replace NoCollege_inc_bin_`i' = NoCollege_inc_bin_`i'/(1-implied_college_share)
}

/* July 12th-- TURNING THIS PROCEDURE OFF FOR NOW. YIELDS ISSUES.
*Correcting income distributions by skill-city productivity to yield individual ability distributions.
forval i = 1/7 {
	gen CorrectedAbilityCollege`i' = hhincome_bin`i'/CollegeWage
	gen CorrectedAbilityNoCollege`i' = hhincome_bin`i'/NoCollegeWage
	gen correct_binCollege`i' = 0
	gen correct_binNoCollege`i'= 0
	
	foreach educ in College NoCollege {
		
		replace correct_bin`educ'`i' = 1 if 0 <= CorrectedAbility`educ'`i' & CorrectedAbility`educ'`i' < 25000
	
		replace correct_bin`educ'`i' = 2 if 25000 <= CorrectedAbility`educ'`i' & CorrectedAbility`educ'`i' < 50000

	
		replace correct_bin`educ'`i' = 3 if 50000 <= CorrectedAbility`educ'`i' & CorrectedAbility`educ'`i' < 75000

		replace correct_bin`educ'`i' = 4 if 75000 <= CorrectedAbility`educ'`i' & CorrectedAbility`educ'`i' < 100000
	
		replace correct_bin`educ'`i' = 5 if 100000 <= CorrectedAbility`educ'`i' & CorrectedAbility`educ'`i' < 150000
	
		replace correct_bin`educ'`i' = 6 if 150000 <= CorrectedAbility`educ'`i' & CorrectedAbility`educ'`i' < 200000
	
		replace correct_bin`educ'`i' = 7 if 200000 <= CorrectedAbility`educ'`i'

	}
	
}

*/

*Constructing correct ability distributions conditional on location and education, finally...
forval i = 1/7 {
	*Ability distributions by skill are in the `bin' variable 
	*JULY 12th -- just set this equal to the nominal income calcuated distributions. This makes sense on average since average city productivity is 1. Avoids extreme granularity issues
	gen Ability_bin_College`i' = College_inc_bin_`i'
	gen Ability_bin_NoCollege`i' = NoCollege_inc_bin_`i'
}

/* July 12th-- turning off for now! Yields some granularity issues when fitting amenities distributions.
forval i = 1/7 {
	forval j = 1/7 {
		
		replace Ability_bin_College`j' = College_inc_bin_`i' + Ability_bin_College`j' if correct_binCollege`i' == `j'
		
		replace Ability_bin_NoCollege`j' = NoCollege_inc_bin_`i' + Ability_bin_NoCollege`j' if correct_binNoCollege`i' == `j'
	
	}
	
}
*/

*Constructing ability distributions unconditional on skill from that. 
forval i = 1/7 {
	gen Ability_bin_`i'_unc = implied_college_share*Ability_bin_College`i' + (1-implied_college_share)*Ability_bin_NoCollege`i'
}


*testing if sums to 1, up to machine precision
egen test = rowtotal(Ability_bin_*_unc)
drop test


*Recovering average commuting time to work
*Taking center as a measure of median
*Constructing shares
forval i = 2/13 {
	
	if `i' < 10 {
		gen share_time_to_work`i' = qthe00`i'/qthe001
	}
	
	if `i' >= 10{
		gen share_time_to_work`i' = qthe0`i'/qthe001
	}

}
*Each row sums to 1 (egen test = rowsum(share_time_to_work*))
*calculating average time to work...
gen avg_commuteMins = 2*share_time_to_work2 + 7*share_time_to_work3 + 12*share_time_to_work4 + 17*share_time_to_work5 + 22*share_time_to_work6 + 27*share_time_to_work7 + 32*share_time_to_work8 + 37*share_time_to_work9 + 42*share_time_to_work10 + 52*share_time_to_work11 + 75*share_time_to_work12 + 95*share_time_to_work13

*replacing missing with MSA average commute times (only 69 block groups)
bysort CBSA: egen MSA_avg_CommuteMins = mean(avg_commuteMins)
replace avg_commuteMins = MSA_avg_CommuteMins if missing(avg_commuteMins)

*Calculating building age and public transport share as controls for instrument
gen MedianYearStructureBuilt = qy2e001
gen Public_transport_share = qtfe010/qtfe001
gen Bus_share = qtfe011/qtfe001

*Keeping needed variables for other programs
keep *Zoning* CBSA* State County Tract BlockGroup Ability_bin* *Wage Nominal_Income_bin* hhincome_bin* RegulatedHousingUnitShare SingleFamilyShare DuplexShare TriFourplexShare implied_college_share ability_grp* avg_commuteMins MedianYearStructureBuilt Public_transport_share Bus_share //CorrectedAbility*  *_inc_bin_*

drop hhincome_bin_*

*Part 3: merging in hendonic indices + aggregate home value measures + 2010 census housing counts
merge 1:1 State County Tract BlockGroup using "Data/US_Data/Output/ZillowHedonicIndex.dta"
drop if _merge == 2
drop _merge

*292 lost from above procedure, hence removing them

merge 1:1 State County Tract BlockGroup using "Data/US_Data/Output/SecondaryHedonicIndex.dta"
drop if _merge == 2
drop _merge

*Most matched.

*Generating zillow index identifier
gen ZillowIndex = 1 if missing(hedonicPrice) == 0

*Rescaling so secondary prices match log mean and log variance of Zillow index
gen loghedonicPrice = log(hedonicPrice)
*storing mean and sd of the logarithm of prices for Zillow index
sum loghedonicPrice
local ZillowMean `r(mean)'
local ZillowSd `r(sd)'

gen Secondary_loghedonicPrice = log(Secondary_hedonicPrice)
sum Secondary_loghedonicPrice
replace Secondary_loghedonicPrice = ((Secondary_loghedonicPrice - `r(mean)')/`r(sd)')*`ZillowSd' + `ZillowMean'
*Checking to see if same log mean and variance
sum loghedonicPrice Secondary_loghedonicPrice

replace hedonicPrice = exp(Secondary_loghedonicPrice) if missing(hedonicPrice)

*Checking correlation between zillow and secondary index, rsquared of 0.67
reg loghedonicPrice Secondary_loghedonicPrice, r
drop loghedonicPrice Secondary_loghedonicPrice

*Dropping if no hedonic price
drop if missing(hedonicPrice)

*171,440 block groups remain in sample. 

*importing Planning Database for census counts
save "Data/Counterfactuals/temp.dta", replace
import delimited using "Data/US_Data/CensusTract2010/2010PlanningDatabase.csv", clear 
rename state State
rename county County
rename tract Tract
rename block_group BlockGroup
*Keeping total housing units from 2010 census + land mass for block group
*Converting square miles to acres
gen land_area_acres = land_area*640
keep State County Tract BlockGroup tot_housing_units_cen_2010 land_area_acres
merge 1:1 State County Tract BlockGroup using "Data/Counterfactuals/temp.dta", keep(matched using)
*all matched
drop _merge


*Creating measures of housing value that combines these measures-- storing in averageHouseValue
gen averageHouseValue = .
*zillow based measures
replace averageHouseValue = averageHomePrice if ZillowIndex == 1
*secondary measures where aggregate value is available
replace averageHouseValue = Secondary_avgHomePrice if missing(Secondary_avgHomePrice) == 0 & missing(ZillowIndex) == 1
*secondary measures using more-available median house as a measure of center
replace averageHouseValue = Secondary_medHomePrice if missing(Secondary_avgHomePrice) & missing(ZillowIndex)

*Rescaling secondary measures so that they are comparable to the ZillowIndex-- both mean and variance. Rests on random data missing assumption, which may be wrong.



*PART 4: Joining data on minimum lot size estimates
merge 1:1 State County Tract BlockGroup using "Data/ZillowData/Output/LotSize_Master.dta"
drop if _merge == 2
drop _merge


*PART TWO
*___________________________________________________________________________________________________________
*Constructing actual measures of effective minimum lot size

*Detailed explanation of how it is constructed
*1) If fraction of housing units in singlefamily-fourplexes is below some threshold, set to zero. 
*2) If the percentage of lots that are below the restriction exceeds some threshold, set to zero.
*3) Take the minimum of SingleF, Duplex, Triplex and Fourplex, provided this minimum occurs with fraction of units above a certain threshold.
*4) If the minimum number of housing units implied by the density restriction exceeds the land mass of the block group, set to zero. 
*5) Impose a total cap on density restrictions to ignore clear anomolies-- i.e. 100 acre minimum lots for residential use.  

sort uZoningDistrictID

*Change local parameters here for assigning regulation to a block group.
*DEFAULT VALUE: HighDensityThreshold = 1 (no restriction), PctBelowRestrictionThreshold = 0.35, UnitModeThreshold = 0.35

local HighDensityThreshold 1
local PctBelowRestrictionThreshold 0.35
local UnitModeThreshold 0.35

*Assigning no regulation if share of singlefamily-fourplex units is below threshold
cap gen DensityRestriction = .
replace DensityRestriction = 0 if RegulatedHousingUnitShare < 1 -`HighDensityThreshold'

*Minor Variance threshold 
replace DensityRestriction = 0 if (PctBelow_SF_mode > `PctBelowRestrictionThreshold' & missing(PctBelow_SF_mode) == 0) | (PctBelow_Duplex_mode > `PctBelowRestrictionThreshold' & missing(PctBelow_Duplex_mode) == 0) | (PctBelow_Triplex_mode > `PctBelowRestrictionThreshold' & missing(PctBelow_Triplex_mode) == 0) | (PctBelow_Fourplex_mode > `PctBelowRestrictionThreshold' & missing(PctBelow_Fourplex_mode) == 0)

*changing threshold for which to assign the duplex-fourplex mode in terms of fraction of those units
*that appear in the ACS. Note:
replace Duplex_mode = . if (missing(SingleF_mode) == 0 | missing(Triplex_mode) == 0 | missing(Fourplex_mode) == 0) & DuplexShare < `UnitModeThreshold'

replace Triplex_mode = . if (missing(SingleF_mode) == 0 | missing(Duplex_mode) == 0 | missing(Fourplex_mode) == 0) & TriFourplexShare < `UnitModeThreshold'

replace Fourplex_mode = . if (missing(SingleF_mode) == 0 | missing(Duplex_mode) == 0 | missing(Triplex_mode) == 0) & TriFourplexShare < `UnitModeThreshold'

*Taking row minimum of duplexes-fourplexes, and adjusting DensityRestriction
egen temp = rowmin(SingleF_mode Duplex_mode Triplex_mode Fourplex_mode)
replace DensityRestriction = temp if missing(DensityRestriction)
drop temp

*Some remote locations have measured minimum lot sizes that are far too large to be real.
*Example-- 100 acre minimum lots. 
*capping these lot sizes at 5 acres?

*DEFAULT VALUE: DensityRestrictionCap = 5
local DensityRestrictionCap = 5
replace DensityRestriction = `DensityRestrictionCap' if DensityRestriction > `DensityRestrictionCap' & missing(DensityRestriction) == 0

*Constructing measures of land for residential use (only matters for regulated neighborhoods)
*1) generating measurements of land area if all homes were in regulated structures
gen lot_area = tot_housing_units_cen_2010*EffectiveLotSizeAcres

*Generating variable that takes minimum of land area and implied land if all units were in regulated structures
egen res_land_acres = rowmin(land_area_acres lot_area)

*Adjusting if density restriction imposed exceeds residential land estimate at census data (suggesting high density structures)
replace DensityRestriction = 0 if DensityRestriction*tot_housing_units_cen_2010 > res_land_acres


*Note: following measure of housing value density is only valid for regulated neighborhoods (ones are assigned DensityRestriction > 0)
*Note: dividing average house value by 
gen HousingValueDensity = averageHouseValue/LotSizeAcres_r
label var HousingValueDensity "Value of Housing per acre in regulated neighborhoods"
gen IncomeStringencyofRegulation = HousingValueDensity*DensityRestriction
label var IncomeStringencyofRegulation "Value of Housing per acre x minimum # of acres per housing unit"

*Conservatively setting the minimum lot size to zero if no mode could be found
replace DensityRestriction = 0 if missing(SingleF_mode) & missing(Duplex_mode) & missing(Triplex_mode) & missing(Fourplex_mode)
replace IncomeStringencyofRegulation = 0 if (missing(SingleF_mode) & missing(Duplex_mode) & missing(Triplex_mode) & missing(Fourplex_mode)) | RegulatedHousingUnitShare == 0 

*Importing housing supply elasticities,
merge m:1 State County Tract using "Data/Counterfactuals/Elasticities.dta", keep(master match)
drop _merge
*11,000 block groups not assigned supply elasticities under this scheme. Imputing these supply elasticities with the MSA average, and if not possible the sample-wide average
bysort CBSA: egen MSA_avg_elasticity = mean(converted_elasticities)
egen avg_elasticity = mean(converted_elasticities)

replace converted_elasticities = MSA_avg_elasticity if missing(converted_elasticities)
replace converted_elasticities = avg_elasticity if missing(converted_elasticities)


*removing negative measured elasticities
replace converted_elasticities = 0 if converted_elasticities <= 0

*Removing Alaska, which is for some reason in the sample 
drop if State == 2
*Drop some tracts that have no residential housing units in the Census (only one)
drop if tot_housing_units_cen_2010 == 0

*Checking to see if location choice is independent of education conditional on MSA and ability group.
forval i = 1/7 {
	 gen Jdensity_abl_College`i' = tot_housing_units_cen_2010*implied_college_share*Ability_bin_College`i'

	*Within CBSA totals
	bysort CBSA: egen CBSA_Jdens_abtot_College`i' = total(Jdensity_abl_College`i')
	
	replace Jdensity_abl_College`i' = Jdensity_abl_College`i'/CBSA_Jdens_abtot_College`i'
	
	gen Jdensity_abl_NoCollege`i' = tot_housing_units_cen_2010*(1-implied_college_share)*Ability_bin_NoCollege`i'
	
	*Within CBSA totals
	bysort CBSA: egen CBSA_Jdens_abtot_NoCollege`i' = total(Jdensity_abl_NoCollege`i')
	
	replace Jdensity_abl_NoCollege`i' = Jdensity_abl_NoCollege`i'/CBSA_Jdens_abtot_NoCollege`i'
}

*Close enough--some error due to rebinning of distributions, but that's fine. What we can do is assume away this error and only target ability distributions conditional on location but unconditional on education

drop Jdensity* CBSA_Jdens* MSA_avg_elasticity avg_elasticity

*Saving...
save "Data/Counterfactuals/JoinedDataForCounterfactuals.dta", replace
erase "Data/Counterfactuals/temp.dta"
