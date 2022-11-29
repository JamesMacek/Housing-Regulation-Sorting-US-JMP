cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*Estimates a complex IV model-- requires output from CalibrateAmenityValues.R + Construct_donut_instrument.R
*Date edited: October 7th, 2022

**Part 1: simple cleaning and dataset construction**

*Nhgis block group data 
use "Data/US_Data/NHGIS/BlockGroup/nhgis_blkgrp.dta", clear

*Merging calibrated amenity values + other variables
merge 1:1 State County Tract BlockGroup using "Data/Counterfactuals/CalibratedAmenityValues.dta", keep(matched using)
drop _merge


*Aggregate income/# of households in ACS to get average income per household. 
gen log_Average_income  = log(qvbe001/qx6e001)



*Aggregating amenity values (average across all income groups to limit error)

*generating log variables
foreach var of varlist withincity_amenity_* {
	gen l`var' = log(`var')
}

*Taking average amenity value across income/education groups: this will be our main left hand size variable
egen log_Amenity = rowmean(lwithincity_amenity_*) 


*Encoding CBSA definitions
encode CBSA_NAME, generate(eCBSA_NAME) 

*replacing log_Amenity with commuting adjusted values
*Semi-elasticity from Redding/Sturm commuting paper
local commuting_semielasticity 0.01

*Partialling out effect of commute time
gen log_Amenity_decommute = log_Amenity
replace log_Amenity = log_Amenity + `commuting_semielasticity'*avg_commuteMins 


*Importing instrument data
merge 1:1 State County Tract BlockGroup CBSA CBSA_NAME using "Data/US_Data/Instrument/Instrument_constructed.dta", keep(master matched)



*Labels
la var Avg_Slope "Average Slope (deg)"
la var log_Average_income "lnIncome"
la var log_Amenity_decommute "lnAmenity"
la var MedianYearStructureBuilt "Median Build Year"
la var Public_transport_share "Public transport share"
la var Bus_share "Bus share"
la var avg_commuteMins "Average Commute (mins)"

*Control variables
gen Average_slope_control = control_Avg_Slope_spec_1
la var Average_slope_control "Slope Control"

la var instrument_Avg_Slope_spec_1 "Slope Donut (200-1000m)"
la var instrument_Avg_Slope_spec_2 "Slope Donut (500-1000m)"
la var instrument_Avg_Slope_spec_3 "Slope Donut (750-1250m)"
la var instrument_Avg_Slope_spec_4 "Slope Donut (1000-1500m)"

*Preferred specification = this one. 
ivreghdfe log_Amenity Avg_Slope (log_Average_income = instrument_Avg_Slope_spec_1), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) sfirst


*KP F STAT = 42!!!! Interesting- Avg slope as control variable is insignificant.
*Elasticity of 1.31 at this specification (about half our previous specification--which makes sense!) 


*OTHER SPECIFICATIONS
*If don't control for own Avg Slopes
ivreghdfe log_Amenity (log_Average_income = instrument_Avg_Slope_spec_1), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) 
*get slightly lower amenity value. Very interesting-- so slopes are not local? 

*No controls at all
ivreghdfe log_Amenity log_Average_income, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) 
*So-- we observe DOWNWARD bias. on the coefficient-- general equilibrium!

*TESTS
ivreghdfe log_Amenity Average_slope_control, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) sfirst
ivreghdfe log_Average_income Average_slope_control, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) sfirst

*Add Elevation
ivreghdfe log_Amenity Average_slope_control Avg_Ele (log_Average_income = instrument_Avg_Slope_spec_1 instrument_Avg_Elev_spec_1), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) sfirst
*Bad instrument sacrifices some first stage power.

*To do: check robustness with different buffer sizes, elevation. 
*Specifications 2 and 3
replace Average_slope_control = control_Avg_Slope_spec_2
ivreghdfe log_Amenity Average_slope_control (log_Average_income = instrument_Avg_Slope_spec_2), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME)
*Much weaker instrument on spec2, but much greater point estimates (could be due to higher standard errors?)
*This means what-- exactly? larger local exclusion 
replace Average_slope_control = control_Avg_Slope_spec_3
ivreghdfe log_Amenity Avg_Slope (log_Average_income = instrument_Avg_Slope_spec_3), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME)
*Slightly weaker instrument on spec3

*Checking estimates when commuting time allowed to vary arbitrarily 
*not much change in estimates! That's good. 

replace Average_slope_control = control_Avg_Slope_spec_2

local replace replace
ivreghdfe log_Amenity_decommute (log_Average_income = instrument_Avg_Slope_spec_2), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME)
local KP = round(`e(widstat)', 0.01)
outreg2 using "Data/US_Data/Instrument/Estimates", `replace' tex(pretty) label nocon dec(4) ///
addtext(KP-FStat, `KP', Cluster, MSA)

local replace

ivreghdfe log_Amenity_decommute Average_slope_control (log_Average_income = instrument_Avg_Slope_spec_2), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME)
local KP = round(`e(widstat)', 0.01)
outreg2 using "Data/US_Data/Instrument/Estimates", `replace' tex(pretty) label nocon dec(4) ///
addtext(KP-FStat, `KP', Cluster, MSA)

local replace

ivreghdfe log_Amenity_decommute Average_slope_control avg_commuteMins (log_Average_income = instrument_Avg_Slope_spec_2), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME)
local KP = round(`e(widstat)', 0.01)
outreg2 using "Data/US_Data/Instrument/Estimates", `replace' tex(pretty) label nocon dec(4) ///
addtext(KP-FStat, `KP', Cluster, MSA)

local replace

ivreghdfe log_Amenity_decommute Average_slope_control avg_commuteMins MedianYearStructureBuilt (log_Average_income = instrument_Avg_Slope_spec_2), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME)
local KP = round(`e(widstat)', 0.01)
outreg2 using "Data/US_Data/Instrument/Estimates", `replace' tex(pretty) label nocon dec(4) ///
addtext(KP-FStat, `KP', Cluster, MSA)

local replace

ivreghdfe log_Amenity_decommute Average_slope_control avg_commuteMins MedianYearStructureBuilt Public_transport_share (log_Average_income = instrument_Avg_Slope_spec_2), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME)
local KP = round(`e(widstat)', 0.01)
outreg2 using "Data/US_Data/Instrument/Estimates", `replace' tex(pretty) label nocon dec(4) ///
addtext(KP-FStat, `KP', Cluster, MSA)
*Public transportation share + building age corrects some DOWNWARD BIAS! Interesting...

local replace

ivreghdfe log_Amenity_decommute Average_slope_control avg_commuteMins MedianYearStructureBuilt Public_transport_share Bus_share (log_Average_income = instrument_Avg_Slope_spec_2), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME)
local KP = round(`e(widstat)', 0.01)
outreg2 using "Data/US_Data/Instrument/Estimates", `replace' tex(pretty) label nocon dec(4) ///
addtext(KP-FStat, `KP', Cluster, MSA)
*Public transportation share + building age corrects some DOWNWARD BIAS! Interesting...

*Put this in a table
local replace replace


*Robustnessness table

replace Average_slope_control = control_Avg_Slope_spec_1

ivreghdfe log_Amenity_decommute Average_slope_control avg_commuteMins MedianYearStructureBuilt Public_transport_share Bus_share (log_Average_income = instrument_Avg_Slope_spec_1), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(avg_commuteMins MedianYearStructureBuilt Public_transport_share Bus_share)
local KP = round(`e(widstat)', 0.01)
outreg2 using "Data/US_Data/Instrument/Estimates_robust", `replace' tex(pretty) label nocon dec(4) ///
addtext(KP-FStat, `KP', Cluster, MSA, Full Controls, Yes)


local replace

replace Average_slope_control = control_Avg_Slope_spec_2

ivreghdfe log_Amenity_decommute Average_slope_control avg_commuteMins MedianYearStructureBuilt Public_transport_share Bus_share (log_Average_income = instrument_Avg_Slope_spec_2), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(avg_commuteMins MedianYearStructureBuilt Public_transport_share Bus_share)
local KP = round(`e(widstat)', 0.01)
outreg2 using "Data/US_Data/Instrument/Estimates_robust", `replace' tex(pretty) label nocon dec(4) ///
addtext(KP-FStat, `KP', Cluster, MSA, Full Controls, Yes)

local replace

replace Average_slope_control = control_Avg_Slope_spec_3

ivreghdfe log_Amenity_decommute Average_slope_control avg_commuteMins MedianYearStructureBuilt Public_transport_share Bus_share (log_Average_income = instrument_Avg_Slope_spec_3), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(avg_commuteMins MedianYearStructureBuilt Public_transport_share Bus_share)
local KP = round(`e(widstat)', 0.01)
outreg2 using "Data/US_Data/Instrument/Estimates_robust", `replace' tex(pretty) label nocon dec(4) ///
addtext(KP-FStat, `KP', Cluster, MSA, Full Controls, Yes)

local replace

replace Average_slope_control = control_Avg_Slope_spec_4

ivreghdfe log_Amenity_decommute Average_slope_control avg_commuteMins MedianYearStructureBuilt Public_transport_share Bus_share (log_Average_income = instrument_Avg_Slope_spec_4), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(avg_commuteMins MedianYearStructureBuilt Public_transport_share Bus_share)
local KP = round(`e(widstat)', 0.01)
outreg2 using "Data/US_Data/Instrument/Estimates_robust", `replace' tex(pretty) label nocon dec(4) ///
addtext(KP-FStat, `KP', Cluster, MSA, Full Controls, Yes)

local replace


