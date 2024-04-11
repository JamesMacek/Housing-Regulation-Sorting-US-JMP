cd $proj_filepath

*This file performs the entire IV estimation in amenity changes 2010-2020.
*Uses (generally) MSA level clustering, with a bartlett kernel estimation in the alternative do file. 
*Date created: Nov 30th, 2023

*Importing slope data (levels)
use "DataV2/US_Data/Instrument/Instrument_constructed.dta", clear

*Controls from our main empirical dataset,  wide format
merge 1:1 State County Tract BlockGroup CBSA using "DataV2/US_Data/Output/Constructed_Block_V2.dta" 
drop _merge

*Calibrated historical amenity values
merge 1:1 State County Tract BlockGroup CBSA CBSA_NAME using "DataV2/US_Data/Instrument/Amenity_values_forEstimation_historical.dta"
drop _merge
*Renaming to _hist convention before importing contemporary amenities
foreach var of varlist *_Amenity_* hedonicPrice {
	rename `var' `var'_hist
}

*RMA
merge 1:1 State County Tract BlockGroup CBSA CBSA_NAME using "DataV2/US_Data/Instrument/RMA_ctrl.dta"
drop _merge


*Contemporary values from calibration
merge 1:1 State County Tract BlockGroup CBSA CBSA_NAME using "DataV2/US_Data/Instrument/Amenity_values_forEstimation.dta"
drop _merge


*encoding CBSA_NAME
encode CBSA_NAME, generate(eCBSA_NAME)

*Encode on countyID
egen Countyid = group(State County)

*Encode on Tract
egen Tractid = group(State County Tract)


*Taking logs of all amenity values
foreach sampl in "" "_hist"  {
	forval incomeType = 1/7 {
		replace WithinCity_Amenity_`incomeType'`sampl' = log(WithinCity_Amenity_`incomeType'`sampl')
		replace lower_WithinCity_Amenity_`incomeType'`sampl' = log(lower_WithinCity_Amenity_`incomeType'`sampl')
		replace upper_WithinCity_Amenity_`incomeType'`sampl' = log(upper_WithinCity_Amenity_`incomeType'`sampl')
		
	}
}


*Aggregating amenity value
foreach sampl in "" "_hist" {
	*Amenities: our LHS variable
	egen log_Amenity`sampl' = rowmean(WithinCity_Amenity_*`sampl')
	*Upper and lower bounds for migration elasticity for robustness
	egen log_Amenity_upper`sampl' = rowmean(upper_WithinCity_Amenity_*`sampl')
	egen log_Amenity_lower`sampl' = rowmean(lower_WithinCity_Amenity_*`sampl')

	*Amenities by low, medium, high aggregated type
	egen log_Amenity_ltype`sampl' = rowmean(WithinCity_Amenity_1`sampl' WithinCity_Amenity_2`sampl')
	egen log_Amenity_mtype`sampl' = rowmean(WithinCity_Amenity_3`sampl' WithinCity_Amenity_4`sampl' WithinCity_Amenity_5`sampl')
	egen log_Amenity_htype`sampl' = rowmean(WithinCity_Amenity_6`sampl' WithinCity_Amenity_7`sampl')
}

*First differencing
foreach var of varlist log_Amenity log_Amenity_upper log_Amenity_lower log_Amenity_ltype log_Amenity_mtype log_Amenity_htype {
	gen d`var' = `var' - `var'_hist
	
	
}

*dlogIncome (our RHS variable)
gen dlog_Average_Income = log(Average_income) - log(Average_income_hist)
gen log_Average_Income_hist = log(Average_income_hist)


*All control variables to take first difference
foreach var of varlist demeaned_avg_travel_time demeaned_median_bage dm_public_transport_share demeaned_bus_share {
	gen d_`var' = `var' - `var'_hist
	
}



*ALL SPCECIFICATIONS: CONTROL FOR LOG LAND MASS
gen log_land = log(ALAND)

*RMA controls (these are bad controls, like density)
gen log_RMA = log(RMA10_0)

*Starting logs
cap log close
log using "DataV2/US_Data/Instrument/FULLdIV_MSACluster_log.txt", text replace


*________________________START CONSTRUCTION OF SPECIFICATION HERE
*Preferred specification. Change this if you want to change the following code. 
local Pref_spec = 3


*Locals for ring distances.
if `Pref_spec' == 1 {
	local instrumentString "0.2-1km"
}
if `Pref_spec' == 2 {
	local instrumentString "0.5-1km"
}
if `Pref_spec' == 3 {
	local instrumentString "0.75-1.25km"
}
if `Pref_spec' == 4 {
	local instrumentString "1-1.5km"
}
if `Pref_spec' == 5 {
	local instrumentString "1.5-3km"
}
if `Pref_spec' == 6 {
	local instrumentString "3-6km"
}
if `Pref_spec' == 7 {
	local instrumentString "6-10km"
}
*_________

*Labels for tables
cap la var instrument_Avg_Slope_spec_1 "Slope Donut (0.2-1km)"
cap la var instrument_Avg_Slope_spec_2 "Slope Donut (0.5-1km)"
cap la var instrument_Avg_Slope_spec_3 "Slope Donut (0.75-1.25km)"
cap la var instrument_Avg_Slope_spec_4 "Slope Donut (1-1.5km)"
cap la var instrument_Avg_Slope_spec_5 "Slope Donut (1.5-3km)"
cap la var instrument_Avg_Slope_spec_6 "Slope Donut (3-6km)"
cap la var instrument_Avg_Slope_spec_7 "Slope Donut (6-10km)"

cap la var log_Average_Income "ln Income"
cap la var log_Amenity "ln Amenity"

*Labels by income time
cap la var log_Amenity_ltype "ln Amenity (Low)"
cap la var log_Amenity_mtype "ln Amenity (Med)"
cap la var log_Amenity_htype "ln Amenity (High)"

*Create temporary variable with controls
*NOTE: WE USE CONTROL AVG SLOPE SPEC_ALT TO CONTROL FOR EXACT SLOPES IN BUFFER AROUND BLOCK GROUP (rather than via centroids)
gen Average_slope_control = control_Avg_Slope_spec_`Pref_spec'
la var Average_slope_control "Donut Slope Control"

*average slope at own block group (this is an additional control beyond average slopes of the buffer region)
la var Avg_Slope "Local Slope Control"


*ALL CONTROL SETS (to be partialled out of regression plot via FWL)
*_________________________________________________________________________________________________
*1) land mass + 
local control_set_1 "log_land"

*2) D2CBD, building age, public transport share, bus share (controlled separately). Most of change in coefficients is from demeaned_bus_share.
local control_set_2 "log_land log_RMA rank_inv_D2CBD d_demeaned_avg_travel_time d_demeaned_median_bage d_dm_public_transport_share d_demeaned_bus_share"

*
local control_set_3 "log_land log_RMA demeaned_avg_travel_time demeaned_median_bage dm_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD rank_density_CBSA"

*CHOOSE BASELINE CONTROL HERE
local baseline_controls `control_set_3'

*C1
*First difference of original regression
ivreghdfe dlog_Average_Income Average_slope_control Avg_Slope Max_Avg_Slope log_Average_Income_hist `baseline_controls' instrument_Avg_Slope_spec_`Pref_spec', absorb(i.eCBSA_NAME) partial(`baseline_controls') sfirst

*
ivreghdfe log_Amenity_hist Average_slope_control Avg_Slope Max_Avg_Slope log_Average_Income_hist (dlog_Average_Income = instrument_Avg_Slope_spec_`Pref_spec') `baseline_controls', absorb(i.eCBSA_NAME) partial(`baseline_controls') sfirst

*Placebo test fails for inner donut (donut 1), instruments weak for outer donut --


*_______________________________________________________________________________
***** Estimating model in differences ******************************************
*_______________________________________________________________________________

*
ivreghdfe dlog_Amenity Average_slope_control Avg_Slope Max_Avg_Slope `baseline_controls' (dlog_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) partial(`baseline_controls') sfirst


*Estimates for high and low types look identical in cross section-- similar levels of bias correction
*But NOT middle income types. Why? Very strange!

*Note: instruments on the weak side. 
foreach t in l m h {

ivreghdfe dlog_Amenity_`t'type Average_slope_control Avg_Slope  Max_Avg_Slope `baseline_controls' (dlog_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) partial(`baseline_controls') sfirst

*OLS
ivreghdfe dlog_Amenity_`t'type dlog_Average_Income Average_slope_control Avg_Slope Max_Avg_Slope  `baseline_controls', absorb(i.eCBSA_NAME) partial(`baseline_controls') sfirst
	
}


cap log close




