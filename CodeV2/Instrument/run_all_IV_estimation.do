*This file runs all IV estimation in a unified way
*Date created: Jan 19th, 2024

cd $proj_filepath

*This file performs the entire IV estimation in the data.
*Uses (generally) MSA level clustering, with a bartlett kernel estimation in the alternative do file. 
*This file also (uniquely) estimates 
*Date created: Feb 4th, 2023

*Importing all data 
use "DataV2/US_Data/Instrument/Instrument_constructed.dta", clear

*Controls from our main empirical dataset
merge 1:1 State County Tract BlockGroup CBSA using "DataV2/US_Data/Output/Constructed_Block_V2.dta" 
drop _merge

*Calibrated historical amenity values
merge 1:1 State County Tract BlockGroup CBSA CBSA_NAME using "DataV2/US_Data/Instrument/Amenity_values_forEstimation_historical.dta"
drop _merge

*Renaming to _hist convention before importing contemporary amenities
foreach var of varlist *_Amenity_* hedonicPrice {
	rename `var' `var'_hist
}

*Contemporary amenity values
merge 1:1 State County Tract BlockGroup CBSA CBSA_NAME using "DataV2/US_Data/Instrument/Amenity_values_forEstimation.dta"
drop _merge

*encoding CBSA_NAME
encode CBSA_NAME, generate(eCBSA_NAME)

*Encode on countyID
egen Countyid = group(State County)

*Encode on Tract
egen Tractid = group(State County Tract)


*AMENITY VALUES 
*Amenities by low, medium, high aggregated type
foreach sampl in "" "_hist"  {
	foreach t in l m h {
		gen log_Amenity_`t'type`sampl' = log(WithinCity_Amenity_`t'`sampl')
		
		*lower and upper confidence intervals on estimate of 
		*within-city migration elasticity
		foreach s in upper lower {
			gen `s'_log_Amenity_`t'type`sampl' = log(`s'_WithinCity_Amenity_`t'`sampl')
		}
	}
}
	
*Amenities: our LHS variable
	*Taking average amenity across aggregated types for pooled estimation
	*Note: log of zeros are dropped from calculation of the mean
foreach sampl in "" "_hist"  {
	egen log_Amenity`sampl' = rowmean(log_Amenity_*type`sampl')
	
	*lower and upper confidence intervals on estimate of 
	*within-city migration elasticity
	foreach s in upper lower {
		egen log_Amenity_`s'`sampl' = rowmean(`s'_log_Amenity_*type`sampl')
	}
}

*Income (our RHS variable)
	gen log_Average_Income = log(Average_income)
	gen log_Average_Income_hist = log(Average_income_hist)

*Dropping obs with no lat/lon info (only a few hundred) 
*These are outside estimation sample anyways
	drop if missing(Lat) | missing(Lon)	
	
*ALL SPCECIFICATIONS: CONTROL FOR LOG LAND MASS (area unit problem otherwise)
	gen log_land = log(ALAND)


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
	local instrumentString "6-8km"
}

*_________

*Labels for tables
cap la var instrument_Avg_Slope_spec_1 "Slope Donut (0.2-1km)"
cap la var instrument_Avg_Slope_spec_2 "Slope Donut (0.5-1km)"
cap la var instrument_Avg_Slope_spec_3 "Slope Donut (0.75-1.25km)"
cap la var instrument_Avg_Slope_spec_4 "Slope Donut (1-1.5km)"
cap la var instrument_Avg_Slope_spec_5 "Slope Donut (1.5-3km)"
cap la var instrument_Avg_Slope_spec_6 "Slope Donut (3-6km)"
cap la var instrument_Avg_Slope_spec_7 "Slope Donut (6-8km)"

cap la var log_Average_Income "ln Income"
cap la var log_Amenity "ln Amenity"

*Labels by income time
cap la var log_Amenity_ltype "ln Amenity (Low)"
cap la var log_Amenity_mtype "ln Amenity (Med)"
cap la var log_Amenity_htype "ln Amenity (High)"
*Amenities in levels
cap la var WithinCity_Amenity_l "Amenity (Low)"
cap la var WithinCity_Amenity_m "Amenity (Med)"
cap la var WithinCity_Amenity_h "Amenity (High)"

*Create temporary variable with controls
*Slope of inner buffer as control
gen Average_slope_control = control_Avg_Slope_spec_`Pref_spec'
la var Average_slope_control "Slope Control"

gen Outer_slope_control = outer_control_Avg_Slope_spec_`Pref_spec'
la var Outer_slope_control "Outer Slope Control"

*average slope at own block group (this is an additional control beyond average slopes of the buffer)
la var Avg_Slope "Local Slope Control"

*ALL CONTROL SETS (to be partialled out of regression plot via FWL)
*_________________________________________________________________________________________________
*1) land mass + white share (ignoring racial effects)
local control_set_1 "log_land"

*2) D2CBD, building age, public transport share, bus share (controlled separately). Most of change in coefficients is from demeaned_bus_share.
local control_set_2 "log_land demeaned_avg_travel_time demeaned_median_bage dm_public_transport_share demeaned_bus_share rank_inv_D2CBD"

*3) Add amenity controls + topographical controls (OUR BASELINE SET OF CONTROLS)
local control_set_3 "log_land demeaned_avg_travel_time demeaned_median_bage dm_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens demeaned_prop_park_area_tract demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD"

*Investigating removing public transport in robustness check
local control_set_3_rmpubtr "log_land demeaned_avg_travel_time demeaned_median_bage demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens demeaned_prop_park_area_tract demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD"

*5) Add CBSA density ranking 
local control_set_4 "log_land demeaned_avg_travel_time demeaned_median_bage dm_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD rank_density_CBSA"

*NOTE: controlling for white and college share makes this relationship negative an insignitficant (and the instrument irrelevant). This is not surprising-- its a bad control!

*CHOOSE BASELINE CONTROL HERE FOR ESTIMATION
local baseline_controls `control_set_2'
local baseline_control_rmpubtr `control_set_3_rmpubtr'

*BASELINE CONTROL INDICATORS FOR PLOT
if "`baseline_controls'" == "`control_set_1'" {
    local baseControls_indicator "No"
	local AmenTopo_Controls_indicator "No"
	local DensityControl_indicator "No"
}

if "`baseline_controls'" == "`control_set_2'" {
    local baseControls_indicator "Yes"
	local AmenTopo_Controls_indicator "No"
	local DensityControl_indicator "No"
}

if "`baseline_controls'" == "`control_set_3'" {
    local baseControls_indicator "Yes"
	local AmenTopo_Controls_indicator "Yes"
	local DensityControl_indicator "No"
}

if "`baseline_controls'" == "`control_set_4'" {
    local baseControls_indicator "Yes"
	local AmenTopo_Controls_indicator "Yes"
	local DensityControl_indicator "Yes"
}


*____ INDIVIDUAL ESTIMATION FILES HERE______
*35km Bartlett kernel, see test in FullIV_clusterBartlett.do
global dist_cutoff = 35 

*Use include to pass local macros into these subroutines
*The following do files take very long to run, as calculating Bartlett standard errors is time consuming. 

*1) Quicker estimation using MSA clustering first (used for testing)
include "CodeV2/Instrument/Functions/Full_IV_clusterMSA.do"

*2) Baseline estimates using Bartlett kernel + placebo tests
include "CodeV2/Instrument/Functions/Full_IV_clusterBartlett.do"

*3) First stage estimates using Bartlett kernel
include "CodeV2/Instrument/Functions/FirstStage_clusterBartlett.do"

*4) Placebo tests (Requires locals from Full_IV_clusterMSA.do)
*include "CodeV2/Instrument/Functions/Placebo.do"