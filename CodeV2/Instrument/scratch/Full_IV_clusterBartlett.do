cd $proj_filepath

*This file performs the entire IV estimation in the data.
*Uses (generally) Bartlett kernel clustering. 
*Date created: Feb 4th, 2023

*Importing all data 
use "DataV2/US_Data/Instrument/Instrument_constructed.dta", clear

merge 1:1 State County Tract BlockGroup CBSA using "DataV2/US_Data/Instrument/Instrument_constructed_alternate.dta" 
drop _merge


*Controls from our main empirical dataset
merge 1:1 State County Tract BlockGroup CBSA CBSA_NAME using "DataV2/US_Data/Output/Constructed_Block_V2.dta" 
drop _merge
*Amenity values from calibration
merge 1:1 State County Tract BlockGroup CBSA CBSA_NAME using "DataV2/US_Data/Instrument/Amenity_values_forEstimation.dta"
drop _merge

*encoding CBSA_NAME
encode CBSA_NAME, generate(eCBSA_NAME)

*Encode on countyID
egen Countyid = group(State County)

*Encode on Tract
egen Tractid = group(State County Tract)

*constructing aggregated index of amenities == unweighted average of all types
*NOTE: WE ARE DROPPING (MANY!) ZEROS IN THE DATA HERE.
forval incomeType = 1/7 {
	replace WithinCity_Amenity_`incomeType' = log(WithinCity_Amenity_`incomeType')
	
}
*Amenities: our LHS variable
egen log_Amenity = rowmean(WithinCity_Amenity_*)

*Dropping for final sample
drop if log_Amenity == .

*Income (our RHS variable)
gen log_Average_Income = log(Average_income)

*Starting logs
log using "DataV2/US_Data/Instrument/FULLIV_BartlettCluster_log.txt", text replace


*Preferred specification (500-1000m)
local Pref_spec = 2

if `Pref_spec' == 1 {
	local instrumentString "200-1000m"
}
if `Pref_spec' == 2 {
	local instrumentString "500-1000m"
}
if `Pref_spec' == 3 {
	local instrumentString "750-1250m"
}

if `Pref_spec' == 4 {
	local instrumentString "1000-1500m"
}


cap la var instrument_Avg_Slope_spec_1 "Slope Donut (200-1000m)"
cap la var instrument_Avg_Slope_spec_2 "Slope Donut (500-1000m)"
cap la var instrument_Avg_Slope_spec_3 "Slope Donut (750-1250m)"
cap la var instrument_Avg_Slope_spec_4 "Slope Donut (1000-1500m)"
cap la var log_Average_Income "ln Income"
cap la var log_Amenity "ln Amenity Value"

*Create temporary variable with controls
*NOTE: WE USE CONTROL AVG SLOPE SPEC_ALT TO CONTROL FOR EXACT SLOPES IN BUFFER AROUND BLOCK GROUP
gen Average_slope_control = control_Avg_Slope_spec_`Pref_spec'_alt
la var Average_slope_control "Slope Control"

*renaming some long variable names because acreg will run into an error otherwise 
rename demeaned_public_transport_share d_public_transport_share

*Checking correlelogram on first stage of main specification
*Preferred specification for checking spatial autocorrelation in errors?
reghdfe log_Amenity log_Average_Income control_Avg_Slope_spec_`Pref_spec'_alt, absorb(i.eCBSA_NAME) residuals(base_error_spec2`Pref_spec')

*Do this for each city and calculate each 
*San Fransico (reduced sample to calculate quickly)
foreach d in 25 50 75 100 {
quietly moransi base_error_spec2 if CBSA == 41860, lat(Lat) lon(Lon) swm(bin) dist(`d') dunit(km) nomatsave
di `r(I)'
}

*at 50km, correlation in errors drops very low in SF to approximatley 0.02. Choose acreg bartlett kernel
*with ~ 35km in bandwidth should suffice (midpoint between 25km and 50km)
*Baseline first stage specification with no controls
*TESTING
*Units in kilometers, informed by calculation about in San Fransico
global dist_cutoff = 35 

local replace replace

*Starting regressions (we don't explicitly estimate the first stage)
acreg log_Amenity Average_slope_control (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)
 
outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Donut, `instrumentString', Base Controls, No, Amenity Controls, No, Topo Controls, No, FStat Bart c $dist_cutoff km, `BF')

local replace 

*First set of main controls 
local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage d_public_transport_share demeaned_bus_share rank_inv_D2CBD"
acreg log_Amenity Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01) 

outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Donut, `instrumentString', Base Controls, Yes, Amenity Controls, No, Topo Controls, No, FStat Bart c $dist_cutoff km, `BF') drop(`selected_ctrl')

local replace

*Amenity controls from NaNDA
local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage d_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens demeaned_prop_park_area_tract rank_inv_D2CBD"
acreg log_Amenity Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Donut, `instrumentString', Base Controls, Yes, Amenity Controls, Yes, Topo Controls, No, FStat Bart c $dist_cutoff km, `BF') drop(`selected_ctrl')

local replace

*Add topographic controls 
local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage d_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens  demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD rank_density_CBSA"
acreg log_Amenity Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Donut, `instrumentString', Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, FStat Bart c $dist_cutoff km, `BF') drop(`selected_ctrl')

local replace 

*PART2: Robustness to alternative definitions of cutoff

*Robustness to alternative definitions
local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage d_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens  demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD rank_density_CBSA"

local replace replace

replace Average_slope_control = control_Avg_Slope_spec_1_alt

acreg log_Amenity Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_1), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Donut, 200-1000m, Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, FStat Bart c $dist_cutoff km, `BF') drop(`selected_ctrl')

local replace 

replace Average_slope_control = control_Avg_Slope_spec_2_alt

acreg log_Amenity Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_2), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Donut, 500-1000m, Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, FStat Bart c $dist_cutoff km, `BF') drop(`selected_ctrl')
 
local replace 

replace Average_slope_control = control_Avg_Slope_spec_3_alt

acreg log_Amenity Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_3), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Donut, 750-1250m, Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, FStat Bart c $dist_cutoff km, `BF') drop(`selected_ctrl')

local replace 

replace Average_slope_control = control_Avg_Slope_spec_4_alt

acreg log_Amenity Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_4), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Donut, 1000-1500m, Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, FStat Bart c $dist_cutoff km, `BF') drop(`selected_ctrl')

local replace 


log close