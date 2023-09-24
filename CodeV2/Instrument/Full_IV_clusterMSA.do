cd $proj_filepath

*This file performs the entire IV estimation in the data.
*Uses (generally) MSA level clustering, with a bartlett kernel estimation in the alternative do file. 
*Date created: Feb 4th, 2023

*Importing all data 
use "DataV2/US_Data/Instrument/Instrument_constructed.dta", clear

*Alternate instrument controls
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
*NOTE: WE ARE DROPPING POTENTIALLY MANY ZEROS IN THE DATA HERE
forval incomeType = 1/7 {
	replace WithinCity_Amenity_`incomeType' = log(WithinCity_Amenity_`incomeType')
	replace lower_WithinCity_Amenity_`incomeType' = log(lower_WithinCity_Amenity_`incomeType')
	replace upper_WithinCity_Amenity_`incomeType' = log(upper_WithinCity_Amenity_`incomeType')
	
}
*Amenities: our LHS variable
egen log_Amenity = rowmean(WithinCity_Amenity_*)
*Upper and lower bounds for migration elasticity for robustness
egen log_Amenity_upper = rowmean(upper_WithinCity_Amenity_*)
egen log_Amenity_lower = rowmean(lower_WithinCity_Amenity_*)

*Amenities by low, medium, high aggregated type
egen log_Amenity_ltype = rowmean(WithinCity_Amenity_1 WithinCity_Amenity_2)
egen log_Amenity_mtype = rowmean(WithinCity_Amenity_3 WithinCity_Amenity_4 WithinCity_Amenity_5)
egen log_Amenity_htype = rowmean(WithinCity_Amenity_6 WithinCity_Amenity_7)

*Income (our RHS variable)
gen log_Average_Income = log(Average_income)

*ALL SPCECIFICATIONS: CONTROL FOR LOG LAND MASS
gen log_land = log(ALAND)

*Starting logs
cap log close
log using "DataV2/US_Data/Instrument/FULLIV_MSACluster_log.txt", text replace


*Preferred specification. Change this if you want to change the following code. 
local Pref_spec = 3

*Locals for ring distances.
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

*Labels for tables
cap la var instrument_Avg_Slope_spec_1 "Slope Donut (200-1000m)"
cap la var instrument_Avg_Slope_spec_2 "Slope Donut (500-1000m)"
cap la var instrument_Avg_Slope_spec_3 "Slope Donut (750-1250m)"
cap la var instrument_Avg_Slope_spec_4 "Slope Donut (1000-1500m)"
cap la var log_Average_Income "ln Income"
cap la var log_Amenity "ln Amenity"

*Labels by income time
cap la var log_Amenity_ltype "ln Amenity (Low)"
cap la var log_Amenity_mtype "ln Amenity (Med)"
cap la var log_Amenity_htype "ln Amenity (High)"

*Create temporary variable with controls
*NOTE: WE USE CONTROL AVG SLOPE SPEC_ALT TO CONTROL FOR EXACT SLOPES IN BUFFER AROUND BLOCK GROUP (rather than via centroids)
gen Average_slope_control = control_Avg_Slope_spec_`Pref_spec'_alt
la var Average_slope_control "Donut Slope Control"

*average slope at own block group (this is an additional control beyond average slopes of the buffer)
la var Avg_Slope "Local Slope Control"

*ALL CONTROL SETS (to be partialled out of regression plot via FWL)
*_________________________________________________________________________________________________
*1) land mass + 
local control_set_1 "log_land"

*2) D2CBD, building age, public transport share, bus share (controlled separately). Most of change in coefficients is from demeaned_bus_share.
local control_set_2 "log_land demeaned_avg_travel_time demeaned_median_bage demeaned_public_transport_share demeaned_bus_share rank_inv_D2CBD"

*3) Add amenity controls + topographical controls (OUR BASELINE SET OF CONTROLS)
local control_set_3 "log_land demeaned_avg_travel_time demeaned_median_bage demeaned_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens demeaned_prop_park_area_tract demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD"
*Investigating removing public transport in robustness check
local control_set_3_rmpubtr "log_land demeaned_avg_travel_time demeaned_median_bage demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens demeaned_prop_park_area_tract demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD"

*5) Add CBSA density ranking + white and skill share 
local control_set_4 "log_land demeaned_avg_travel_time demeaned_median_bage demeaned_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD rank_density_CBSA"

*NOTE: controlling for white and college share makes this relationship negative an insignitficant (and the instrument irrelevant). This is not surprising-- its a bad control!

*CHOOSE BASELINE CONTROL HERE
local baseline_controls `control_set_3'
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


*_________________________________________________________________________________________________
local replace replace

*_______OLS, NO CONTROLS______
ivreghdfe log_Amenity log_Average_Income Average_slope_control Avg_Slope `control_set_1' if !missing(instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_1') sfirst
outreg2 using "DataV2/US_Data/Instrument/FullIVMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, OLS, Donut, . , Base Controls, No, Amen/Topo Controls, No, Density Control, No, KP-FStat, . , Cluster, MSA) 
local replace

*_______CONTROL SET 1:________
*Main spec
ivreghdfe log_Amenity Average_slope_control Avg_Slope `control_set_1' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_1') sfirst
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, No, Amen/Topo Controls, No, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

*First stage
local replace replace 
ivreghdfe log_Average_Income Average_slope_control Avg_Slope instrument_Avg_Slope_spec_`Pref_spec' `control_set_1', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_1') sfirst
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, No, Amen/Topo Controls, No, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

*_______CONTROL SET 2:________
*Main spec
ivreghdfe log_Amenity Average_slope_control Avg_Slope `control_set_2' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_2') sfirst
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, No, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

*First stage
ivreghdfe log_Average_Income Average_slope_control Avg_Slope instrument_Avg_Slope_spec_`Pref_spec' `control_set_2', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_2') sfirst
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, No, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

*______CONTROL SET 3:____________
*Coefficient virtually identical as last.
ivreghdfe log_Amenity Average_slope_control Avg_Slope `control_set_3' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_3') sfirst
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

ivreghdfe log_Average_Income Average_slope_control Avg_Slope instrument_Avg_Slope_spec_`Pref_spec' `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl') sfirst
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

*______CONTROL SET 4:____________
ivreghdfe log_Amenity Average_slope_control Avg_Slope `control_set_4' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_4') sfirst
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, Yes, KP-FStat, `KP', Cluster, MSA) 
local replace

ivreghdfe log_Average_Income Average_slope_control Avg_Slope instrument_Avg_Slope_spec_`Pref_spec' `control_set_4', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_4') sfirst
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, Yes, KP-FStat, `KP', Cluster, MSA) 
local replace
*_________________________________


*What we learn-- most endogeneity issues probably coming from average slope correlation with hard public transport infrastructure (i.e. subways, light rails, etc)
*We should control for them to get sensible results. 

*Aside: show that non-bus public transportation is a major omitted factor, but do not save table
di "WITH PUBLIC TRANSPORT"
ivreghdfe log_Amenity Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst
di "WITHOUT PUBLIC TRANSPORT"
ivreghdfe log_Amenity Average_slope_control Avg_Slope `baseline_control_rmpubtr' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_control_rmpubtr') sfirst

*OTHER CHECKS___________________________________________________________________ 

*Clustering at smaller geographical units (Counties) with main controls
*Obviously F stat increases in these cases, less autocorrelated standard errors (not for output)
ivreghdfe log_Amenity Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(Countyid) partial(`baseline_controls')

*FOR OUTPUT, ROBUSTNESS TO DIFFERENT DONUTS AT MAIN CONTROLS
local replace replace
forvalues i = 1/4 { 
    
	if `i' == 1 {
		local donutString "200-1000m"
	}
	if `i' == 2 {
		local donutString "500-1000m"
	}
	if `i' == 3 {
		local donutString "750-1250m"
	}

	if `i' == 4 {
		local donutString "1000-1500m"
	}
	
	replace Average_slope_control = control_Avg_Slope_spec_`i'_alt
	ivreghdfe log_Amenity Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`i'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls')
	local KP = round(`e(widstat)', 0.1)
	local KP = string(`KP', "%4.1f")
	
	outreg2 using "DataV2/US_Data/Instrument/FullIVMSAClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `donutString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator', KP-FStat, `KP', Cluster, MSA) 
	
	if `i' != 4 {
		local replace
	}
}

local replace replace


*CHECKING ADDITIONAL ROBUSTNESS, TO SAVE IN LOG__________________________________________________________________________________________________________

replace Average_slope_control = control_Avg_Slope_spec_`Pref_spec'_alt

*OLS at baseline controls
ivreghdfe log_Amenity Average_slope_control Avg_Slope `baseline_controls' log_Average_Income, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl') sfirst

*Add hedonic price controls to see if things change a lot (see if our assumptions about housing demand matter)
ivreghdfe log_Amenity Average_slope_control Avg_Slope hedonicPrice `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst

*Lower and upper bounds on migration elasticity (corresponding to 95% CI in BaumSnow & Han 2023) -- things change a bit
ivreghdfe log_Amenity_lower Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst

ivreghdfe log_Amenity_upper Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst

ivreghdfe log_Amenity Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst


*FINALLY, SPLITTING UP ESTIMATION BY INCOME GROUP (final specification will be using Bartlett kernels)_____________________________________
*LOW TYPES
ivreghdfe log_Amenity_ltype Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst
ivreghdfe log_Amenity_ltype Average_slope_control Avg_Slope `baseline_controls' log_Average_Income, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst

*MED TYPES
ivreghdfe log_Amenity_mtype Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst
ivreghdfe log_Amenity_mtype Average_slope_control Avg_Slope `baseline_controls' log_Average_Income, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst

*HIGH TYPES
ivreghdfe log_Amenity_htype Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst
ivreghdfe log_Amenity_htype Average_slope_control Avg_Slope `baseline_controls' log_Average_Income, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst

*SPLITTING UP ESTIMATION BY INCOME GROUP (final specification will be using Bartlett kernels)_____________________________________

local replace replace

foreach t in l m h {
	ivreghdfe log_Amenity_`t'type log_Average_Income Average_slope_control Avg_Slope `baseline_controls' if !missing(instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls')
	
	
	outreg2 using "DataV2/US_Data/Instrument/OLSMSAClust_byIncomeType", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, OLS, Donut, `instrumentString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator') drop(`baseline_controls')
local replace
}



cap log close