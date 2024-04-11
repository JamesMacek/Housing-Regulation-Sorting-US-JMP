*This file performs the placebo test
*Uses (generally) MSA level clustering, with a bartlett kernel estimation in the alternative do file. 
*Date created: Nov 30th, 2023


*__LOCALS___
******************************************************
*ESTIMATES BY INCOME TYPE FROM FULLIV_CLUSTER_BARTLETT.do
*These need to be manually inputed (for now). Maybe automate?

*Pooled estimates for baseline
local Omega_baseline 0.2287

*Pooled OLS estimates
local Omega_OLS 0.058

*Baseline Estimates by type (baseline from FullIV_BartlettCluster.do)
local Omega_l 0.1341
local Omega_m 0.286
local Omega_h 0.3



*Estimates for each donut (to run placebo test with each donut definition)
*Order is as appears in the CluserRobust table (i.e. other tables)
local Omega_1 0.1574
local Omega_2 0.1848
local Omega_3 0.2287
local Omega_4 0.2483
*This is the weakest instrument, all other estimates here do not make sense
local Omega_5 0.5978
local Omega_6 -0.0636
*Also weak
local Omega_7 -0.1893
******************************************************

*renaming some long variable names because acreg will run into an error otherwise (just for historical placebo)
rename dm_public_transport_share_hist d_public_transport_shareh


	*Partialling out amenity value caused by income assuming previous estimates are consistent (at baseline)		
		gen plog_Amenity_hist = log_Amenity_hist - `Omega_baseline'*log_Average_Income_hist
		
		

	*By income type
		foreach t in l m h {
			gen plog_Amenity_`t'type_hist = log_Amenity_`t'type_hist - `Omega_`t''*log_Average_Income_hist
		}
	
	*By Donut Specification
		forval i = 1/7 {
			gen plog_Amenity_hist_`i' = log_Amenity_hist - `Omega_`i''*log_Average_Income_hist
		
		}

	*OLS
	gen plog_Amenity_hist_OLS = log_Amenity_hist - `Omega_OLS'*log_Average_Income_hist
		
*Labels
la var plog_Amenity_hist "ln Fund. Amenity"
la var plog_Amenity_ltype_hist "ln Fund. Amenity (low)"
la var plog_Amenity_mtype_hist "ln Fund. Amenity (med)"
la var plog_Amenity_htype_hist "ln Fund. Amenity (high)"

forval i = 1/7 {
	la var plog_Amenity_hist_`i' "ln Fund. Amenity"
}

la var log_Average_Income "ln Income (2020)"


*Starting logs
cap log close
log using "DataV2/US_Data/Instrument/Placebo.txt", text replace

*ALL CONTROL SETS FOR PLACEBO (Separate because they contain other controls for variables in 2010 cross-section)
*_________________________________________________________________________________________________
*1) land mass + 
local control_set_1 "log_land"

*2) D2CBD, 2010 Tract RMA, building age, public transport share, bus share (controlled separately).
local control_set_2 "log_land demeaned_avg_travel_time_hist demeaned_median_bage_hist d_public_transport_shareh demeaned_bus_share_hist rank_inv_D2CBD"

*3) Additional topographic features in 2020 (obviously going to be highly correlated over time)
local control_set_3 "log_land demeaned_avg_travel_time_hist demeaned_median_bage_hist d_public_transport_shareh demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD"

*3) Additionally controling for density ranking in 2020
local control_set_4 "log_land demeaned_avg_travel_time_hist demeaned_median_bage_hist d_public_transport_shareh demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_inv_D2CBD rank_density_CBSA"

*CHOOSE BASELINE CONTROL SET HERE (in line with other baseline estimates)
local baseline_controls_placebo `control_set_2'

if "`baseline_controls_placebo'" == "`control_set_1'" {
    local baseControls_indicator "No"
	local AmenTopo_Controls_indicator "No"
	local DensityControl_indicator "No"
}

if "`baseline_controls_placebo'" == "`control_set_2'" {
    local baseControls_indicator "Yes"
	local AmenTopo_Controls_indicator "No"
	local DensityControl_indicator "No"
}

if "`baseline_controls_placebo'" == "`control_set_3'" {
    local baseControls_indicator "Yes"
	local AmenTopo_Controls_indicator "Yes"
	local DensityControl_indicator "No"
}

if "`baseline_controls_placebo'" == "`control_set_4'" {
    local baseControls_indicator "Yes"
	local AmenTopo_Controls_indicator "Yes"
	local DensityControl_indicator "Yes"
}

******************************************************************************
***********START PLACEBO TESTS HERE*******************************************
******************************************************************************

local replace replace

*REGRESS: partialled historical amenities on log_Average_Income
	acreg plog_Amenity_hist Average_slope_control Avg_Slope Outer_slope_control `baseline_controls_placebo' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2
	local KP = round(`e(widstat)', 0.1)
	local KP = string(`KP', "%4.1f")
	outreg2 using "DataV2/US_Data/Instrument/Placebo_Baseline", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator', FStat Bart c $dist_cutoff km, `KP') drop(`baseline_controls_placebo')	

local replace

*OLS
acreg plog_Amenity_hist log_Average_Income Average_slope_control Avg_Slope Outer_slope_control `baseline_controls_placebo' if !missing(instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2
	local KP = round(`e(widstat)', 0.1)
	local KP = string(`KP', "%4.1f")
	outreg2 using "DataV2/US_Data/Instrument/Placebo_Baseline", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, OLS, Donut, `instrumentString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator') drop(`baseline_controls_placebo')	

local replace

*Repeating by income type 
foreach t in l m h {
	
	acreg plog_Amenity_`t'type_hist Average_slope_control Avg_Slope Outer_slope_control `baseline_controls_placebo' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2
	local KP = round(`e(widstat)', 0.1)
	local KP = string(`KP', "%4.1f")
	outreg2 using "DataV2/US_Data/Instrument/Placebo_Baseline", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator', FStat Bart c $dist_cutoff km, `KP') drop(`baseline_controls_placebo')	
	
	local replace 
}



*OLS COUNTERPARTS:

*Doing OLS for breakup (use ivreghdfe to speed up, for log only)
foreach t in l m h {
	ivreghdfe plog_Amenity_`t'type_hist log_Average_Income Average_slope_control Avg_Slope Outer_slope_control  `baseline_controls_placebo' if !missing(instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls_placebo') sfirst
	
}

**********TEST: CHECKING IF PLACEBO REJECTS POOLED OLS*********************************
*(Assuming pooled OLS was significant)
ivreghdfe plog_Amenity_hist_OLS log_Average_Income Average_slope_control Avg_Slope Outer_slope_control  `baseline_controls_placebo' if !missing(instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls_placebo') sfirst
*OLS Rejects, but no significant change in estimates (note; not bootstrapped)
***********************************************************************************

/* grey this out for testing
*Estimation by donut to check if placebo tests pass at all donuts (even nonsense ones)
local replace replace

forval i = 1/7 {
		
  	if `i' == 1 {
		local donutString "0.2-1km"
	}
	if `i' == 2 {
		local donutString "0.5-1km"
	}
	if `i' == 3 {
		local donutString "0.75-1.25km"
	}
	if `i' == 4 {
		local donutString "1-1.5km"
	}
	if `i' == 5 {
		local donutString "1.5-3km"
	}
	if `i' == 6 {
		local donutString "3-6km"
	}
	if `i' == 7 {
		local donutString "6-8km"
	}
	
	
	*__________Replacing slopes for this donut________________
	replace Average_slope_control = control_Avg_Slope_spec_`i'
	replace Outer_slope_control = outer_control_Avg_Slope_spec_`i'
	*_________________________________________________________
	
	acreg plog_Amenity_hist_`i' Average_slope_control Avg_Slope Outer_slope_control `baseline_controls_placebo' (log_Average_Income = instrument_Avg_Slope_spec_`i'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2
	
	local KP = round(`e(widstat)', 0.1)
	local KP = string(`KP', "%4.1f")
	
	outreg2 using "DataV2/US_Data/Instrument/Placebo_Robust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `donutString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator', FStat Bart c $dist_cutoff km, `KP') drop(`baseline_controls')
	
	if `i' != 7 {
		local replace
	}
	
	
}

local replace replace
*Replacing average slope control back to baseline specification
replace Average_slope_control = control_Avg_Slope_spec_`Pref_spec'
replace Outer_slope_control = outer_control_Avg_Slope_spec_`Pref_spec'
*/

log close

*******************************************************************************
