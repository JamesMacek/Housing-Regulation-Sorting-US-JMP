*This is where Full_IV_clusterBartlett diverges from Full_IV_clusterMSA
*Starting logs
cap log close
log using "DataV2/US_Data/Instrument/FULLIV_BartlettCluster_log.txt", text replace
*_________________________________________________________________________________________________


*CALCULATING ROUGHLY SENSIBLE DISTANCE CUTOFF FOR BARTLETT
*Checking correlelogram on first stage of main specification
*Preferred specification for checking spatial autocorrelation in errors?
reghdfe log_Amenity log_Average_Income control_Avg_Slope_spec_`Pref_spec', absorb(i.eCBSA_NAME) residuals(base_error_spec2`Pref_spec')

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


*_________________________________________________________________________________________________
local replace replace

*_______OLS___________________
acreg log_Amenity log_Average_Income Average_slope_control Avg_Slope Outer_slope_control `control_set_1' if !missing(instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2  
outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettCluster", `replace' tex(pretty) label nocon dec(4) nor2 ///
addtext(Specification, OLS, Donut, ., Base Controls, No, Amen/Topo Controls, No, Density Control, No, FStat Bart c $dist_cutoff km, .) drop(`control_set_1')
local replace 

*_______CONTROL SET 1:________
*Main spec
acreg log_Amenity Average_slope_control Avg_Slope Outer_slope_control `control_set_1' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettCluster", `replace' tex(pretty) label nocon dec(4) nor2 ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, No, Amen/Topo Controls, No, Density Control, No, FStat Bart c $dist_cutoff km, `KP') drop(`control_set_1')
local replace

*_______CONTROL SET 2:________
*Main spec
acreg log_Amenity Average_slope_control Avg_Slope Outer_slope_control `control_set_2' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettCluster", `replace' tex(pretty) label nocon dec(4) nor2 ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, No, Density Control, No, FStat Bart c $dist_cutoff km, `KP') drop(`control_set_2')
local replace

*______CONTROL SET 3:____________
*Coefficient virtually identical as last.
acreg log_Amenity Average_slope_control Avg_Slope Outer_slope_control `control_set_3' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettCluster", `replace' tex(pretty) label nocon dec(4) nor2 ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, No, FStat Bart c $dist_cutoff km, `KP') drop(`control_set_3')
local replace

*______CONTROL SET 4:____________
acreg log_Amenity Average_slope_control Avg_Slope Outer_slope_control `control_set_4' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettCluster", `replace' tex(pretty) label nocon dec(4) nor2 ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, Yes, FStat Bart c $dist_cutoff km, `KP') drop(`control_set_4')
local replace

*_________________________________

*PART2: Robustness to alternative definitions of cutoff at baseline specification
*FOR OUTPUT, ROBUSTNESS TO DIFFERENT DONUTS AT MAIN CONTROLS
local replace replace
forvalues i = 1/7 { 
	
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
	
	
	acreg log_Amenity Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`i'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2
	
	local KP = round(`e(widstat)', 0.1)
	local KP = string(`KP', "%4.1f")
	
	outreg2 using "DataV2/US_Data/Instrument/FullIVBartlettClusterRobust", `replace' tex(pretty) label nocon dec(4) nor2 ///
addtext(Specification, IV, Donut, `donutString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator', FStat Bart c $dist_cutoff km, `KP') drop(`baseline_controls')
	
	if `i' != 7 {
		local replace
	}
}


*Replacing average slope control back to baseline specification
replace Average_slope_control = control_Avg_Slope_spec_`Pref_spec'
replace Outer_slope_control = outer_control_Avg_Slope_spec_`Pref_spec'


*SPLITTING UP ESTIMATION BY INCOME GROUP (final specification will be using Bartlett kernels)_____________________________________

local replace replace

foreach t in l m h {
	acreg log_Amenity_`t'type Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2
	
	local KP = round(`e(widstat)', 0.1)
	local KP = string(`KP', "%4.1f")
	
	outreg2 using "DataV2/US_Data/Instrument/FullIVBartlett_byIncomeType", `replace' tex(pretty) label nocon dec(4) nor2 ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator', FStat Bart c $dist_cutoff km, `KP') drop(`baseline_controls')
local replace
}

cap log close