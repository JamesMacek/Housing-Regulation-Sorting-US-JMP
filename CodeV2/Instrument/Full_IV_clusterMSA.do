*Please do run_all_IV_estimation.do. This is not a standalone program. 

*Starting logs
cap log close
log using "DataV2/US_Data/Instrument/FULLIV_MSACluster_log.txt", text replace

*_________________________________________________________________________________________________
local replace replace

*_______OLS, NO CONTROLS______
ivreghdfe log_Amenity log_Average_Income Average_slope_control Avg_Slope Outer_slope_control  `control_set_1' if !missing(instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_1') sfirst
outreg2 using "DataV2/US_Data/Instrument/FullIVMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, OLS, Donut, . , Base Controls, No, Amen/Topo Controls, No, Density Control, No, KP-FStat, . , Cluster, MSA) 
local replace

*_______CONTROL SET 1:________
*Main spec
ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control `control_set_1' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_1') sfirst
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, No, Amen/Topo Controls, No, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

*First stage
local replace replace 
ivreghdfe log_Average_Income Average_slope_control Avg_Slope instrument_Avg_Slope_spec_`Pref_spec' Outer_slope_control `control_set_1' if missing(log_Amenity) == 0, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_1') sfirst
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, No, Amen/Topo Controls, No, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

*_______CONTROL SET 2:________
*Main spec
ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control `control_set_2' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_2') sfirst
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, No, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

*First stage
ivreghdfe log_Average_Income Average_slope_control Avg_Slope instrument_Avg_Slope_spec_`Pref_spec' Outer_slope_control `control_set_2' if missing(log_Amenity) == 0, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_2') sfirst
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, No, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

*______CONTROL SET 3:____________
*Coefficient virtually identical as last.
ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control `control_set_3' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_3') sfirst
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

ivreghdfe log_Average_Income Average_slope_control Avg_Slope instrument_Avg_Slope_spec_`Pref_spec' Outer_slope_control `control_set_3' if missing(log_Amenity) == 0, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_3') sfirst
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, No, KP-FStat, `KP', Cluster, MSA) 
local replace

*______CONTROL SET 4:____________
ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control `control_set_4' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_4') sfirst
local KP = round(`e(widstat)', 0.01)
local KP = string(`KP', "%4.1f")
outreg2 using "DataV2/US_Data/Instrument/FullIVMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, Yes, KP-FStat, `KP', Cluster, MSA) 
local replace

ivreghdfe log_Average_Income Average_slope_control Avg_Slope instrument_Avg_Slope_spec_`Pref_spec' Outer_slope_control `control_set_4' if missing(log_Amenity) == 0, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`control_set_4') sfirst
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, Yes, KP-FStat, `KP', Cluster, MSA) 
local replace
*_________________________________


*What we learn-- most endogeneity issues probably coming from average slope correlation with hard public transport infrastructure (i.e. subways, light rails, etc)
*We should control for them to get sensible results. 

*Aside: show that non-bus public transportation is a major omitted factor, but do not save table
di "WITH PUBLIC TRANSPORT"
ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst
di "WITHOUT PUBLIC TRANSPORT"
ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control `baseline_control_rmpubtr' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_control_rmpubtr') sfirst

*OTHER CHECKS___________________________________________________________________ 

*Clustering at smaller geographical units (Counties) with main controls
*Obviously F stat increases in these cases, less autocorrelated standard errors (not for output)
ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(Countyid) partial(`baseline_controls')

*FOR OUTPUT, ROBUSTNESS TO DIFFERENT DONUTS AT MAIN CONTROLS (make sure to loop over number of donuts calculated)
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

	*BASELINE SPECIFICATION
	
	*__________Replacing slopes for this donut________________
	replace Average_slope_control = control_Avg_Slope_spec_`i'
	replace Outer_slope_control = outer_control_Avg_Slope_spec_`i'
	*_________________________________________________________
	
	ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`i'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls')
	local KP = round(`e(widstat)', 0.1)
	local KP = string(`KP', "%4.1f")
	
	outreg2 using "DataV2/US_Data/Instrument/FullIVMSAClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `donutString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator', KP-FStat, `KP', Cluster, MSA) 


	*ROBUSTNESS: Remove Outer buffer to show that estimates significantly change
	ivreghdfe log_Amenity Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`i'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls')
	local KP = round(`e(widstat)', 0.1)
	local KP = string(`KP', "%4.1f")
	
	outreg2 using "DataV2/US_Data/Instrument/FullIVMSAClusterRobust_noMaxSlope", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `donutString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator', KP-FStat, `KP', Cluster, MSA) 
	
	if `i' != 7 {
		local replace
	}
}

local replace replace

*_______________________________________________________________________________
*CHECKING ADDITIONAL ROBUSTNESS, SAVED IN LOG ONLY
*_______________________________________________________________________________
*Replacing average slope control back to baseline specification
replace Average_slope_control = control_Avg_Slope_spec_`Pref_spec'
replace Outer_slope_control = outer_control_Avg_Slope_spec_`Pref_spec'

*OLS at baseline controls
ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' log_Average_Income, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl') sfirst

*Add hedonic price controls to see if things change a lot (see if our assumptions about housing demand matter)
ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control hedonicPrice `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst

*Lower and upper bounds on migration elasticity (corresponding to 95% CI in Baum-Snow & Han 2023) -- things change a bit, as expected
ivreghdfe log_Amenity_lower Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst

ivreghdfe log_Amenity_upper Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst

ivreghdfe log_Amenity Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst


*OLS + IV (MSA CLUSTERING) FOR OUTPUT
local replace replace

foreach t in l m h {
	*IV
	ivreghdfe log_Amenity_`t'type Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst
	
	outreg2 using "DataV2/US_Data/Instrument/IVMSAClust_byIncomeType", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator') drop(`baseline_controls')
local replace
}

local replace replace 

foreach t in l m h {
	*Check, NO MAX SLOPE
	ivreghdfe log_Amenity_`t'type Average_slope_control Avg_Slope `baseline_controls' (log_Average_Income = instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls') sfirst
	
	outreg2 using "DataV2/US_Data/Instrument/IVMSAClust_byIncomeType_noMaxSlope", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV, Donut, `instrumentString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator') drop(`baseline_controls')
local replace
}

*______________________________________________________________________________
*OLS by income type
local replace replace 

foreach t in l m h {
	*OLS by income type
	ivreghdfe log_Amenity_`t'type log_Average_Income Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' if !missing(instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`baseline_controls')
	
	
	outreg2 using "DataV2/US_Data/Instrument/OLSMSAClust_byIncomeType", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, OLS, Donut, `instrumentString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator') drop(`baseline_controls')
local replace
}
*_______________________________________________________________________________


*_______________________________________________________________________________
*Baseline specification IVPPML by income type (use control function approach with ppmlhdfe and reghdfe), no bootstrap (to do, incl bootstrap--this is just for robustness of estimates)


*Taking first stage, which is the same across all income types since amenities are defined in all neighborhoods in sample thus far (dataset includes some out of sample from earlier merge)
reghdfe log_Average_Income Average_slope_control Avg_Slope instrument_Avg_Slope_spec_`Pref_spec' Outer_slope_control `baseline_controls' if!missing(instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) 
predict l_Avg_Inc_hat if e(sample)
label var l_Avg_Inc_hat "Ln Income (Residualized)"
*Note: amenities not missing in exp() model because we don't drop zeroes
	

*IV PMML estimates
local replace replace 

foreach t in l m h {
	*Take first stage 
	ppmlhdfe WithinCity_Amenity_`t' l_Avg_Inc_hat Average_slope_control Avg_Slope Outer_slope_control `baseline_controls', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME)
	
		outreg2 using "DataV2/US_Data/Instrument/IV_PPML_MSAClust_byIncomeType", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, IV-PPML, Donut, `instrumentString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator') drop(`baseline_controls')
	
	local replace 
}

*No Instrument PPML, looks the similar to OLS
local replace replace

foreach t in l m h {
	ppmlhdfe WithinCity_Amenity_`t' log_Average_Income Average_slope_control Avg_Slope Outer_slope_control `baseline_controls' if !missing(instrument_Avg_Slope_spec_`Pref_spec'), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME)
	
		outreg2 using "DataV2/US_Data/Instrument/PPML_MSAClust_byIncomeType", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, PPML, Donut, `instrumentString', Base Controls, `baseControls_indicator', Amen/Topo Controls, `AmenTopo_Controls_indicator', Density Control, `DensityControl_indicator') drop(`baseline_controls')
local replace 
}



cap log close