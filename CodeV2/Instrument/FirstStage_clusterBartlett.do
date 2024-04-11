cap log close
log using "DataV2/US_Data/Instrument/FirstStage_BartlettCluster_log.txt", text replace

*This file runs first stage regressions using the Bartlett kernel for all controls. 

local replace replace


*_______CONTROL SET 1:________
	acreg log_Average_Income instrument_Avg_Slope_spec_`Pref_spec' Average_slope_control Avg_Slope Outer_slope_control `control_set_1', spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2 
local KP = round(`e(widstat)', 0.1)
local KP = string(`KP', "%4.1f")
	outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, FirstStage, Donut, `instrumentString', Base Controls, No, Amen/Topo Controls, No, Density Control, No) drop(`control_set_1')

local replace

*_______CONTROL SET 2:________
	acreg log_Average_Income instrument_Avg_Slope_spec_`Pref_spec' Average_slope_control Avg_Slope Outer_slope_control `control_set_2', spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2  
local KP = round(`e(widstat)', 0.1)
local KP = string(`KP', "%4.1f")
	outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, FirstStage, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, No, Density Control, No) drop(`control_set_2')

local replace

*_______CONTROL SET 3:________
	acreg log_Average_Income instrument_Avg_Slope_spec_`Pref_spec' Average_slope_control Avg_Slope Outer_slope_control `control_set_3', spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2  
local KP = round(`e(widstat)', 0.1)
local KP = string(`KP', "%4.1f")
	outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, FirstStage, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, No) drop(`control_set_3')

local replace 

*_______CONTROL SET 4:________
	acreg log_Average_Income instrument_Avg_Slope_spec_`Pref_spec' Average_slope_control Avg_Slope Outer_slope_control `control_set_4', spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett correctr2  
local KP = round(`e(widstat)', 0.1)
local KP = string(`KP', "%4.1f")
	outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Specification, FirstStage, Donut, `instrumentString', Base Controls, Yes, Amen/Topo Controls, Yes, Density Control, Yes) drop(`control_set_4')

log close