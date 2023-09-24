cd $proj_filepath

*Estimates a first stage of the complex IV model on 2020 block group data. 
*Date edited: Nov 4th, 2022

**Part 1: simple cleaning and dataset construction**

*Importing 

use "DataV2/US_Data/Instrument/Instrument_constructed.dta", clear

merge 1:1 State County Tract BlockGroup CBSA CBSA_NAME using "DataV2/US_Data/Output/Constructed_Block_V2.dta"
*NOTE: unmatched are those without positive land mass or number of households from census

*encoding CBSA_NAME
encode CBSA_NAME, generate(eCBSA_NAME)

*Encode on countyID
egen Countyid = group(State County)

*Encode on Tract
egen Tractid = group(State County Tract)

*Estimating the first stage of our equation
*Note: housing density is a placeholder for our first stage in this preliminary program

gen log_Average_Income = log(Average_income)

*Label Variables
la var Avg_Slope "Average Slope (deg)"
la var instrument_Avg_Slope_spec_1 "Slope Donut (200m-1000m)"
la var instrument_Avg_Slope_spec_2 "Slope Donut (500m-1000m)"
la var instrument_Avg_Slope_spec_3 "Slope Donut (750m-1250m)"
la var instrument_Avg_Slope_spec_4 "Slope Donut (1000m-1500m)"
la var log_Average_Income "lnIncome"


*dropping unmerged
drop if _merge == 1

*renaming some long variable names because acreg will run into an error otherwise 
rename demeaned_public_transport_share d_public_transport_share

*Starting logs
log using "DataV2/US_Data/Instrument/FirstStageClusterBartlett_log.txt", text replace



*Checking correlelogram on base first stage for each specification
*Preferred specification? 
reghdfe log_Average_Income control_Avg_Slope_spec_2 instrument_Avg_Slope_spec_2, absorb(i.eCBSA_NAME) residuals(base_error_spec2)

*Do this for each city and calculate each 
*San Fransico (reduced sample to calculate quickly)
foreach d in 25 50 75 100 {
quietly moransi base_error_spec2 if CBSA == 41860, lat(Lat) lon(Lon) swm(bin) dist(`d') dunit(km) nomatsave
di `r(I)'
}

*at 50km, correlation in errors drops very low in SF to approximatley 0.02. Choose acreg bartlett kernel
*with ~ 35km in bandwidth should suffice (midpoint between 25km and 50km)

*Initializing first set of controls with baseline specification: 500m-1km buffer that seems to provide the best balance between first stage + distance
gen Average_slope_control = control_Avg_Slope_spec_2
la var Average_slope_control "Slope Control"

*Baseline first stage specification with no controls
*TESTING
*Units in kilometers, informed by calculation about in San Fransico
global dist_cutoff = 35 


local replace replace

acreg Housing_density Average_slope_control (log_Average_Income = instrument_Avg_Slope_spec_2), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)
*use faster ivreghdfe with MSA clustering (NOTE: standard errors on these coeffients in table do not reflect the coefficients using acreg. This is because we want to speed things up and there is no way to do ACreg twice while also being fast. Very slow to calculate these errors)

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_2, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) 
outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, No, Amenity Controls, No, Topo Controls, No, FStat Bart c $dist_cutoff km, `BF')

local replace

local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage d_public_transport_share demeaned_bus_share"
acreg Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_2), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_2 `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, No, Topo Controls, No, FStat Bart c $dist_cutoff km, `BF')

local replace

local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage d_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens demeaned_prop_park_area_tract"
acreg Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_2), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_2 `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, No, FStat Bart c $dist_cutoff km, `BF')

local replace

local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage d_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens rank_inv_D2CBD demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_density_CBSA"
acreg Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_2), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_2 `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettCluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, FStat Bart c $dist_cutoff km, `BF')

local replace

*Robustness to alternative definitions
local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage d_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens rank_inv_D2CBD demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_density_CBSA"

local replace replace

replace Average_slope_control = control_Avg_Slope_spec_1

acreg Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_1), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_1 `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, FStat Bart c $dist_cutoff km, `BF')

local replace

replace Average_slope_control = control_Avg_Slope_spec_3

acreg Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_3), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_3 `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, FStat Bart c $dist_cutoff km, `BF')

local replace

replace Average_slope_control = control_Avg_Slope_spec_4

acreg Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_4), spatial latitude(Lat) longitude(Lon) dist($dist_cutoff) pfe1(eCBSA_NAME) bartlett 
local BF = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_4 `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageBartlettClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, FStat Bart c $dist_cutoff km, `BF')

*close log
log close
