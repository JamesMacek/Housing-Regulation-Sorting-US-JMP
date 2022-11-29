cap cd  "Z:/Dropbox/Schoolfolder/Projects/Zoning"

*Estimates a first stage of the complex IV model on 2020 block group data.
*Uses (generally) MSA level clustering, but will move to a complete corrolellogram check and a Bartlett kernel.  
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


*Starting logs
log using "DataV2/US_Data/Instrument/FirstStageMSACluster_log.txt", text replace


*Preferred specification

la var Avg_Slope "Average Slope (deg)"
la var instrument_Avg_Slope_spec_1 "Slope Donut (200-1000m)"
la var instrument_Avg_Slope_spec_2 "Slope Donut (500-1000m)"
la var instrument_Avg_Slope_spec_3 "Slope Donut (750-1250m)"
la var instrument_Avg_Slope_spec_4 "Slope Donut (1000-1500m)"
la var log_Average_Income "lnIncome"

*Create temporary variable with controls
gen Average_slope_control = control_Avg_Slope_spec_1
la var Average_slope_control "Slope Control"
local replace replace

ivreghdfe Housing_density Average_slope_control (log_Average_Income = instrument_Avg_Slope_spec_1), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) sfirst
*For KP F Stat for specification one
*KP F-Stat = 29 (not 42 for our previous data, which is interesting.)
*Store KP F-Stat
local KP = round(`e(widstat)', 0.01)

*First stage

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_1, absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) sfirst
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, No, Amenity Controls, No, Topo Controls, No, KP-FStat, `KP', Cluster, MSA)

local replace


*CONTROL SET 1: PUBLIC TRANSPORT ACCESS + BUILDING AGE 
*Adding car share + demeaned stops per square mile is highly multicorrelated, which means this reduces the KP F Stat,
*Just control for public transport share, travel time, and building age which captures other alternative explanations for income sorting in the literature that may be important
*Could also control for stops per square mile instead of public transport share, which gets you a KP-F Stat of 19. However this control only varies at the tract level which is probably why it is noisier. It also doesn't affect the slope estimate too much. 
local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage demeaned_public_transport_share demeaned_bus_share"
ivreghdfe Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_1), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl') sfirst
local KP = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_1 `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, No, Topo Controls, No, KP-FStat, `KP', Cluster, MSA)

local replace


*CONTROL SET 2: DENSITY OF CONSUMPTION AMENITIES
*Coefficient virtually identical. 
local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage demeaned_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens demeaned_prop_park_area_tract"
ivreghdfe Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_1), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl') sfirst
local KP = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_1 `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, No, KP-FStat, `KP', Cluster, MSA)

local replace

*CONTROL SET 3: ADD CORRELATES WITH LAND COVER + D2CBD + density
local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage demeaned_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens rank_inv_D2CBD demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_density_CBSA"
ivreghdfe Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_1), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl') sfirst
local KP = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control instrument_Avg_Slope_spec_1 `selected_ctrl', absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSACluster", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, KP-FStat, `KP', Cluster, MSA)

local replace

*What we learn-- most endogeneity issues probably coming from average slope correlation with hard public transport infrastructure (i.e. subways, light rails, etc)
*We should control for them to get sensible results. 


*OTHER CHECKS 
*Clustering at smaller geographical units (Counties) with main controls
*Obviously F stat increases in these cases, less autocorrelated standard errors  
local replace replace

local selected_ctrl "demeaned_avg_travel_time demeaned_median_bage demeaned_public_transport_share demeaned_bus_share demeaned_coffee_dens demeaned_bar_dens demeaned_frestaurant_dens rank_inv_D2CBD demeaned_perennial_snow demeaned_deciduous_forest demeaned_evergreen_forest demeaned_shrubs demeaned_herbaceous demeaned_woody_wetlands demeaned_herbaceous_wetlands demeaned_mixed_forest rank_density_CBSA"
ivreghdfe Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_1), absorb(i.eCBSA_NAME) cluster(Countyid) partial(`selected_ctrl')
local KP = round(`e(widstat)', 0.01)


ivreghdfe log_Average_Income Average_slope_control `selected_ctrl' instrument_Avg_Slope_spec_1, absorb(i.eCBSA_NAME) cluster(Countyid) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSAClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, KP-FStat, `KP', Cluster, County)
*Massive KP F-Stat of 41 with county level clustering. 
local replace

*New measure
replace Average_slope_control = control_Avg_Slope_spec_2

*Alternative measures u-- run into weak instrument problem except for specification #2. 
ivreghdfe Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_2), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
local KP = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control `selected_ctrl' instrument_Avg_Slope_spec_2, absorb(i.eCBSA_NAME) cluster(Countyid) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSAClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, KP-FStat, `KP', Cluster, MSA)
local replace

*New measure
replace Average_slope_control = control_Avg_Slope_spec_3

ivreghdfe Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_3), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
local KP = round(`e(widstat)', 0.01)
local KP = round(`KP', 0.01)
di `KP'

ivreghdfe log_Average_Income Average_slope_control `selected_ctrl' instrument_Avg_Slope_spec_3, absorb(i.eCBSA_NAME) cluster(Countyid) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSAClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, KP-FStat, `KP', Cluster, MSA)
local replace

*New measure
replace Average_slope_control = control_Avg_Slope_spec_4

ivreghdfe Housing_density Average_slope_control `selected_ctrl' (log_Average_Income = instrument_Avg_Slope_spec_4), absorb(i.eCBSA_NAME) cluster(eCBSA_NAME) partial(`selected_ctrl')
local KP = round(`e(widstat)', 0.01)

ivreghdfe log_Average_Income Average_slope_control `selected_ctrl' instrument_Avg_Slope_spec_4, absorb(i.eCBSA_NAME) cluster(Countyid) partial(`selected_ctrl')
outreg2 using "DataV2/US_Data/Instrument/FirstStageMSAClusterRobust", `replace' tex(pretty) label nocon dec(4) ///
addtext(Base Controls, Yes, Amenity Controls, Yes, Topo Controls, Yes, KP-FStat, `KP', Cluster, MSA)

local replace replace

*We learned from robust estimates that 750m-1250m might have good first stage when using a more sensible clustering region, 
*but 500m-1000m seems like a good fit for first stage performance traded off with robustness to endogeneity issues. 

*close log
log close
