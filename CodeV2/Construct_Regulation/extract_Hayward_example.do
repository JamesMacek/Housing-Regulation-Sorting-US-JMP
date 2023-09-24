*This file produces assessments for Hayward, CA for use in Zoning_figures_output.R
cd $proj_filepath

use clip landusecode propertyindicatorcode numberofunits acres landsquarefootage State County Tract BlockGroup CBSA Geocoded_municipality  if Geocoded_municipality == "Hayward city"  using "DataV2/CoreLogic/output/currentAssess_master.dta", clear

save "DataV2/CoreLogic/output/currentAssess_hayward.dta", replace 