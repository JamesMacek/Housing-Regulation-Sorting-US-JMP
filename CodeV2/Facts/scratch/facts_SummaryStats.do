*This file produces summary statistics for data used in the empirical work.

*Import main dataset used in empirical work
use "DataV2/US_Data/Output/Constructed_Block_V2.dta", clear

*Average income
*Unit density restrictions
*land value density measure
*regulated housing unit share measure
*Income stringency measure

*Average lot sizes in all locations, all structures


*Replacing missing obs with zeros (imputed from merge_stringency.R)
replace UnitDensityRestriction_cl = 0 if missing(IncomeStringency_cl)
replace IncomeStringency_cl = 0 if missing(IncomeStringency_cl)

replace Average_income = log(Average_income)

label var Average_income "Log Average Income"
label var UnitDensityRestriction_cl "Measured Unit Density Restriction"
label var LandValueDensity_med "Land Value Density (\$/acre)"
label var regulated_housingUnit_share "Share of Housing Units in Regulated Structures (1-4 units per lot)"
label var IncomeStringency_cl "Regulatory Stringency Measure"


keep Average_income UnitDensityRestriction_cl LandValueDensity_med regulated_housingUnit_share IncomeStringency_cl
outreg2 using "DataV2/US_Data/Output/Facts_summarystats.tex", replace tex(pretty) label nocon dec(2) sum(log) eqkeep(mean sd N)

