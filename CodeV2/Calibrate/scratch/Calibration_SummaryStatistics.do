
*This file outputs summary statistics for both calibrated amenities and housing prices.

*baseline specification (change to get different summary statistics)
local bySkill "FALSE"
local pref "SG"

use "DataV2/Counterfactuals/Master_post_calibration_bySkill`bySkill'_pref_`pref'_amenities.dta", clear

*Summary statistics for hedonic prices and amenities.
label var price_regulated "Housing price (Regulated zone)"
label var price_unregulated "Housing price (Unregulated zone)"

label var Amenity_1 "log(Amenity value) 0-25k"
label var Amenity_2 "log(Amenity value) 25-50k"
label var Amenity_3 "log(Amenity value) 50-75k"
label var Amenity_4 "log(Amenity value) 75k-100k"
label var Amenity_5 "log(Amenity value) 100k-150k"
label var Amenity_6 "log(Amenity value) 150k-200k"
label var Amenity_7 "log(Amenity value) 200k+"

*Making sure summary statistics don't include zeros in amenity value
foreach var of varlist Amenity* {
	replace `var' = log(`var')
}

label var consumption_Val_1 "Consumption value 0-25k"
label var consumption_Val_2 "Consumption value 25-50k"
label var consumption_Val_3 "Consumption value 50-75k"
label var consumption_Val_4 "Consumption value 75k-100k"
label var consumption_Val_5 "Consumption value 100k-150k"
label var consumption_Val_6 "Consumption value 150k-200k"
label var consumption_Val_7 "Consumption value 200k+"


keep price_regulated price_unregulated Amenity* consumption_Val*
outreg2 using "DataV2/Counterfactuals/Calibration_Output/Calibration_summarystats.tex", replace tex(pretty) label nocon dec(2) sum(log) eqkeep(mean sd)

