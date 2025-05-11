clear all
set more off

/* Gets user name */
local user = c(username)
display "`user'"

/* Stores filepath conditionally */
if ("`user'" == "Jovana") {
    global filepath `"C:\Users\Jovana\OneDrive - Università Commerciale Luigi Bocconi\Desktop\Master\Y1S2\European Integration\Take Home\data"'
}

if ("`user'" == "user") {
    global filepath "C:\Users\user\Desktop\EEU" //Claudia
}
// Set directory
cd "$filepath"

* Defines paths and folders 
global data "$filepath\data"
global output "$filepath\output"
global temp "$filepath\output\temp"


*Download ESS Round 8
use "$data\ESS8e02_3\ESS8e02_3.dta" 

keep if cntry=="IT"
keep pspwght gndr agea edulvlb region prtvtbit sbsrnen

save "$data/ESS8.dta", replace

*Point a: Merge the ESS dataset with data on the China shock (region-specific average), based on the region of residence of each respondent.

use "$output/region_shocks_avg.dta"
keep if country=="Italy"
rename nuts2 region
save "$data\region_shocks_italy_avg.dta", replace

use "$data\ESS8.dta", clear
merge m:1 region using "$data\region_shocks_italy_avg.dta"
drop if _merge == 2

save "$data\merged_ESSCS.dta", replace

*Point b
gen nuts1 = substr(region, 1, 3)
encode nuts1, gen(NUTS1)

encode region, gen(region_id)

log using "Regressions7.log", replace
reg sbsrnen ChinaShock gndr agea i.edulvlb i.NUTS1 [pw=pspwght], vce(cluster region_id)

*ssc install estout
*esttab using Q7b.tex, replace tex label se title("Regression of green attitude score on region-level China shock") keep(avg_china_shock)  

*Point c
ivregress 2sls sbsrnen (ChinaShock = USShock) gndr agea i.edulvlb i.NUTS1  [pw = pspwght], vce(cluster region_id) first
log close


