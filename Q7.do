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

/*

 (e) Starting from the augmented ESS dataset used in the previous regressions, attach to the variable "party voted
 in last national election" the score for the "very general favorable references to underprivileged minority groups"
 available in the Manifesto Project for the election of 2013. You can search in the codebook the code for this
 score in the dataset. Please note that party names do not necessarily match exactly, and that ESS may have
 more parties than coded by the Manifesto Project. In the end, you should be able to match 9 parties.

*/

* cleaning Manifesto Project dataset
use "$data\MPDataset_MPDS2024a_stata14.dta", clear
keep if country==32 & floor(date/100) == 2013
describe per705

* Now let's properly prepare the MP dataset with the parties and per705
keep country date partyname per705

* Drop duplicates to ensure clean data
duplicates report partyname
duplicates drop partyname, force

* Create the prtvtbit variable for merging
gen prtvtbit = .
replace prtvtbit = 1 if partyname == "Democratic Party"
replace prtvtbit = 2 if partyname == "Left Ecology Freedom"
replace prtvtbit = 3 if partyname == "Civil Revolution"
replace prtvtbit = 4 if partyname == "Five Star Movement"
replace prtvtbit = 5 if partyname == "Civic Choice"
replace prtvtbit = 6 if partyname == "Union of the Center"
replace prtvtbit = 10 if partyname == "Brothers of Italy - National Centre-right"
replace prtvtbit = 8 if partyname == "People of Freedom"
replace prtvtbit = 9 if partyname == "Northern League"

* Keep only parties with valid prtvtbit values
drop if prtvtbit == .

* Display the per705 values for each party before merging
list partyname prtvtbit per705, clean noobs

* Save this as a temporary dataset with a clear name
save "$temp\mp_per705_values.dta", replace

* Now load the merged ESSCS dataset
use "$data\merged_ESSCS.dta", clear

* Check the structure before merging
describe prtvtbit

* Perform the merge with clear options
merge m:1 prtvtbit using "$temp\mp_per705_values.dta", keepusing(per705) generate(merge_per705)

* Label the per705 variable
label variable per705 "Favorable references to underprivileged minority groups"

* Verify the merge worked correctly
tab merge_per705
tab prtvtbit if merge_per705==3, summarize(per705)

* Save the final merged dataset with a clear name
save "$data\esscs_with_per705.dta", replace

* Check that per705 exists in the final dataset
describe per705



/*

(f) Regress (both OLS and IV, as above) the underprivileged minority groups score of the party voted against the region-level China shock, controlling for gender, age, dummies for levels of education, and dummies for Nuts regions at the 1 digit level. For this purpose, transform the score as follows: minority scorepc = log(.5 + zpc) with z being the variable for the "very general favorable references to underprivileged minority groups". Cluster the standard errors by Nuts region level 2. Be sure to use survey weights in the regressions. Comment.

*/


use "$data\esscs_with_per705.dta", clear

* Keep only complete cases
keep if !missing(per705, ChinaShock, USShock, pspwght, edulvlb, gndr, region, agea)

* Create dependent variable
gen minority_scorepc = log(0.5 + per705)

* Extract NUTS1 region from full NUTS2 region code
gen nuts1 = substr(region, 3, 1)
encode nuts1, gen(nuts1_code)

* OLS
eststo ols: reg minority_scorepc ChinaShock i.gndr c.agea i.edulvlb i.nuts1_code [pweight=pspwght], vce(cluster region)

* IV (2SLS)
eststo iv: ivregress 2sls minority_scorepc i.gndr c.agea i.edulvlb i.nuts1_code (ChinaShock = USShock) [pweight=pspwght], vce(cluster region)

* rtf file
esttab ols iv using summary_results.rtf, replace se star(* 0.10 ** 0.05 *** 0.01) b(%9.2f) se(%9.2f) scalars(N) label keep(ChinaShock 2.gndr agea 113.edulvlb 313.edulvlb 520.edulvlb 710.edulvlb 800.edulvlb) coeflabels(ChinaShock "China Shock" 2.gndr "Female" agea "Age" 113.edulvlb "Primary education (ISCED 1)" 313.edulvlb "Upper secondary (ISCED 3)" 520.edulvlb "Adv. vocational (ISCED 5B)" 710.edulvlb "Master's (ISCED 5A long)" 800.edulvlb "Doctoral degree (ISCED 6)") addnote("Controls for remaining education levels and NUTS1 region fixed effects included.")

* Stata output window
esttab ols iv, se star(* 0.10 ** 0.05 *** 0.01) b(%9.2f) se(%9.2f) label keep(ChinaShock 2.gndr agea 113.edulvlb 313.edulvlb 520.edulvlb 710.edulvlb 800.edulvlb) coeflabels(ChinaShock "China Shock" 2.gndr "Female" agea "Age" 113.edulvlb "Primary education (ISCED 1)" 313.edulvlb "Upper secondary (ISCED 3)" 520.edulvlb "Adv. vocational (ISCED 5B)" 710.edulvlb "Master's (ISCED 5A long)" 800.edulvlb "Doctoral degree (ISCED 6)") addnote("Controls for remaining education levels and NUTS1 region fixed effects included.")

