local user = c(username)
display "`user'"

if ("`user'" == "user") {
    global filepath "C:\Users\user\Desktop\EEU" //Claudia
}
// Set directory
cd "$filepath"

global data "$filepath\data"

*Download ESS Round 8
*Define path conditionally 
use "$data\ESS8e02_3\ESS8e02_3.dta" 

keep if cntry=="IT"
keep if regunit == 2
keep pspwght gndr agea edulvlb region prtvtbit sbsrnen

save "$data/ESS8.dta", replace

use "$data\MPDataset_MPDS2024a_stata14.dta"
keep if country==32
save "$data/MPItaly.dta", replace

*Point a: Merge the ESS dataset with data on the China shock (region-specific average), based on the region of residence of each respondent.

use "$data\china_shock_crosssection.dta"
keep if country=="Italy"
rename nuts2 region
save "$data\china_shock_italy.dta", replace

use "$data\ESS8.dta", clear
merge m:1 region using "$data\china_shock_italy.dta"
* There is one region that did not match

save "$data\merged_ESSCS.dta", replace

*Point b

gen nuts1 = substr(region, 1, 3)
encode nuts1, gen(NUTS1)

encode region, gen(region_id)

reg sbsrnen avg_china_shock i.gndr agea i.edulvlb i.NUTS1 [pw=pspwght], vce(cluster region_id)
*I included age as a continuous variable and not dummies because I think it is what they do in the paper. To check

*Point c: To check, I think this is strange
*The huge magnitude almost certainly reflects the scale of the shock variable

reg avg_china_shock avg_iv_china_shock 

predict avg_CS 

reg sbsrnen avg_CS i.gndr agea i.edulvlb i.NUTS1 [pw=pspwght], vce(cluster region_id) 
************************************************************************
ivregress 2sls                                                   ///
    sbsrnen i.gndr agea i.edulvlb i.NUTS1                       ///
    (avg_china_shock = avg_iv_china_shock)                     ///
    [pw = pspwght], vce(cluster region_id)

estat firststage   // strength of instrument (F-stat)


