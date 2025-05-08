

*****************************AGGIORNARE CON CODICE DI MANU******************************


*Value for Chinashock piemonte is not the same as others 
local user = c(username)
display "`user'"

if ("`user'" == "user") {
    global filepath "C:\Users\user\Desktop\EEU" //Claudia
}
// Set directory
cd "$filepath"

global data "$filepath\data"

global output "$filepath\output"

*Download ESS Round 8
*Define path conditionally 
use "$data\ESS8e02_3\ESS8e02_3.dta" 

keep if cntry=="IT"
keep if regunit == 2 //see if needed
keep pspwght gndr agea edulvlb region prtvtbit sbsrnen

save "$data/ESS8.dta", replace

*Point a: Merge the ESS dataset with data on the China shock (region-specific average), based on the region of residence of each respondent.

use "$output/region_shocks_avg.dta"
keep if country=="Italy"
rename nuts2 region
save "$data\region_shocks_italy_avg.dta", replace

use "$data\ESS8.dta", clear
merge m:1 region using "$data\region_shocks_italy_avg.dta"
* There is one region that did not match

save "$data\merged_ESSCS.dta", replace

*Point b
gen nuts1 = substr(region, 1, 3)
encode nuts1, gen(NUTS1)

encode region, gen(region_id)

reg sbsrnen ChinaShock gndr agea i.edulvlb i.NUTS1 [pw=pspwght], vce(cluster region_id)

*********************************Risultati regressione sbagliato perchè chinashock sbagliato *********************************
ssc install estout
esttab using Q7b.tex, replace tex label se title("Regression of green attitude score on region-level China shock") keep(avg_china_shock)  

*I included age as a continuous variable and not dummies because I think it is what they do in the paper. To check

*Point c

ivregress 2sls sbsrnen (ChinaShock = USShock) gndr agea i.edulvlb i.NUTS1  [pw = pspwght], vce(cluster region_id) first



