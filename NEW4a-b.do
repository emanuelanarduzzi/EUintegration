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

*----------------------------------------------------------------*
**************************---QUESTION 4---************************
*----------------------------------------------------------------*

*------------Question 4.a--------------*

use "C:\Users\user\Desktop\EEU\TH\Dataset4.dta", clear 

*Estimating TFP with the Levinsohn-Petrin value added procedure 
*Sector 13
xi:levpet ln_real_VA if sector==13, free(ln_L i.year i.country) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_13, omega

* Sector 29
xi:levpet ln_real_VA if sector==29, free(ln_L i.year i.country) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_29, omega

*Estimating TFP with the Wooldridge procedure
xi:prodest ln_real_VA if sector==13, free(ln_L) state(ln_real_K) proxy(ln_real_M) method(wrdg) id(id_n) t(year) valueadded 
predict ln_TFP_WRDG_13, residuals 
gen TFP_WRDG_13= exp(ln_TFP_WRDG_13)

* Sector 29
xi:prodest ln_real_VA if sector==29, free(ln_L) state(ln_real_K) proxy(ln_real_M) method(wrdg) id(id_n) t(year) valueadded 
predict ln_TFP_WRDG_29, residuals 
gen TFP_WRDG_29= exp(ln_TFP_WRDG_29)

log using "extremev.log", replace
sum TFP_LP_13, d
sum TFP_WRDG_13, d

sum TFP_LP_29, d
sum TFP_WRDG_29, d
log close

***Comment on the presence of "extreme" values in both industries. 

*Clear the TFP estimates from extreme values 

summarize TFP_LP_13, d
replace TFP_LP_13 = . if  !inrange(TFP_LP_13, r(p1), r(p99))

summarize TFP_WRDG_13, d
replace TFP_WRDG_13 = . if !inrange(TFP_WRDG_13, r(p1), r(p99))

summarize TFP_LP_29, d
replace TFP_LP_29 = . if !inrange(TFP_LP_29, r(p1), r(p99))

summarize TFP_WRDG_29, d
replace TFP_WRDG_29 = . if!inrange(TFP_WRDG_29, r(p1), r(p99))

log using "extremev_cleaned.log", replace
sum TFP_LP_13, d
sum TFP_WRDG_13, d

sum TFP_LP_29, d
sum TFP_WRDG_29, d
log close 

gen TFP_LP = .
replace TFP_LP = TFP_LP_13 if sector == 13
replace TFP_LP = TFP_LP_29 if sector == 29

gen TFP_WRDG = .
replace TFP_WRDG = TFP_WRDG_13 if sector == 13
replace TFP_WRDG = TFP_WRDG_29 if sector == 29

gen ln_TFP_LP = ln(TFP_LP)
gen ln_TFP_WRDG = ln(TFP_WRDG)

save "cleaned_sample.dta", replace

*Plot the kdensity of the TFP distribution and the kdensity of the logarithmic transformation of TFP in each industry.
*Sector 13 TFP
twoway (kdensity TFP_LP if sector==13, lcolor(blue)) ///
       (kdensity TFP_WRDG if sector==13, lcolor(sienna)), ///
       title("TFP Density Industry 13") ///
       legend(label(1 "LP") label(2 "WRDG")) ///
       xtitle("TFP") ytitle("Density")
graph export $output/TFP_13.png, replace

*Sector 29 TFP	   
twoway (kdensity TFP_LP if sector==29, lcolor(blue)) ///
       (kdensity TFP_WRDG if sector==29, lcolor(sienna)), ///
       title("TFP Density Industry 29") ///
       legend(label(1 "LP") label(2 "WRDG")) ///
       xtitle("TFP") ytitle("Density")
graph export $output/TFP_29.png, replace

*Sector 13 Log TFP
twoway (kdensity ln_TFP_LP if sector==13, lcolor(blue)) ///
       (kdensity ln_TFP_WRDG if sector==13, lcolor(sienna)), ///
       title("Log TFP Density Industry 13") ///
       legend(label(1 "LP") label(2 "WRDG")) ///
       xtitle("Log TFP") ytitle("Density")
graph export $output/lnTFP_13.png, replace

twoway (kdensity ln_TFP_LP if sector==29, lcolor(blue)) ///
       (kdensity ln_TFP_WRDG if sector==29, lcolor(sienna)), ///
       title("Log TFP Density Industry 29") ///
       legend(label(1 "LP") label(2 "WRDG")) ///
       xtitle("Log TFP") ytitle("Density")
graph export $output/lnTFP_29.png, replace

*Plot the TFP distribution for each country.	   
*Spain
twoway (kdensity TFP_LP if country=="Spain", lcolor(blue)) ///
       (kdensity TFP_WRDG if country=="Spain", lcolor(sienna)), ///
       title("TFP Density for Spain") ///
       legend(label(1 "LP") label(2 "WRDG")) ///
       xtitle("TFP") ytitle("Density")
graph export $output/TFP_SP.png, replace

*Italy
twoway (kdensity TFP_LP if country=="Italy", lcolor(blue)) ///
       (kdensity TFP_WRDG if country=="Italy", lcolor(sienna)), ///
       title("TFP Density for Italy") ///
       legend(label(1 "LP") label(2 "WRDG")) ///
       xtitle("TFP") ytitle("Density")
graph export $output/TFP_IT.png, replace
	  
*France
twoway (kdensity TFP_LP if country=="France", lcolor(blue)) ///
       (kdensity TFP_WRDG if country=="France", lcolor(sienna)), ///
       title("TFP Density for France") ///
       legend(label(1 "LP") label(2 "WRDG")) ///
       xtitle("TFP") ytitle("Density")
graph export $output/TFP_FR.png, replace