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
replace TFP_LP_13 = . if sector==29
*Generate logarithmic transformation
gen ln_TFP_LP_13= ln(TFP_LP_13)

* Sector 29
xi:levpet ln_real_VA if sector==29, free(ln_L i.year i.country) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_29, omega
replace TFP_LP_29 = . if sector==13
gen ln_TFP_LP_29= ln(TFP_LP_29)

*Estimating TFP with the Wooldridge procedure
xi:prodest ln_real_VA if sector==13, free(ln_L) state(ln_real_K) proxy(ln_real_M) method(wrdg) id(id_n) t(year) valueadded 
predict ln_TFP_WRDG_13, residuals 
replace ln_TFP_WRDG_13 = . if sector==29
*We do not put year dummy because Woolridge automatically accounts for time-varying differences
gen TFP_WRDG_13= exp(ln_TFP_WRDG_13)

* Sector 29
xi:prodest ln_real_VA if sector==29, free(ln_L) state(ln_real_K) proxy(ln_real_M) method(wrdg) id(id_n) t(year) valueadded 
predict ln_TFP_WRDG_29, residuals 
replace ln_TFP_WRDG_29 = . if sector==13
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

save "cleaned_sample.dta", replace

log using "extremev_cleaned.log", replace
sum TFP_LP_13, d
sum TFP_WRDG_13, d

sum TFP_LP_29, d
sum TFP_WRDG_29, d
log close 

*Plot the kdensity of the TFP distribution and the kdensity of the logarithmic transformation of TFP in each industry.

twoway (kdensity TFP_WRDG_13, lcolor(sienna)) || (kdensity TFP_LP_13, lcolor(blue)), title("TFP Density Industry 13") legend(label(1 "WRDG") label(2 "LP"))
graph export $output/TFP_13.png, replace

twoway (kdensity ln_TFP_WRDG_13, lcolor(sienna)) || (kdensity ln_TFP_LP_13, lcolor(blue)), title("Log TFP vDensity Industry 13") legend(label(1 "WRDG") label(2 "LP")) 
graph export $output/lnTFP_13.png, replace

twoway (kdensity TFP_WRDG_29, lcolor(sienna)) || (kdensity TFP_LP_29, lcolor(blue)), title("TFP Density Industry 29") legend(label(1 "WRDG") label(2 "LP")) 
graph export $output/TFP_29.png, replace

twoway (kdensity ln_TFP_WRDG_29, lcolor(sienna)) || (kdensity ln_TFP_LP_29, lcolor(blue)), title("Log TFP Density Industry 29") legend(label(1 "WRDG") label(2 "LP")) 
graph export $output/lnTFP_29.png, replace

*What do you notice? Are there any differences if you rely on the LP or WRDG procedure? Comment.

*------------Question 4.b--------------*

*************************Estimate TFP with LP********************************
*** Using the original sample not cleaned the results are the same 
use "C:\Users\user\Desktop\EEU\cleaned_sample.dta", clear

*Estimating TFP distribution for Spain
xi:levpet ln_real_VA if country == "Spain", free(ln_L i.year i.country) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_SP if e(sample), omega
replace TFP_LP_SP = . if country != "Spain"
gen ln_TFP_LP_SP= ln(TFP_LP_SP)
sum TFP_LP_SP, d

*Estimating TFP distribution for Italy
xi:levpet ln_real_VA if country == "Italy", free(ln_L i.year i.country) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_IT if e(sample), omega
replace TFP_LP_IT = . if country != "Italy"
gen ln_TFP_LP_IT = ln(TFP_LP_IT)
sum TFP_LP_IT, d

*Estimating TFP distribution for France
xi:levpet ln_real_VA if country == "France", free(ln_L i.year i.country) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_FR if e(sample), omega
replace TFP_LP_FR = . if country != "France"
gen ln_TFP_LP_FR = ln(TFP_LP_FR)
sum TFP_LP_FR, d


*************************Estimate TFP with WRDG********************************
capture drop country_dummy*
tab country, gen(country_dummy)

*Estimating TFP distribution for Spain
xi:prodest ln_real_VA if country == "Spain", free(ln_L) state(ln_real_K) proxy(ln_real_M) method(wrdg) id(id_n) t(year) valueadded 
predict ln_TFP_WRDG_SP if country == "Spain", residuals 
gen TFP_WRDG_SP= exp(ln_TFP_WRDG_SP)
sum TFP_WRDG_SP, d

*Estimating TFP distribution for Italy
xi:prodest ln_real_VA if country == "Italy", free(ln_L) state(ln_real_K) proxy(ln_real_M) method(wrdg) id(id_n) t(year) valueadded 
predict ln_TFP_WRDG_IT if country == "Italy", residuals 
gen TFP_WRDG_IT = exp(ln_TFP_WRDG_IT)
sum TFP_WRDG_IT, d

*Estimating TFP distribution for France
xi:prodest ln_real_VA if country == "France", free(ln_L) state(ln_real_K) proxy(ln_real_M) method(wrdg) id(id_n) t(year) valueadded 
predict ln_TFP_WRDG_FR if country == "France", residuals 
gen TFP_WRDG_FR = exp(ln_TFP_WRDG_FR)
sum TFP_WRDG_FR, d


*Clean outliers 
summarize TFP_LP_SP, d
replace TFP_LP_SP = . if  !inrange(TFP_LP_SP, r(p1), r(p99))
summarize TFP_WRDG_SP, d
replace TFP_WRDG_SP = . if !inrange(TFP_WRDG_SP, r(p1), r(p99))

summarize TFP_LP_IT, d
replace TFP_LP_IT = . if  !inrange(TFP_LP_IT, r(p1), r(p99))
summarize TFP_WRDG_IT, d
replace TFP_WRDG_IT = . if !inrange(TFP_WRDG_IT, r(p1), r(p99))

summarize TFP_LP_FR, d
replace TFP_LP_FR = . if  !inrange(TFP_LP_FR, r(p1), r(p99))
summarize TFP_WRDG_FR, d
replace TFP_WRDG_FR = . if !inrange(TFP_WRDG_FR, r(p1), r(p99))

*Plot the TFP distribution for each country

*Spain
twoway (kdensity TFP_WRDG_SP, lcolor(sienna)) || (kdensity TFP_LP_SP, lcolor(blue)), title("TFP Density Spain") legend(label(1 "WRDG") label(2 "LP")) 
graph export $output/TFP_SP.png, replace

twoway (kdensity ln_TFP_WRDG_SP, lcolor(sienna)) || (kdensity ln_TFP_WRDG_SP, lcolor(blue)), title("Log TFP Density Spain") legend(label(1 "WRDG") label(2 "LP")) 
graph export $output/lnTFP_SP.png, replace

*Italy
twoway (kdensity TFP_WRDG_IT, lcolor(sienna)) || (kdensity TFP_LP_IT, lcolor(blue)), title("TFP Density Italy") legend(label(1 "WRDG") label(2 "LP")) 
graph export $output/TFP_IT.png, replace

twoway (kdensity ln_TFP_WRDG_IT, lcolor(sienna)) || (kdensity ln_TFP_LP_IT, lcolor(blue)), title("Log TFP Density Italy") legend(label(1 "WRDG") label(2 "LP")) 
graph export $output/lnTFP_IT.png, replace

*France
twoway (kdensity TFP_WRDG_FR, lcolor(sienna))||(kdensity TFP_LP_FR, lcolor(blue)), title("TFP Density France") legend(label(1 "WRDG") label(2 "LP")) 
graph export $output/TFP_FR.png, replace

twoway (kdensity ln_TFP_WRDG_FR, lcolor(sienna)) || (kdensity ln_TFP_LP_FR, lcolor(blue)), title("Log TFP Density France") legend(label(1 "WRDG") label(2 "LP")) 
graph export $output/lnTFP_FR.png, replace

*Are there any differences if you rely on the LP or WRDG procedure? Compare and comment.
