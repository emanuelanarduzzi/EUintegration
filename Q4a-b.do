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
    global filepath "/Users/user/Desktop/STATA/eu/" //emanuela's file path
}

if ("`user'" == "user") {
    global filepath "C:\Users\user\Desktop\EEU" //Claudia
}
// Set directory
cd "$filepath"

* Defines paths and folders 
global data "$filepath"
global output "$filepath\output"
global temp "$filepath\output\temp"

*----------------------------------------------------------------*
**************************---QUESTION 4---************************
*----------------------------------------------------------------*

*------------Question 4.a--------------*
*Estimating TFP of firms in industry 13 as the residuals of an OLS regression of value added on inputs, with fixed effects for country and year
reg ln_real_VA ln_L ln_real_K i.country_num i.year if sector==13
predict ln_TFP_OLS_13, residuals 
*Retrieve TFP from its logartihmic transformation
gen TFP_OLS_13= exp(ln_TFP_OLS_13)

*Repeating the same procedure for industry 29 
reg ln_real_VA ln_L ln_real_K i.country_num i.year if sector==29
predict ln_TFP_OLS_29, residuals 
gen TFP_OLS_29= exp(ln_TFP_OLS_29)

*Combining industry-specific TFP
gen TFP_OLS = .
replace TFP_OLS=TFP_OLS_13 if sector==13
replace TFP_OLS=TFP_OLS_29 if sector==29
gen ln_TFP_OLS=ln(TFP_OLS)

drop ln_TFP_OLS_13 ln_TFP_OLS_29 TFP_OLS_13 TFP_OLS_29

*Estimating TFP with the Levinsohn-Petrin value added procedure 
*Sector 13
levpet ln_real_VA if sector==13, free(ln_L year_dummy* country_dummy*) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_13, omega
*Generate logarithmic transformation
gen ln_TFP_LP_13= ln(TFP_LP_13)

* Sector 29
levpet ln_real_VA if sector==29, free(ln_L year_dummy* country_dummy*) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_29, omega
gen ln_TFP_LP_29= ln(TFP_LP_29)

*Combining industry-specific TFP
gen TFP_LP = .
replace TFP_LP=TFP_LP_13 if sector==13
replace TFP_LP=TFP_LP_29 if sector==29
gen ln_TFP_LP=ln(TFP_LP)

drop ln_TFP_LP_13 ln_TFP_LP_29 TFP_LP_13 TFP_LP_29

*Estimating TFP with the Wooldridge procedure
xi:prodest ln_real_VA if sector==13, free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* country_dummy*)  method(wrdg) id(id_n) t(year) valueadded 
predict ln_TFP_WRDG_13, residuals 
gen TFP_WRDG_13= exp(ln_TFP_WRDG_13)

* Sector 29
xi:prodest ln_real_VA if sector==29, free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* country_dummy*)  method(wrdg) id(id_n) t(year) valueadded 
predict ln_TFP_WRDG_29, residuals 
gen TFP_WRDG_29= exp(ln_TFP_WRDG_29)

*Combining industry-specific TFP
gen TFP_WRDG = .
replace TFP_WRDG=TFP_WRDG_13 if sector==13
replace TFP_WRDG=TFP_WRDG_29 if sector==29
gen ln_TFP_WRDG=ln(TFP_WRDG)

drop ln_TFP_WRDG_13 ln_TFP_WRDG_29 TFP_WRDG_13 TFP_WRDG_29

*Drop county dummy
capture drop country_dummy*

log using "extremev.log", replace
sum TFP_OLS if sector==13, d
sum TFP_LP if sector==13 , d
sum TFP_WRDG if sector==13, d

sum TFP_OLS if sector==29, d
sum TFP_LP if sector==29, d
sum TFP_WRDG if sector==29, d
log close

***Comment on the presence of "extreme" values in both industries. 

*Clear the TFP estimates from extreme values 

summarize TFP_OLS if sector == 13, d
replace TFP_OLS = . if sector == 13 & !inrange(TFP_OLS, r(p1), r(p99))

summarize TFP_LP if sector == 13, d
replace TFP_LP = . if sector == 13 & !inrange(TFP_LP, r(p1), r(p99))

summarize TFP_WRDG, d
replace TFP_WRDG = . if sector ==13 & !inrange(TFP_WRDG, r(p1), r(p99))

summarize TFP_OLS if sector == 29, d
replace TFP_OLS = . if sector ==29 & !inrange(TFP_OLS, r(p1), r(p99))

summarize TFP_LP if sector == 29, d
replace TFP_LP = . if sector ==29 & !inrange(TFP_LP, r(p1), r(p99))

summarize TFP_WRDG, d
replace TFP_WRDG = . if sector ==29 & !inrange(TFP_WRDG, r(p1), r(p99))

save "cleaned_sample.dta", replace

*Plot the kdensity of the TFP distribution and the kdensity of the logarithmic transformation of TFP in each industry.

twoway (kdensity TFP_OLS if sector==13, lcolor(green))|| (kdensity TFP_WRDG if sector==13, lcolor(sienna)) || (kdensity TFP_LP if sector==13, lcolor(blue)), title("TFP Density Industry 13") legend(label(1 "OLS") label(2 "WRDG") label(3 "LP"))
graph export $output/TFP_13.png, replace

twoway (kdensity ln_TFP_OLS if sector==13, lcolor(green))|| (kdensity ln_TFP_WRDG if sector==13, lcolor(sienna)) || (kdensity ln_TFP_LP if sector==13, lcolor(blue)), title("Log TFP Density Industry 13") legend(label(1 "OLS") label(2 "WRDG") label(3 "LP")) 
graph export $output/lnTFP_13.png, replace

twoway (kdensity TFP_OLS if sector==29, lcolor(green))|| (kdensity TFP_WRDG if sector==29, lcolor(sienna)) || (kdensity TFP_LP if sector==29, lcolor(blue)), title("TFP Density Industry 29") legend(label(1 "OLS") label(2 "WRDG") label(3 "LP")) 
graph export $output/TFP_29.png, replace

twoway (kdensity ln_TFP_OLS if sector==29, lcolor(green))|| (kdensity ln_TFP_WRDG if sector==29, lcolor(sienna)) || (kdensity ln_TFP_LP if sector==29, lcolor(blue)), title("Log TFP Density Industry 29") legend(label(1 "OLS") label(2 "WRDG") label(3 "LP")) 
graph export $output/lnTFP_29.png, replace

*What do you notice? Are there any differences if you rely on the LP or WRDG procedure? Comment.

*------------Question 4.b--------------*

*************************Estimate TFP with OLS********************************
*Question: is it correct to exclude the country dummy? Should I include a sectoral dummy?
*Estimating TFP with OLS for Spain
reg ln_real_VA ln_L ln_real_K i.year i.sector if country == "Spain"
predict ln_TFP_OLS_SP, residuals
gen TFP_OLS_SP= exp(ln_TFP_OLS_SP)

*Repeating the same procedure for Italy
reg ln_real_VA ln_L ln_real_K i.year i.sector if country == "Italy"
predict ln_TFP_OLS_IT, residuals 
gen TFP_OLS_IT = exp(ln_TFP_OLS_IT)

*Repeating the same procedure for France
reg ln_real_VA ln_L ln_real_K i.year i.sector if country == "France"
predict ln_TFP_OLS_FR, residuals 
gen TFP_OLS_FR = exp(ln_TFP_OLS_FR)

*Combining country-specific TFP
gen TFP_OLS_Country = .
replace TFP_OLS_Country = TFP_OLS_SP if country=="Spain"
replace TFP_OLS_Country = TFP_OLS_IT if country=="Italy"
replace TFP_OLS_Country = TFP_OLS_FR if country=="Italy"
gen ln_TFP_OLS_Country=ln(TFP_OLS_Country)

drop ln_TFP_OLS_SP ln_TFP_OLS_IT ln_TFP_OLS_FR TFP_OLS_SP TFP_OLS_IT TFP_OLS_FR 

*************************Estimate TFP with LP********************************
*Should we include the dummies in the free parameters?
xi: levpet ln_real_VA if country == "Spain", free(ln_L i.year i.sector) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_SP, omega
gen ln_TFP_LP_SP= ln(TFP_LP_SP)

xi: levpet ln_real_VA if country == "Italy", free(ln_L i.year i.sector) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_IT, omega
gen ln_TFP_LP_IT = ln(TFP_LP_IT)

xi: levpet ln_real_VA if country == "France", free(ln_L i.year i.sector) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_FR, omega
gen ln_TFP_LP_FR = ln(TFP_LP_FR)

gen TFP_LP_Country = .
replace TFP_LP_Country = TFP_LP_SP if country=="Spain"
replace TFP_LP_Country = TFP_LP_IT if country=="Italy"
replace TFP_LP_Country = TFP_LP_FR if country=="Italy"
gen ln_TFP_LP_Country=ln(TFP_LP_Country)

drop ln_TFP_LP_SP ln_TFP_LP_IT ln_TFP_LP_FR TFP_LP_SP TFP_LP_IT TFP_LP_FR

*************************Estimate TFP with WRDG********************************
tab sector, gen (sector_dummy)
tab year, gen(year_dummy)

xi:prodest ln_real_VA if country == "Spain", free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* sector_dummy*) method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded 
predict ln_TFP_WRDG_SP, residuals 
gen TFP_WRDG_SP= exp(ln_TFP_WRDG_SP)

xi: prodest ln_real_VA if country == "Italy", free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* sector_dummy*) method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded 
predict ln_TFP_WRDG_IT, residuals 
gen TFP_WRDG_IT = exp(ln_TFP_WRDG_IT)

xi: prodest ln_real_VA if country == "France", free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* sector_dummy*) method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded 
predict ln_TFP_WRDG_FR, residuals 
gen TFP_WRDG_FR = exp(ln_TFP_WRDG_FR)

gen TFP_WRDG_Country = .
replace TFP_WRDG_Country = TFP_WRDG_SP if country=="Spain"
replace TFP_WRDG_Country = TFP_WRDG_IT if country=="Italy"
replace TFP_WRDG_Country = TFP_WRDG_FR if country=="France"
gen ln_TFP_WRDG_Country = ln(TFP_WRDG_Country)

drop ln_TFP_WRDG_SP ln_TFP_WRDG_IT ln_TFP_WRDG_FR TFP_WRDG_SP TFP_WRDG_IT TFP_WRDG_FR

*Drop dummy variables 
capture drop sector_dummy*

*Plot the TFP distribution for each country

twoway (kdensity TFP_OLS_Country if country=="Spain", lcolor(green))|| (kdensity TFP_WRDG_Country if country=="Spain", lcolor(sienna)) || (kdensity TFP_LP_Country if country=="Spain", lcolor(blue)), title("TFP Density Spain") legend(label(1 "OLS") label(2 "WRDG") label(3 "LP")) 
graph export $output/TFP_SP.png, replace

twoway (kdensity ln_TFP_OLS_Country if country=="Spain", lcolor(green))|| (kdensity ln_TFP_WRDG_Country if country=="Spain", lcolor(sienna)) || (kdensity ln_TFP_LP_Country if country=="Spain", lcolor(blue)), title("Log TFP Density Spain") legend(label(1 "OLS") label(2 "WRDG") label(3 "LP")) 
graph export $output/lnTFP_SP.png, replace

twoway (kdensity TFP_OLS_Country if country=="Italy",  lcolor(green)) || (kdensity TFP_WRDG_Country if country=="Italy", lcolor(sienna)) || (kdensity TFP_LP_Country   if country=="Italy", lcolor(blue)), title("TFP Density Italy") legend(label(1 "OLS") label(2 "WRDG") label(3 "LP"))
graph export $output/TFP_IT.png, replace

twoway (kdensity ln_TFP_OLS_Country if country=="Italy",  lcolor(green)) || (kdensity ln_TFP_WRDG_Country if country=="Italy", lcolor(sienna)) || (kdensity ln_TFP_LP_Country   if country=="Italy", lcolor(blue)), title("Log TFP Density Italy") legend(label(1 "OLS") label(2 "WRDG") label(3 "LP"))
graph export $output/lnTFP_IT.png, replace

*------------------------does not work--------------------
twoway (kdensity TFP_OLS_Country if country=="France",  lcolor(green))||(kdensity TFP_WRDG_Country if country=="France", lcolor(sienna))||(kdensity TFP_LP_Country if country=="France", lcolor(blue)), title("TFP Density France") legend(label(1 "OLS") label(2 "WRDG") label(3 "LP"))
graph export $output/TFP_FR.png, replace

twoway (kdensity ln_TFP_OLD_Country if country=="France",  lcolor(green))||(kdensity ln_TFP_WRDG_Country if country=="France", lcolor(sienna))||(kdensity ln_TFP_LP_Country if country=="France", lcolor(blue)), title("Log TFP Density France") legend(label(1 "OLS") label(2 "WRDG") label(3 "LP"))
graph export $output/lnTFP_FR.png, replace


*Are there any differences if you rely on the LP or WRDG procedure? Compare and comment.
