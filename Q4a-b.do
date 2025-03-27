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

* Even when you run a regression on a subset, Stata's predict applies to the whole dataset
replace ln_TFP_OLS_13 = . if sector != 13

*Retrieve TFP from its logartihmic transformation
gen TFP_OLS_13= exp(ln_TFP_OLS_13)

*Estimating TFP with the Levinsohn-Petrin value added procedure 
xi: levpet ln_real_VA if sector==13, free(ln_L i.country i.year) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_13, omega
replace TFP_LP_13 = . if sector != 13
*Generate logarithmic transformation
gen ln_TFP_LP_13= ln(TFP_LP_13)

*Estimating TFP with the Wooldridge procedure
*Generating dummy variables for country and year 
capture confirm variable country_num
if _rc != 0 {
    encode country, gen(country_num)
}
capture drop year_dummy*
capture drop country_dummy*
tab year, gen(year_dummy)
tab country_num, gen(country_dummy)

xi:prodest ln_real_VA if sector==13, free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* country_dummy*)  method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded 
predict ln_TFP_WRDG_13, residuals 
replace ln_TFP_WRDG_13 = . if sector != 13
gen TFP_WRDG_13= exp(ln_TFP_WRDG_13)


*Repeating the same procedure for industry 29

reg ln_real_VA ln_L ln_real_K i.country_num i.year if sector==29
predict ln_TFP_OLS_29, residuals 
replace ln_TFP_OLS_29 = . if sector != 29
gen TFP_OLS_29= exp(ln_TFP_OLS_29)

*Estimating TFP with the Levinsohn-Petrin value added procedure
xi: levpet ln_real_VA if sector==29, free(ln_L i.country i.year) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_29, omega
replace TFP_LP_29 = . if sector != 29
gen ln_TFP_LP_29= ln(TFP_LP_29)

*Estimating TFP with the Wooldridge procedure
xi:prodest ln_real_VA if sector==29, free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* country_dummy*)  method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded 
predict ln_TFP_WRDG_29, residuals 
replace ln_TFP_WRDG_29 = . if sector != 29
gen TFP_WRDG_29= exp(ln_TFP_WRDG_29)

*Drop dummy variables 
capture drop year_dummy*
capture drop country_dummy*

log using "extremev.log", replace
sum TFP_OLS_13, d
sum TFP_LP_13, d
sum TFP_WRDG_13, d

sum TFP_OLS_29, d
sum TFP_LP_29, d
sum TFP_WRDG_29, d
log close

***Comment on the presence of "extreme" values in both industries. 

*Clear the TFP estimates from extreme values - Should I drop the observations? 
sum TFP_OLS_13, d
replace TFP_OLS_13 = . if !inrange(TFP_OLS_13, r(p1), r(p99))

sum TFP_LP_13, d
replace TFP_LP_13 = . if !inrange(TFP_LP_13, r(p1), r(p99))

sum TFP_WRDG_13, d
replace TFP_WRDG_13 = . if !inrange(TFP_WRDG_13, r(p1), r(p99))

sum TFP_OLS_29, d
replace TFP_OLS_29 = . if !inrange(TFP_OLS_29, r(p1), r(p99))

sum TFP_LP_29, d
replace TFP_LP_29 = . if !inrange(TFP_LP_29, r(p1), r(p99))

sum TFP_WRDG_29, d
replace TFP_WRDG_29 = . if !inrange(TFP_WRDG_29, r(p1), r(p99))

save "cleaned_sample.dta", replace

*Plot the kdensity of the TFP distribution and the kdensity of the logarithmic transformation of TFP in each industry.

kdensity TFP_OLS_13
save "$output\TFP_OLS_13", replace

kdensity TFP_LP_13
save "$output\TFP_LP_13", replace

kdensity TFP_WRDG_13
save "$output\TFP_WRDG_13", replace

tw kdensity ln_TFP_OLS_13 || kdensity ln_TFP_LP_13 || kdensity ln_TFP_WRDG_13
save "$output\TFP_13_compared", replace

kdensity TFP_OLS_29
save "$output\TFP_OLS_29", replace

kdensity TFP_LP_29
save "$output\TFP_LP_29", replace

kdensity TFP_WRDG_29
save "$output\TFP_WRDG-29", replace

tw kdensity ln_TFP_OLS_29 || kdensity ln_TFP_LP_29 || kdensity ln_TFP_WRDG_29
save "$output\TFP_29_compared", replace

*What do you notice? Are there any differences if you rely on the LP or WRDG procedure? Comment.

*------------Question 4.b--------------*

*************************Estimate TFP for Spain********************************
reg ln_real_VA ln_L ln_real_K i.year if country == "Spain"
predict ln_TFP_OLS_SP, residuals 
replace ln_TFP_OLS_SP = . if country != "Spain"
gen TFP_OLS_SP= exp(ln_TFP_OLS_SP)

xi: levpet ln_real_VA if country == "Spain", free(ln_L i.year) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_SP, omega
replace TFP_LP_SP = . if country != "Spain"
gen ln_TFP_LP_SP= ln(TFP_LP_SP)

*Generating dummy variables for year 
capture drop year_dummy*
tab year, gen(year_dummy)

xi:prodest ln_real_VA if country == "Spain", free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy*)  method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded 
predict ln_TFP_WRDG_SP, residuals 
replace ln_TFP_WRDG_SP = . if country != "Spain"
gen TFP_WRDG_SP= exp(ln_TFP_WRDG_SP)

*************************Estimate TFP for Italy********************************
reg ln_real_VA ln_L ln_real_K i.year if country == "Italy"
predict ln_TFP_OLS_IT, residuals 
replace ln_TFP_OLS_IT = . if country != "Italy"
gen TFP_OLS_IT = exp(ln_TFP_OLS_IT)

xi: levpet ln_real_VA if country == "Italy", free(ln_L i.year) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_IT, omega
replace TFP_LP_IT = . if country != "Italy"
gen ln_TFP_LP_IT = ln(TFP_LP_IT)

xi: prodest ln_real_VA if country == "Italy", free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy*) method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded 
predict ln_TFP_WRDG_IT, residuals 
replace ln_TFP_WRDG_IT = . if country != "Italy"
gen TFP_WRDG_IT = exp(ln_TFP_WRDG_IT)

*************************Estimate TFP for France********************************
reg ln_real_VA ln_L ln_real_K i.year if country == "France"
predict ln_TFP_OLS_FR, residuals 
replace ln_TFP_OLS_FR = . if country != "France"
gen TFP_OLS_FR = exp(ln_TFP_OLS_FR)

xi: levpet ln_real_VA if country == "France", free(ln_L i.year) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
predict TFP_LP_FR, omega
replace TFP_LP_FR = . if country != "France"
gen ln_TFP_LP_FR = ln(TFP_LP_FR)

xi: prodest ln_real_VA if country == "France", free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy*) method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded 
predict ln_TFP_WRDG_FR, residuals 
replace ln_TFP_WRDG_FR = . if country != "France"
gen TFP_WRDG_FR = exp(ln_TFP_WRDG_FR)

*Drop dummy variables 
capture drop year_dummy*

*Plot the TFP distribution for each country
tw kdensity ln_TFP_OLS_SP || kdensity ln_TFP_LP_SP || kdensity ln_TFP_WRDG_SP
save "$output\TFP_SP_compared", replace

tw kdensity ln_TFP_OLS_IT || kdensity ln_TFP_LP_IT || kdensity ln_TFP_WRDG_IT
save "$output\TFP_IT_compared", replace

tw kdensity ln_TFP_OLS_FR || kdensity ln_TFP_LP_FR || kdensity ln_TFP_WRDG_FR
save "$output\TFP_FR_compared", replace

*Are there any differences if you rely on the LP or WRDG procedure? Compare and comment.
