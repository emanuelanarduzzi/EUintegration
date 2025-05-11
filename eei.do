*******************************
*------- Assignment -------*
*---------- GROUP n ----------*
* nome cognome - student id
* nome cognome - student id
* Jovana Mrdalj - 3280610
* Emanuela Narduzzi - 3173310
*******************************
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
    global filepath "/Users/user/Desktop/STATA/eu" //emanuela's file path
}

*if ("`user'" == "user") {
    *global filepath "C:\Users\user\Desktop\EEU" //Claudia
*}

// Set directory
cd "$filepath"

* Defines paths and folders 
global data "$filepath"
global output "$filepath\output"
global temp "$filepath\output\temp"


*----------------------------------------------------------------*
**************************---QUESTION 1---************************
*----------------------------------------------------------------*


*------------Question 1.a--------------*

use "$data\EEI_TH_2025.dta", clear

* Keeping only French firms in 2007, sectors 13 and 29, Nord–Pas de Calais (FR30). Note: FR30 retrieved from knoema.com
preserve
keep if country == "France" & year == 2007 & inlist(sector, 13, 29) & nuts2 == "FR30"
collapse (count) id_n (mean) K sales L real_VA real_K real_sales, by(sector)

* Start a new Word document
putdocx begin
putdocx paragraph
putdocx text ("Descriptive Statistics of Firms in Textiles (Sector 13) and Motor Vehicles (Sector 29) in Nord–Pas de Calais (FR30), 2007"), bold linebreak
putdocx table table2007 = data(sector id_n K sales L real_VA real_K real_sales), varnames
restore

*------------Question 1.b--------------*

keep if country == "France" & year == 2017 & inlist(sector, 13, 29) & nuts2 == "FR30"
collapse (count) id_n (mean) K sales L real_VA real_K real_sales, by(sector)

* Add a paragraph break and second table
putdocx paragraph
putdocx text ("Descriptive Statistics of Firms in Textiles (Sector 13) and Motor Vehicles (Sector 29) in Nord–Pas de Calais (FR30), 2017"), bold linebreak
putdocx table table2017 = data(sector id_n K sales L real_VA real_K real_sales), varnames

* Save the final document
putdocx save "$output\descriptive_table_sector13_29_FR30.docx", replace

*--------------------------------------*
*----------------------------------------------------------------*
**************************---QUESTION 2---************************
*----------------------------------------------------------------*

*net install st0060, from("http://www.stata-journal.com/software/sj4-2/")
*ssc install outreg2
*net install prodest, from("http://fmwww.bc.edu/RePEc/bocode/p")

*------------Question 2.a--------------*

use "$filepath\EEI_TH_2025.dta", clear
keep if inlist(sector, 13, 29)

*drop neg values
foreach var in real_sales real_M real_K L{
        drop if  `var'<=0
        }

*create logarithms of continuous variables (on deflated values)
foreach var in real_sales real_M real_K L real_VA {
        gen ln_`var'=ln(`var')
		}
		
	
*net install st0060, from("http:\\www.stata-journal.com\software\sj4-2\")
*ssc install outreg2
*net install prodest, from("http:\\fmwww.bc.edu\RePEc\bocode\p")


*** Consider now all the three countries. Estimate for the two industries available in NACE Rev. 2 2-digit format the production function coefficients, by using standard OLS, the Wooldridge (WRDG) and the Levinsohn & Petrin (LP) procedure. How do you treat the fact that data come from different countries in different years in the productivity estimation?


xtset id_n year


matrix results = J(6, 4, .)
matrix colnames results = "Sector" "L_coef" "K_coef" "M_coef"
local row = 1

foreach s in 13 29 {
    preserve
    keep if sector == `s'

   *OLS REGRESSION
    xi:reg ln_real_VA ln_L ln_real_K i.year i.country
    
    matrix results[`row', 1] = `s'
    matrix results[`row', 2] = _b[ln_L]
    matrix results[`row', 3] = _b[ln_real_K]
    matrix results[`row', 4] = 0
    local row = `row' + 1
    
    estimates store OLS_`s'

    *WOOLRIDGE REGRESSION
    xi:prodest ln_real_VA, free(ln_L) state(ln_real_K) proxy(ln_real_M) method(wrdg) id(id_n) t(year) valueadded 

    matrix results[`row', 1] = `s'
    matrix results[`row', 2] = _b[ln_L]
    matrix results[`row', 3] = _b[ln_real_K]
    matrix results[`row', 4] = 0
    local row = `row' + 1
    
    estimates store WRDG_`s'

    *LP REGRESSION
    xi:levpet ln_real_VA, free(ln_L i.year i.country) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
	
    matrix results[`row', 1] = `s'
    matrix results[`row', 2] = _b[ln_L]
    matrix results[`row', 3] = _b[ln_real_K]
    matrix results[`row', 4] = 0
    local row = `row' + 1
    
    estimates store LP_`s'
    
    restore
}

matrix list results

estout OLS_13 WRDG_13 LP_13, cells(b(fmt(3)) se(par fmt(3))) stats(N r2) title("Sector 13 Results")
estout OLS_29 WRDG_29 LP_29, cells(b(fmt(3)) se(par fmt(3))) stats(N r2) title("Sector 29 Results")


*------------Question 2.b--------------*

* Create Excel file and define path
local excel_file "$output\Production_Function_Table.xlsx"
putexcel set "`excel_file'", replace

* Write headers with formatting and lines
putexcel A1 = "" B1 = "Nace-13" C1 = "Nace-29", bold hcenter
putexcel A1:C1, border(bottom, thick)

* Labels with section lines
putexcel A2 = "Lev-Pet", bold
putexcel A3 = "ln(labor)"
putexcel A4 = "ln(capital)"
putexcel A4:C4, border(bottom)

putexcel A5 = "WRDG", bold
putexcel A6 = "ln(labor)"
putexcel A7 = "ln(capital)"
putexcel A7:C7, border(bottom)

putexcel A8 = "OLS", bold
putexcel A9 = "ln(labor)"
putexcel A10 = "ln(capital)"
putexcel A10:C10, border(bottom)

putexcel A11 = "Bias in labour coefficient-lp", bold
putexcel A12 = "Bias in labour coefficient-wrdg", bold
putexcel A13 = "N. of observations", bold
putexcel A13:C13, border(bottom)

* Based on your matrix creation code:
* Rows 1-3 for sector 13: OLS (row 1), WRDG (row 2), LP (row 3)
* Rows 4-6 for sector 29: OLS (row 4), WRDG (row 5), LP (row 6)

* Lev-Pet (LP) - rows 3 and 6
putexcel B3 = matrix(results[3,2]), nformat(number_d2)
putexcel B4 = matrix(results[3,3]), nformat(number_d2)
putexcel C3 = matrix(results[6,2]), nformat(number_d2)
putexcel C4 = matrix(results[6,3]), nformat(number_d2)

* WRDG - rows 2 and 5
putexcel B6 = matrix(results[2,2]), nformat(number_d2)
putexcel B7 = matrix(results[2,3]), nformat(number_d2)
putexcel C6 = matrix(results[5,2]), nformat(number_d2)
putexcel C7 = matrix(results[5,3]), nformat(number_d2)

* OLS - rows 1 and 4
putexcel B9 = matrix(results[1,2]), nformat(number_d2)
putexcel B10 = matrix(results[1,3]), nformat(number_d2)
putexcel C9 = matrix(results[4,2]), nformat(number_d2)
putexcel C10 = matrix(results[4,3]), nformat(number_d2)

* Bias in labour coefficient (OLS - LP)
putexcel B11 = formula(B9-B3), nformat(number_d2)
putexcel C11 = formula(C9-C3), nformat(number_d2)
putexcel B12 = formula(B9-B6), nformat(number_d2)
putexcel C12 = formula(C9-C6), nformat(number_d2)


* Number of observations for sector 13
preserve
keep if sector == 13
xi: reg ln_real_VA ln_L ln_real_K i.year i.country
local obs13 = e(N)
putexcel B13 = `obs13'
restore

* Number of observations for sector 29
preserve
keep if sector == 29
xi: reg ln_real_VA ln_L ln_real_K i.year i.country
local obs29 = e(N)
putexcel C13 = `obs29'
restore

* Add title below table
putexcel A14 = "Table 1: Comparison of Production Function Coefficients for NACE-13 and NACE-29", bold


/*comment*/
*----------------------------------------------------------------*
**************************---QUESTION 3---************************
*----------------------------------------------------------------*

/* comment on LateX*/




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

twoway (kdensity ln_TFP_WRDG_13, lcolor(sienna)) || (kdensity ln_TFP_LP_13, lcolor(blue)), title("Log TFP Density Industry 13") legend(label(1 "WRDG") label(2 "LP")) 
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

/*************************************************************************************

*------------Question 4.c--------------*

Focus now on the TFP distributions of industry 13 in France and Spain. Do you find changes in these two TFP distributions in 2006 vs 2015? Did you expect these results? Compare the results obtained with WRDG and LP procedure and comment.
*************************************************************************************/

*Spain  - 2006 vs 2015 - LP
twoway (kdensity TFP_LP_13 if country=="Spain" & year==2006, lcolor(blue)) (kdensity TFP_LP_13 if country=="Spain" & year==2015, lcolor(red)), legend(label(1 "2006") label(2 "2015")) title("Kernel Density of TFP for Spain") subtitle("Levinsohn-Petrin") xtitle("TFP_LP_13") ytitle("Density") name(SP_lp, replace)

*Spain  - 2006 vs 2015 - WRDG
twoway (kdensity TFP_WRDG_13 if country=="Spain" & year==2006, lcolor(blue)) (kdensity TFP_WRDG_13 if country=="Spain" & year==2015, lcolor(red)), legend(label(1 "2006") label(2 "2015")) title("Kernel Density of TFP for Spain") subtitle("Wooldridge") xtitle("TFP_WRDG_13") ytitle("Density") name(SP_WRDG, replace)

graph combine SP_lp SP_WRDG, col(2)
graph export "$output/TFP_SP.png", replace

/* If we look at the TFP distribution (obtained with LP procedure) in industry 13 in Spain, we see that both the distributions closely resemble a Pareto distribution, providing evidence for the fact that firms' performance in terms of productivity is highly heterogeneous, with many below-average firms and a small number of high performing firms at the right tail of the distribution.
In this case, we can clearly see a shift to the left of the distribution from 2006 to 2015, meaning that the average productivity in sector 13 is decreasing; at the same time, we can spot a thickening of the right tail of the distribution, meaning that there has been an increase in market shares of higly productive firms.
A similar result is obtained relying on the Wooldridge procedure: less productive firms in the aggregate, but a thicker, even though shorter, right tail.
We can thus conclude that Spain has seen an increase in dispersion in the TFP distribution in sector 13 from 2006 to 2015, i.e. there has been a reallocation effect of the trade shock occurred in this time frame from less to more productive firms.
*/

**************************************************************************************
*France  - 2006 vs 2015 - LP
twoway (kdensity TFP_LP_13 if country=="France" & year==2006, lcolor(blue)) (kdensity TFP_LP_13 if country=="France" & year==2015, lcolor(red)), legend(label(1 "2006") label(2 "2015")) title("Kernel Density of TFP for France") subtitle("Levinsohn-Petrin") xtitle("TFP_LP_13") ytitle("Density") name(FR_lp, replace)

*France  - 2006 vs 2015 - WRDG
twoway (kdensity TFP_WRDG_13 if country=="France" & year==2006, lcolor(blue)) (kdensity TFP_WRDG_13 if country=="France" & year==2015, lcolor(red)), legend(label(1 "2006") label(2 "2015")) title("Kernel Density of TFP for France") subtitle("Wooldridge") xtitle("TFP_WRDG_13") ytitle("Density") name(FR_WRDG, replace)

graph combine FR_lp FR_WRDG, col(2)
graph export "$output/TFP_FR.png", replace

/*************************************************************************************

*------------Question 4.d--------------*

Look at changes in skewness in the same time window (again, focus on industry 13 only in these two countries). What happens? Relate this result to what you have found at point c.
*************************************************************************************/



/*************************************************************************************

*------------Question 4.e--------------*
Do you find the shifts to be homogenous throughout the distribution? Once you have defined a specific parametrical distribution for the TFP, is there a way through which you can statistically measure the changes in the TFP distribution in each industry over time (2006 vs 2015)?
*************************************************************************************/
 
*We estimate the curvature of the distributions of TFP by looking at the estimated coefficient k, which we obtain by an OLS regression, as explained by Del Gatto, Mion and Ottaviano (2006): we assume that productivity is a random variable (X)  distributed with a Pareto distribution with shape parameter k; then, we regress   ln(1 − F(X)) on ln(X). The OLS estimate of the coefficient plus a constant is a consistent estimator of k, as proved by Norman, Kotz and Balakrishnan (1994). Furthermore, the estimator is consistent if the corresponding R2 is close to unity: in our case, all the R2 are greater than 0.75, showing that the distribution of the productivity in this sector closely resemble a Pareto distribution.  Table . shows the estimated ks and the R2 of our regressions by sector. 

*First, let's check sector 13
sort year
by year: cumul TFP_LP_13, generate(cum_TFP_LP_13)
gen rhs_LP_13=log(1- cum_TFP_LP_13)

foreach y in 2006 2015{
qui reg rhs_LP_13 ln_TFP_LP_13 if year==`y'
outreg2 using "$output/Pareto_13.xls", append title("Pareto Distribution for sector 13") ctitle("`y'")
qui reg rhs_LP_13 ln_TFP_LP_13 if year==`y' & country=="France"
outreg2 using "$output/Pareto_13.xls", append title("Pareto Distribution for sector 13") ctitle("France `y'")
qui reg rhs_LP_13 ln_TFP_LP_13 if year==`y' & country=="Spain"
outreg2 using "$output/Pareto_13.xls", append title("Pareto Distribution for sector 13") ctitle("Spain `y'")
qui reg rhs_LP_13 ln_TFP_LP_13 if year==`y' & country=="Italy"
outreg2 using "$output/Pareto_13.xls", append title("Pareto Distribution for sector 13") ctitle("Italy `y'")
}

/*The table presents the estimated curvature (`k' coefficients) of the distributions of the TFP in 2006 and in 2015 in sector 13, both at the aggregate industry level (1 and 5), and also for each country (2,3,4,6,7,8).
Larger coefficients correspond to industries characterised by larger shares of small and unproductive firms and which are therefore more prone to reallocation effects after a trade shock. In our case, the estimated k for sector 13 is 1.236 in 2006 and 1.133 in 2015, meaning that the TFP distribution has become more dispersed. We can thus conclude that there has been a reallocation of market shares from unproductive firms to more productive firms in textile manufacturing from 2006 to 2015. If we look at the number of observations, there were 6,496 firms in sector 13 in 2006, while after nine years the number of firms in the same sector decreased to 5,706, meaning that the trade shock probably has led to the market exit of many of the small, unproductive firms. 
If we look at the TFP distributions by country, we can see that the k parameter increased in France from 1.187 in 2006 to 1.226 in 2015, indicating that the productivity distribution became even more skewed towards relatively small and inefficient firms. Estimated ks instead decreased in Spain from 1.095 to 1.056 and also in Italy, from 1.346 to 1.158, from 2006 to 2015. Thus, we can conclude that the general trend at the industry level is driven mainly by the decrease in k in Spain and Italy from 2006 to 2015.
*/


*Now, sector 29
sort year
by year: cumul TFP_LP_29, generate(cum_TFP_LP_29)
gen rhs_LP_29=log(1- cum_TFP_LP_29)

foreach y in 2006 2015{
qui reg rhs_LP_29 ln_TFP_LP_29 if year==`y'
outreg2 using "$output/Pareto_29.xls", append title("Pareto Distribution for sector 29") ctitle("`y'")
qui reg rhs_LP_29 ln_TFP_LP_29 if year==`y' & country=="France"
outreg2 using "$output/Pareto_29.xls", append title("Pareto Distribution for sector 29") ctitle("France `y'")
qui reg rhs_LP_29 ln_TFP_LP_29 if year==`y' & country=="Spain"
outreg2 using "$output/Pareto_29.xls", append title("Pareto Distribution for sector 29") ctitle("Spain `y'")
qui reg rhs_LP_29 ln_TFP_LP_29 if year==`y' & country=="Italy"
outreg2 using "$output/Pareto_29.xls", append title("Pareto Distribution for sector 29") ctitle("Italy `y'")
}

/* The table presents the estimated curvature (`k' coefficients) of the distributions of the TFP in 2006 and in 2015 in sector 29, both at the aggregate industry level (1 and 5), and also for each country (2,3,4,6,7,8). In this case, the R2 from the OLS regressions are even higher than before, the lowest being 0.782 and the highest being 0.836: this means that our estimators of k are consistent with the k parameters of the Pareto distributions, validating our results.
In sector 29, the estimated OLS coefficient is 1.253 in 2006 and 1.137 in 2015, meaning that also in the motor vehicles, trailers, and semi-trailers manifacturing sector there has been a reallocation effect of market shares from least to most productive firms from 2006 to 2015. The number of firms has decreased also in this sector, but less significantly than before, from 2,797 in 2006 to 2,646 in 2015.
If we look at the country level, both France and Italy saw a reduction in the estimated k from 2006 to 2015, from, respectively, 1.472 and 1.198 in 2006 to, respectively, 1.198 and 1.006 in 2015. These two countries have thus seen an increase in the number of productive firms and a decrease in the one of unproductive firms in this time frame.
Spain has instead seen a decrease in dispersion in its TFP distribution in this sector, with the estimated k going from 1.166 to 1.240 from 2006 to 2015.  
*/


*----------------------------------------------------------------*
**************************---QUESTION 5---************************
*----------------------------------------------------------------*

*ssc install spmap
*ssc install shp2dta

*------------Question 5.a--------------*

/* (a) Merge the first three datasets together. Compute the China shock for each region, in each year for which it is possible, according to the equation above. Use a lag of 5 years to compute the import deltas (i.e., growth in imports between t-6 and t-1). Repeat the same procedure with US imports, i.e., substituting ∆IM P Chinackt with ∆IM P ChinaU SAkt, following the identification strategy by Colantone and Stanig (AJPS, 2018). */

	*—— i. Load pre-sample employment shares (first year only) ———————————————*
use "$data/Employment_Shares_Take_Home.dta", clear
tab  nace 
keep country year nuts2 nace empl tot_empl_nuts2 tot_empl_country_nace
gen share_rk    = empl / tot_empl_nuts2
save "$output/weights.dta", replace

	*—— ii. China imports delta ——————————————————————————————————————————————*
use "$output/weights.dta", clear

merge m:1 country year nace using "$data/Imports_China_Take_Home.dta"
assert _merge==3
drop _merge

egen panel_id = group(country nace nuts2)
xtset panel_id year

gen imp1  = L.real_imports_china      // imports at t−1.  *vanno presi 
gen imp6  = L6.real_imports_china     // imports at t−6
gen d_imp_china = imp1 - imp6              // 5-year growth
gen scaled_d_imp_china = d_imp_china / tot_empl_country_nace
gen china_shock_elem  = share_rk * scaled_d_imp_china 
drop if missing(china_shock_elem)

bysort country nuts2 year: egen ChinaShock = total(china_shock_elem)
keep country nuts2 year ChinaShock
duplicates drop
save "$output/ChinaShock_by_region_year.dta", replace


*============================*
* Load China shock data
*============================*
use "$output/ChinaShock_by_region_year.dta", clear

*============================*
* Step 1: Create period variable
*============================*
gen period = .
replace period = 1 if inrange(year, 1994, 2000)
replace period = 2 if inrange(year, 2001, 2007)

label define period_lbl 1 "1994–2000" 2 "2001–2007" 
label values period period_lbl

*============================*
* Step 2: Display percentiles using table (Stata 18 syntax)
*============================*
table period, ///
    statistic(p10 ChinaShock) ///
    statistic(p25 ChinaShock) ///
    statistic(p50 ChinaShock) ///
    statistic(p75 ChinaShock) ///
    statistic(p90 ChinaShock) /// ///

drop period	

	*—— iii. US imports delta (instrument) ————————————————————————————————————*
use "$output/weights.dta", clear

merge m:1 year nace using "$data/Imports_US_China_Take_Home.dta"
drop if _merge!=3
drop _merge

egen nace_id = group(nace nuts2)
xtset nace_id year

gen us1    = L.real_USimports_china
gen us6    = L6.real_USimports_china
gen d_us_imp = us1 - us6

gen scaled_d_us_imp = d_us_imp / tot_empl_country_nace
gen us_iv_elem= share_rk *  scaled_d_us_imp
drop if missing(us_iv_elem)

bysort country nuts2 year: egen USShock = total(us_iv_elem)
keep country nuts2 year USShock
save "$output/USShock_by_region_year.dta", replace

*------------Question 5.b--------------*
/* Collapse the dataset by region to obtain the average 5-year China shock over the sample period. This will be the average of all available years' shocks (for reference, see Colantone and Stanig, American Political Science Review, 2018). You should now have a dataset with cross-sectional data. */
use "$output/ChinaShock_by_region_year.dta", clear
collapse (mean) ChinaShock, by(country nuts2)
save "$output/ChinaShock_by_region_year_collapsed.dta", replace

use "$output/USShock_by_region_year.dta", clear
collapse (mean) USShock, by(country nuts2)
save "$output/USShock_by_region_year_collapsed.dta", replace

use "$output/ChinaShock_by_region_year_collapsed.dta", clear
merge 1:1 country nuts2 using "$output/USShock_by_region_year_collapsed.dta"
drop if _merge!=3
drop _merge

* Compute each region's average 5-year shock over all years available
collapse (mean) ChinaShock USShock, by(country nuts2)

save "$output/region_shocks_avg.dta", replace

*------------Question 5.c--------------*
/* Produce a map visualizing the China shock for each region, i.e., with darker shades reflecting stronger shocks. Going back to the "Employment Shares Take Home.dta", do the same with respect to the overall pre-sample share of employment in the manufacturing sector. Do you notice any similarities between the two maps? What were your expectations? Comment. LINK TO TUTORIAL ON THE PDF */

*—— Load & merge region-cross-section shocks ——————————————————————————————*
use "$output/region_shocks_avg.dta", clear

*—— Convert & merge NUTS-2 shapefile ————————————————————————————————————*
shp2dta using "/Users/user/Desktop/STATA/eu/NUTS_RG_01M_2013_4326/NUTS_RG_01M_2013_4326.shp", ///
    database("nuts2_db.dta") coordinates("nuts2_coords.dta") ///
    genid(id) replace
	
use "$output/region_shocks_avg.dta", clear
rename nuts2 NUTS_ID // Match shapefile region code
save "china_shock_mapped.dta", replace

use "nuts2_db.dta", clear
merge 1:1 NUTS_ID using "china_shock_mapped.dta"
drop if _merge!=3
drop if NAME_LATN=="Canarias"

*—— Map average China shock ——————————————————————————————————————*
spmap ChinaShock using "nuts2_coords.dta", id(id) ///
 fcolor(Blues) clmethod(quantile) clnumber(6) ///
    title("Average China Shock by Region")legend(on pos(6) ring(0) cols(1))

graph export "Average_China_Shock_by_Region.png", replace

*—— Compute & map pre-sample manufacturing share ——————————————————————*
* Use a reference pre-sample year (values are constant)
use "Employment_Shares_Take_Home.dta", clear

* Use a reference pre-sample year (values are constant)
keep if year == 1988

collapse (sum) empl, by(country nuts2)

rename empl manuf_empl
save "manuf_empl.dta", replace

use "Employment_Shares_Take_Home.dta", clear
keep if year == 1988
keep country nuts2 tot_empl_nuts2
duplicates drop

merge 1:1 nuts2 using "manuf_empl.dta"

gen manuf_share=manuf_empl/tot_empl_nuts2

drop if _merge!= 3
drop _merge

rename nuts2 NUTS_ID  // to match shapefile
save "manuf_share.dta", replace

use "nuts2_db.dta", clear
merge 1:1 NUTS_ID using "manuf_share.dta"
drop if _merge!= 3
drop _merge
drop if NAME_LATN=="Canarias"

spmap manuf_share using "nuts2_coords.dta", id(id) ///
    fcolor(Greens) clmethod(quantile) clnumber(6) ///
    title("Manufacturing Share by Region") legend(on pos(6) ring(0) cols(1))
	
graph export "Manufacturing_Share_by_Region.png", replace
		  


*----------------------------------------------------------------*
**************************---QUESTION 6---************************
*----------------------------------------------------------------*
global path "C:\Users\utente\Desktop\2nd semester\European integration\Take home"

global data "$path/Data"
global output "$path/Output"

use "$data/EEI_TH_P6_2025", replace

/* Use the dataset "EEI TH P6 2025.dta" to construct an average of TFP and wages during the post-crisis years (2014-2017).*/
rename nuts_code nuts2
egen tfp_pc = mean(tfp) if inrange(year, 2014, 2017), by(cou nuts2)
egen wages_pc = mean(mean_uwage) if inrange(year, 2014, 2017), by(cou nuts2)

*Create a lag of 3 years in the control variables (education, GDP and population).
preserve

collapse (mean) share_tert_educ lnpop control_gdp, by(nuts2 year)

*Sort and generate the 3-year lags
sort nuts2 year

bysort nuts2 (year): gen lag_educ = share_tert_educ[_n-3]
bysort nuts2 (year): gen lag_pop = lnpop[_n-3]
bysort nuts2 (year): gen lag_gdp = control_gdp[_n-3]

tempfile lags
save `lags'

restore

*Merge the lags back into the original dataset
merge m:1 nuts2 year using `lags', nogen

save "$output/P6_lags.dta", replace

/*Now merge the data you have obtained with data on the China shock (region-specific average).*/
use "$output/P6_lags.dta", replace

merge m:1 nuts2 using "$output/region_shocks_avg.dta"
drop if _merge != 3
drop _merge

save "$output/P6_merged.dta", replace

*------------Question 6.a--------------*

/*Regress (simple OLS) the post-crisis average of TFP against the region-level China shock previously constructed, controlling for the 3-year lags of population, education and GDP. Comment on the estimated coefficient on the China shock, and discuss possible endogeneity issues.
*/
regress tfp_pc ChinaShock lag_pop lag_educ lag_gdp, robust
eststo ols1
/*The estimated coefficient on avg_china_shock is 1.81078 with a t-statistic of 30.07, which is statistically significant at the 1% level (since the p-value is 0.000). The positive sign of the coefficient indicates that, on average, an increase in the China shock (i.e., the higher the average China shock in a region) is associated with an increase in average TFP for that region in the post crisis period (2014-2017).
This might be due to increased competition leading to productivity improvements or adoption of new technologies by existing firms to keep up with the imports from China. At the same time, the positive effect on TFP might also be due to a reallocation effect from less productive firms or sectors (those more threatened by Chinese competition) to more productive ones, leading to aggregate shifts in the regional economies.
There are nonetheless possible endogeneity issues, such as, first of all, bias due to simultaneity. The China trade shock may be influenced by economic conditions at the region level, which could simultaneously affect TFP. For example, regions with higher TFP may be more likely to experience trade shocks, because they are regions already exposed to international trade. This can cause the OLS estimates of the China import shock to overstate its effect. Another source of endogeneity are omitted variables; it's possible that there are other unobserved factors influencing both TFP and the China shock. For instance, there are factors like political stability, local industrial policies, or access to technology that could influence both the response to trade shocks and regional productivity, which might not be captured in the regression. The OLS model would then incorrectly attribute the changes in TFP solely to the China Trade Shock, possibly overstating the effect of the China shock. Finally, the measurement of the China Trade Shock might not be perfectly accurate. If the shock is measured with error, this could lead to biased OLS estimates. 
*/

*------------Question 6.b--------------*

/*To deal with endogeneity issues, use the instrumental variable you have built before, based on changes in Chinese imports to the USA, and run again the regressions as in a). Do you see any changes in the coefficient?
*/
ivregress 2sls tfp_pc lag_pop lag_educ lag_gdp (ChinaShock = USShock), vce(robust)
eststo iv1

/*To address endogeneity concerns, a 2SLS (two-stage least squares) model is used. This method helps to deal with endogeneity by using instrumental variables that are correlated with the endogenous explanatory variable but uncorrelated with TFP. We instrument the import shock in European countries using US imports from China, as in Autor et al. (2013), to capture the variation in Chinese imports which is due to exogenous changes in supply conditions in China, rather than to domestic factors in European countries, potentially correlated with productivity performances.
In the second-stage regression, the China Trade Shock is instrumented to address the potential endogeneity. The drop in the coefficient from 1.811 (OLS) to 0.495 (2SLS) suggests that the OLS model likely overestimated the effect of the China Trade Shock. The 2SLS estimate of 0.495 seems more reliable because it corrects for endogeneity bias, showing a smaller but still significant effect. The positive relationship in the IV regression suggests that the China shock may still have a beneficial effect on TFP, but it is less pronounced when we use instrument for USShock.
*/

*------------Question 6.c--------------*

/*Now, regress (both OLS and IV) the post-crisis average of wage against the region-level China shock previously constructed, controlling for the 3-year lags of population, education and GDP. Comment on the estimated coefficient on the China shock.
*/
regress wages_pc ChinaShock lag_pop lag_educ lag_gdp, robust
eststo ols2
ivregress 2sls wages_pc lag_pop lag_educ lag_gdp (ChinaShock = USShock), vce(robust)
eststo iv2

/*The OLS coefficient of the China shock is 53.51 and is highly significant, suggesting that the import shock from China had a positive effect on wages in the post-crisis period. This result is counterintuitive and it might be biased by endogeneity issues. If we perform a 2SLS regression using the Chinese imports to the US as instrumental variable as before, we see that the coefficient drops dramatically to 3.661 and it loses significance.
*/

/*Lastly, what happens if you regress (both OLS and IV) the post-crisis average of
wage against the region-level China shock previously constructed. Control for the average TFP during the post crisis years, an interaction term between average of TFP and China shock and for the 3-year lags of population, education and GDP? Comment.
*/
regress wages_pc ChinaShock tfp_pc c.tfp_pc#c.ChinaShock lag_pop lag_educ lag_gdp, robust
eststo ols3
ivregress 2sls wages_pc tfp_pc c.tfp_pc#c.ChinaShock lag_pop lag_educ lag_gdp (ChinaShock = USShock), vce(robust)
eststo iv3

/*When we include the interaction term between TFP and China shock, our results change dramatically: now the coefficient of the China shock is significant and with much greater magnitude, -147.9. The 2SLS regression provides a very similar result (-141.4), suggesting that with this model specification, we are able to capture more properly the effect of the China shock on the post crisis average of wages, which is very negative.
At the same time, the coefficient of the interaction term between the China shock and the post crisis average of TFP is positive and statistically significant for both the OLS (66) and 2SLS (63.06) regressions, suggesting that regions with higher TFP levels experienced a larger positive effect in terms of wages from the China trade shock.
This result might be driven by the fact that regions with higher productivity levels might have industries that are more capable of adapting to the external trade shock, possibly due to their ability to innovate, use technology more efficiently, or better integrate into global supply chains, while the losers of this trade shock, at least in terms of wages, were the less productive firms in those same regions.
*/


label variable ChinaShock "China Trade Shock"
label variable USShock "US Instrument"
label variable tfp_pc "TFP ('14-'17)"
label variable wages_pc "Wages ('14-'17)"
label variable lag_pop "3yrs Lag Population"
label variable lag_educ "3yrs Lag Education"
label variable lag_gdp "3yrs Lag GDP"

esttab ols1 iv1 using "$output/TABLE_1.doc", replace label title("OLS and IV Regression Results - TFP") mtitles("OLS" "2SLS")

outreg2 [ols1 iv1] using Table, label title("OLS and IV Regression Results - TFP"), tex replace

esttab ols2 iv2 ols3 iv3 using "$output/TABLE_2.doc", replace label title("OLS and IV Regression Results - Wages") mtitles("OLS" "2SLS" "OLS" "2SLS") compress

esttab [ols2 iv2 ols3 iv3] using "$output/TABLE_2.doc", replace label title("OLS and IV Regression Results - Wages") mtitles("OLS" "2SLS" "OLS" "2SLS") compress



*----------------------------------------------------------------*
**************************---QUESTION 7---************************
*----------------------------------------------------------------*

*Download ESS Round 8
use "$data\ESS8e02_3\ESS8e02_3.dta" 

keep if cntry=="IT"
keep pspwght gndr agea edulvlb region prtvtbit sbsrnen

save "$data/ESS8.dta", replace

*------------Question 7.a--------------*
* Merge the ESS dataset with data on the China shock (region-specific average), based on the region of residence of each respondent.

use "$output/region_shocks_avg.dta"
keep if country=="Italy"
rename nuts2 region
save "$data\region_shocks_italy_avg.dta", replace

use "$data\ESS8.dta", clear
merge m:1 region using "$data\region_shocks_italy_avg.dta"
drop if _merge == 2

save "$data\merged_ESSCS.dta", replace

*------------Question 7.b--------------*

gen nuts1 = substr(region, 1, 3)
encode nuts1, gen(NUTS1)

encode region, gen(region_id)

log using "Regressions7.log", replace
reg sbsrnen ChinaShock gndr agea i.edulvlb i.NUTS1 [pw=pspwght], vce(cluster region_id)

*ssc install estout
*esttab using Q7b.tex, replace tex label se title("Regression of green attitude score on region-level China shock") keep(avg_china_shock)  


*------------Question 7.c--------------*
ivregress 2sls sbsrnen (ChinaShock = USShock) gndr agea i.edulvlb i.NUTS1  [pw = pspwght], vce(cluster region_id) first
log close


*------------Question 7.d--------------*
/*comment on LateX

*------------Question 7.e--------------*
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



*------------Question 7.f--------------*
* Regress (both OLS and IV, as above) the underprivileged minority groups score of the party voted against the region-level China shock, controlling for gender, age, dummies for levels of education, and dummies for Nuts regions at the 1 digit level. For this purpose, transform the score as follows: minority scorepc = log(.5 + zpc) with z being the variable for the "very general favorable references to underprivileged minority groups". Cluster the standard errors by Nuts region level 2. Be sure to use survey weights in the regressions. Comment.

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



*------------Question 7.g--------------*

/*comment on LateX
