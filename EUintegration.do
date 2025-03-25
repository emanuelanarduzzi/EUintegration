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
    global filepath "/Users/user/Desktop/STATA/eu/" //emanuela's file path
}

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
*------------Question 2.a--------------*
use "$filepath/EEI_TH_2025.dta", clear

*drop neg values
foreach var in real_sales real_M real_K L{
        drop if  `var'<=0
        }

*create logarithms of continuous variables (on deflated values)
foreach var in real_sales real_M real_K L real_VA {
        gen ln_`var'=ln(`var')
		}
	
net install st0060, from("http://www.stata-journal.com/software/sj4-2/")
ssc install outreg2
net install prodest, from("http://fmwww.bc.edu/RePEc/bocode/p")


*** Consider now all the three countries. Estimate for the two industries available in NACE Rev. 2 2-digit format the production function coefficients, by using standard OLS, the Wooldridge (WRDG) and the Levinsohn & Petrin (LP) procedure. How do you treat the fact that data come from different countries in different years in the productivity estimation?

*OLS REGRESSION - VALUE ADDED

xi: reg ln_real_VA ln_L ln_real_K i.country i.year if sector==13


matrix table = r(table)
matrix list table

scalar ln_L_OLS_13 = table[1,1]
scalar list ln_L_OLS_13
scalar ln_K_OLS_13 = table[1,2]
scalar list ln_K_OLS_13
return list
display e(N)


xi: reg ln_real_VA ln_L ln_real_K i.country i.year if sector==29


matrix table = r(table)
matrix list table

scalar ln_L_OLS_29 = table[1,1]
scalar list ln_L_OLS_29
scalar ln_K_OLS_29 = table[1,2]
scalar list ln_K_OLS_29
return list
display e(N)


* LEVINSOHN-PETRIN - VALUE ADDED 
count if missing(ln_real_M)
tabulate sector if missing(ln_real_M)

xi: levpet ln_real_VA if sector==13, free(ln_L i.country i.year) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)

matrix table = r(table)
matrix list table

scalar ln_L_LP_13 = table[1,1]
scalar list ln_L_LP_13
scalar ln_K_LP_13 = table[1,21]
scalar list ln_K_LP_13
return list
display e(N)

*PERCHè IL NUMERO DELLE OSSERVAZIONI è DIVERSO? ??

xi: levpet ln_real_VA if sector==29, free(ln_L i.country i.year) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)

matrix table = r(table)
matrix list table

scalar ln_L_LP_29 = table[1,1]
scalar list ln_L_LP_29
scalar ln_K_LP_29 = table[1,21]
scalar list ln_K_LP_29
return list
display e(N)



* Wooldridge (WRDG) - VALUE ADDED

capture drop year_dummy*
capture drop country_dummy*
tab year, gen(year_dummy)
tab country_num, gen(country_dummy)
prodest ln_real_VA if sector==13, free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* country_dummy*)  method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded 
*this is the right method but how do we account for differences in year and countries, is this the right method?

matrix table = r(table)
matrix list table

scalar ln_L_WRDG_13 = table[1,1]
scalar list ln_L_WRDG_13
scalar ln_K_WRDG_13 = table[1,2]
scalar list ln_K_WRDG_13
return list
display e(N)

prodest ln_real_VA if sector==29, free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* country_dummy*)  method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded
matrix table = r(table)
matrix list table

scalar ln_L_WRDG_29 = table[1,1]
scalar list ln_L_WRDG_29
scalar ln_K_WRDG_29 = table[1,2]
scalar list ln_K_WRDG_29
return list
display e(N)

*what bias does he refer to?
gen bias_13=ln_L_OLS_13- ln_L_LP_13
gen bias_29=ln_L_OLS_29- ln_L_LP_29

display bias_13

display bias_29





