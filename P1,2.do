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
