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



