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
*----------------------------------------------------------------*
**************************---QUESTION 2---************************
*----------------------------------------------------------------*
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

xi: levpet ln_real_VA if sector==29, free(ln_L i.country i.year) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)

matrix table = r(table)
matrix list table

scalar ln_L_LP_29 = table[1,1]
scalar list ln_L_LP_29
scalar ln_K_LP_29 = table[1,21]
scalar list ln_K_LP_29
return list
display e(N)

drop _Icountry*  _Iyear*

* Wooldridge (WRDG) - VALUE ADDED

capture confirm variable country_num
if _rc != 0 {
    encode country, gen(country_num)
}
capture drop year_dummy*
capture drop country_dummy*
tab year, gen(year_dummy)
tab country_num, gen(country_dummy)

xi= prodest ln_real_VA if sector==13, free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* country_dummy*)  method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded 

*this is the right method but how do we account for differences in year and countries, is this the right method?

matrix table = r(table)
matrix list table

scalar ln_L_WRDG_13 = table[1,1]
scalar list ln_L_WRDG_13
scalar ln_K_WRDG_13 = table[1,2]
scalar list ln_K_WRDG_13
return list
display e(N)

xi: prodest ln_real_VA if sector==29, free(ln_L) state(ln_real_K) proxy(ln_real_M) control(year_dummy* country_dummy*)  method(wrdg) id(id_n) t(year) level(99) reps(50) valueadded

matrix table = r(table)
matrix list table

scalar ln_L_WRDG_29 = table[1,1]
scalar list ln_L_WRDG_29
scalar ln_K_WRDG_29 = table[1,2]
scalar list ln_K_WRDG_29
return list
display e(N)


gen bias_13=ln_L_OLS_13- ln_L_LP_13
gen bias_29=ln_L_OLS_29- ln_L_LP_29

display bias_13

display bias_29

/*The Cobb-Douglas production function is the basis for the analysis, expressed as:
Y=AL^βK^α
Taking the logarithm leads to a linear specification:
ln⁡Y=ln⁡A+βln⁡L+αln⁡K+ε
where L represents labor, K represents capital, A is total factor productivity (TFP), and ε captures unobserved productivity shocks.

OLS provides baseline estimates but assumes input choices are exogenous, meaning they are not influenced by productivity shocks. This assumption is problematic, as firms typically adjust labor and capital in response to expected productivity changes. As a result, OLS estimates tend to overstate the role of labor and capital in production due to simultaneity bias.
LP addresses simultaneity by using intermediate inputs as proxies for unobserved productivity shocks. This method assumes that firms adjust their intermediate input use based on their productivity expectations, allowing for a correction in the estimation of labor and capital elasticities. However, LP does not account for firm-specific time-invariant characteristics, which may still influence productivity.
WRDG applies firm-level fixed effects within a Generalized Method of Moments (GMM) framework. This approach not only addresses simultaneity but also controls for unobserved heterogeneity across firms. Unlike LP, which relies on proxy variables, WRDG explicitly models productivity as correlated with past input choices, making it a more comprehensive method for addressing endogeneity.

The coefficients for labor and capital are positive and statistically significant at all conventional levels across all estimation methods.
The labor elasticity estimates represent the percentage change in value-added output resulting from a 1% increase in labor input, holding capital constant. Using Ordinary Least Squares (OLS), labor elasticity is estimated at 0.806 for NACE-13, meaning that a 1% increase in labor leads to a 0.806% increase in output. For NACE-29, the OLS estimate is 0.911, indicating a 0.911% increase in output per 1% increase in labor.
he estimation results show that the OLS method consistently yields the highest labor elasticity estimates. In contrast, Levinsohn & Petrin (LP) and Wooldridge (WRDG) methods produce lower labor elasticity values, with the Levinsohn & Petrin (LP) method estimating labor elasticity at 0.639 for NACE-13 and 0.647 for NACE-29, suggesting a 1% increase in labor raises output by only 0.639% and 0.647%, respectively. The Wooldridge (WRDG) method yields slightly higher labor elasticity estimates than LP, at 0.661 for NACE-13 and 0.682 for NACE-29, still below OLS values.
A similar trend is observed for capital elasticity. OLS estimates capital elasticity at 0.163 for NACE-13 and 0.125 for NACE-29, implying that a 1% increase in capital leads to a 0.163% and 0.125% increase in output, respectively. The LP and WRDG methods, which control for simultaneity, yield lower estimates: LP estimates 0.072 for NACE-13 and 0.078 for NACE-29, while WRDG produces estimates of 0.062 and 0.071, respectively.*/

*Given the presence of data from multiple countries and years, it is necessary to control for potential heterogeneity in productivity estimation. Year and country fixed effects are included in the analysis to account for macroeconomic differences and institutional variations. Additionally, WRDG incorporates firm-level fixed effects, ensuring that persistent differences between firms do not bias the results.
*The results indicate that OLS consistently produces higher labor elasticity estimates compared to LP and WRDG.The reason for this bias is that labor and capital inputs are not truly exogenous. Firms adjust these inputs based on productivity expectations, making them correlated with the error term in OLS estimations. LP corrects for this by incorporating intermediate inputs as proxies, while WRDG further refines the estimation by eliminating firm-specific effects that could distort productivity measurements.
*----------------------------------------------------------------*
**************************---QUESTION 3---************************
*----------------------------------------------------------------*

/*The choice between using revenues or value-added in estimating the production function affects the interpretation of productivity coefficients. Estimating productivity using revenues introduces additional biases because revenues incorporate not only production efficiency but also firm-specific pricing strategies. As a result, firms with higher markups appear more productive, even if their actual efficiency remains unchanged.
Value-added, which is computed as revenues minus intermediate inputs, provides a more accurate measure of firm output. By excluding intermediate input costs, it isolates the contributions of labor and capital to production, avoiding distortions caused by price markups.
From a theoretical perspective, the Cobb-Douglas production function assumes constant returns to scale, implying that the sum of labor and capital elasticities should equal one. When revenues are used instead of value-added, the sum of these elasticities often exceeds one, suggesting increasing returns to scale that may not actually exist. This occurs because intermediate inputs, which are part of revenues but not explicitly accounted for in the model, create an overstatement of input contributions.
Empirically, revenue-based estimations tend to yield higher labor and capital coefficients than value-added estimations. OLS applied to revenue overstates factor elasticities due to simultaneity bias and markup distortions. While LP and WRDG attempt to correct for these biases, revenue-based estimates still reflect differences in pricing power rather than pure productivity.*/
