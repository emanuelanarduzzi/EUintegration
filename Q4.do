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
global data "$filepath"
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



