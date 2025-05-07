**********************************************************************
*                          Problem VI                                *
*                                                                    *
**********************************************************************
global path "C:\Users\utente\Desktop\2nd semester\European integration\Take home"

global data "$path/Data"
global output "$path/Output"

use "$data/EEI_TH_P6_2025", replace

/* Use the dataset "EEI TH P6 2025.dta" to construct an average of TFP and wages during the post-crisis years (2014-2017).*/
rename nuts_code nuts2
egen tfp_pc = mean(tfp) if inrange(year, 2014, 2017), by(cou nuts2)
egen wages_pc = mean(mean_uwage) if inrange(year, 2014, 2017), by(cou nuts2)

/* Create a lag of 3 years in the control variables (education, GDP and population).*/
bysort nuts2 (year): gen lag_educ = share_tert_educ[_n-3]
bysort nuts2 (year): gen lag_gdp = control_gdp[_n-3]
bysort nuts2 (year): gen lag_pop = lnpop[_n-3]

save "P6_new.dta", replace

/*Now merge the data you have obtained with data on the China shock (region-specific average).*/
use "P6_new.dta", replace

merge m:1 nuts2 using "$output/region_shocks_avg.dta"
drop if _merge != 3
drop _merge

save "P6_merged.dta", replace

**  a)  **
/*Regress (simple OLS) the post-crisis average of TFP against the region-level China shock previously constructed, controlling for the 3-year lags of population, education and GDP. Comment on the estimated coefficient on the China shock, and discuss possible endogeneity issues.
*/
regress tfp_pc ChinaShock lag_pop lag_educ lag_gdp

/*The estimated coefficient on avg_china_shock is 2.546738 with a t-statistic of 28.24, which is statistically significant at the 1% level (since the p-value is 0.000). The positive sign of the coefficient indicates that, on average, an increase in the China shock (i.e., the higher the average China shock in a region) is associated with an increase in average TFP (Total Factor Productivity) for that region (perhaps due to technological diffusion or increased competition).
*/


**  b)  **
/*To deal with endogeneity issues, use the instrumental variable you have built before, based on changes in Chinese imports to the USA, and run again the regressions as in a). Do you see any changes in the coefficient?
*/
regress tfp_pc USShock lag_pop lag_educ lag_gdp

/*The IV approach changes the magnitude of the coefficient on the China shock, addressing potential endogeneity in the direct China shock variable. The estimated coefficient is now .0269146 with a t-statistic of 4.32, again statistically significant at the 1% level. The positive relationship in the IV regression suggests that the China shock may still have a beneficial effect on TFP, but it is less pronounced when we use instrument for USShock.
*/

**  c  **
/*Now, regress (both OLS and IV) the post-crisis average of wage against the region-level China shock previously constructed, controlling for the 3-year lags of population, education and GDP. Comment on the estimated coefficient on the China shock. Lastly, what happens if you regress (both OLS and IV) the post-crisis average of
wage against the region-level China shock previously constructed. Control for the average TFP during the post crisis years, an interaction term between average of TFP and China shock and for the 3-year lags of population, education and GDP? Comment.
*/
regress wages_pc ChinaShock lag_pop lag_educ lag_gdp
* Coefficient: 74.70234 ; p-value: 0.000
regress wages_pc USShock lag_pop lag_educ lag_gdp
* Coefficient: .025595 ; p-value: 0.904

regress wages_pc ChinaShock c.tfp_pc##c.ChinaShock lag_pop lag_educ lag_gdp
* Coefficient: -200.2357 ; p-value: 0.000
regress wages_pc USShock c.tfp_pc##c.USShock lag_pop lag_educ lag_gdp
* Coefficient: -10.55616 ; p-value: 0.000
