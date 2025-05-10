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
***********************************************************************************
* a)                                                      
***********************************************************************************

/*Regress (simple OLS) the post-crisis average of TFP against the region-level China shock previously constructed, controlling for the 3-year lags of population, education and GDP. Comment on the estimated coefficient on the China shock, and discuss possible endogeneity issues.
*/
regress tfp_pc ChinaShock lag_pop lag_educ lag_gdp, robust
eststo ols1
/*The estimated coefficient on avg_china_shock is 1.81078 with a t-statistic of 30.07, which is statistically significant at the 1% level (since the p-value is 0.000). The positive sign of the coefficient indicates that, on average, an increase in the China shock (i.e., the higher the average China shock in a region) is associated with an increase in average TFP for that region in the post crisis period (2014-2017).
This might be due to increased competition leading to productivity improvements or adoption of new technologies by existing firms to keep up with the imports from China. At the same time, the positive effect on TFP might also be due to a reallocation effect from less productive firms or sectors (those more threatened by Chinese competition) to more productive ones, leading to aggregate shifts in the regional economies.
There are nonetheless possible endogeneity issues, such as, first of all, bias due to simultaneity. The China trade shock may be influenced by economic conditions at the region level, which could simultaneously affect TFP. For example, regions with higher TFP may be more likely to experience trade shocks, because they are regions already exposed to international trade. This can cause the OLS estimates of the China import shock to overstate its effect. Another source of endogeneity are omitted variables; it's possible that there are other unobserved factors influencing both TFP and the China shock. For instance, there are factors like political stability, local industrial policies, or access to technology that could influence both the response to trade shocks and regional productivity, which might not be captured in the regression. The OLS model would then incorrectly attribute the changes in TFP solely to the China Trade Shock, possibly overstating the effect of the China shock. Finally, the measurement of the China Trade Shock might not be perfectly accurate. If the shock is measured with error, this could lead to biased OLS estimates. 
*/

***********************************************************************************
* b)                                                      
***********************************************************************************

/*To deal with endogeneity issues, use the instrumental variable you have built before, based on changes in Chinese imports to the USA, and run again the regressions as in a). Do you see any changes in the coefficient?
*/
ivregress 2sls tfp_pc lag_pop lag_educ lag_gdp (ChinaShock = USShock), vce(robust)
eststo iv1

/*To address endogeneity concerns, a 2SLS (two-stage least squares) model is used. This method helps to deal with endogeneity by using instrumental variables that are correlated with the endogenous explanatory variable but uncorrelated with TFP. We instrument the import shock in European countries using US imports from China, as in Autor et al. (2013), to capture the variation in Chinese imports which is due to exogenous changes in supply conditions in China, rather than to domestic factors in European countries, potentially correlated with productivity performances.
In the second-stage regression, the China Trade Shock is instrumented to address the potential endogeneity. The drop in the coefficient from 1.811 (OLS) to 0.495 (2SLS) suggests that the OLS model likely overestimated the effect of the China Trade Shock. The 2SLS estimate of 0.495 seems more reliable because it corrects for endogeneity bias, showing a smaller but still significant effect. The positive relationship in the IV regression suggests that the China shock may still have a beneficial effect on TFP, but it is less pronounced when we use instrument for USShock.
*/

***********************************************************************************
* c)                                                      
***********************************************************************************

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


