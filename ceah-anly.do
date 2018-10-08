*** Purpose: analysis of children's education and mother's health
*** Author: S Bauldry
*** Date: September 26, 2018

*** Set working directory and load data
cd ~/dropbox/research/hlthineq/mgah/ceah/ceah-work
use ceah-data, replace


*** Examining different measures of adult children's education
eststo clear
foreach x of varlist adeg3 asch xedu xsch {
  eststo d`x': reg dep1 `x', vce(robust)
  logit adl1 `x', vce(robust)
  local pr2`x' = e(r2_p)
  qui estat ic
  mat s = r(S)
  local bic`x' = s[1,6]
  qui lroc
  local roc`x' = r(area)
  eststo a`x': margins, dydx(*) post
}

esttab dadeg3 dasch dxedu dxsch, b(%5.2f) se(%5.2f) r2(%5.2f) bic(%5.2f) ///
  star nonum nogap nomti
  
esttab aadeg3 aasch axedu axsch, b(%5.2f) se(%5.2f) star nonum nogap nomti

dis "pr2: " %5.2f `pr2adeg3' " " %5.2f `pr2asch' " " %5.2f `pr2xedu' ///
  " " %5.2f `pr2xsch'
  
dis "BIC: " %5.2f `bicadeg3' " " %5.2f `bicasch' " " %5.2f `bicxedu' ///
  " " %5.2f `bicxsch'
  
dis "ROC: " %5.2f `rocadeg3' " " %5.2f `rocasch' " " %5.2f `rocxedu' ///
  " " %5.2f `rocxsch'

 
*** Correlations
qui tab mmar1, gen(mm)
corr aedu asch xedu xsch
corr aedu asch mage1 mwht medu mnch mm1 mm2 mm3
corr aedu asch afem amar aliv asee atlk astr aclo


*** Models for depressive symptoms and ADL
* posting estimates for plotting
eststo clear
postutil clear
postfile cf v m e se using ~/desktop/d1, replace

local m1 adeg3
local m2 mage1 mwht i.mmar1 i.medu mnch afem amar
local m3 aliv asee atlk aclo astr

eststo d1: reg dep1 `m1', vce(robust)
mat t1 = r(table)
post cf (1) (1) (t1[1,1]) (t1[2,1])

eststo d2: reg dep1 `m1' `m2', vce(robust)
mat t2 = r(table)
post cf (1) (2) (t2[1,1]) (t2[2,1])

eststo d3: reg dep1 `m1' `m2' `m3', vce(robust)
mat t3 = r(table)
post cf (1) (3) (t3[1,1]) (t3[2,1])

logit adl1 `m1', vce(robust)
eststo a1: margins, dydx(*)
mat t4 = r(table)
post cf (2) (1) (t4[1,1]) (t4[2,1])

logit adl1 `m1' `m2', vce(robust)
eststo a2: margins, dydx(*)
mat t5 = r(table)
post cf (2) (2) (t5[1,1]) (t5[2,1])

logit adl1 `m1' `m2' `m3', vce(robust)
eststo a3: margins, dydx(*)
mat t6 = r(table)
post cf (2) (3) (t6[1,1]) (t6[2,1])

postclose cf

* tables of coefficients for appendix
esttab d1 d2 d3 using ~/desktop/dt.csv, replace b(%5.2f) se(%5.2f) star ///
  r2(%5.2f) nobase nogap nomti 
  
esttab a1 a2 a3 using ~/desktop/at.csv, replace b(%5.2f) se(%5.2f) star ///
  nobase nogap nomti 

* coefficient plots
preserve
use ~/desktop/d1, replace

gen ub = e + 1.96*se
gen lb = e - 1.96*se

twoway (rspike ub lb m if v == 1, hor xline(0, lc(black) lp(dash))) ///
  (scatter m e if v == 1), legend(off) xtit("unstandardized estimate") ///
  ylab(1 "M1" 2 "M2" 3 "M3", angle(h) grid gstyle(dot)) ///
  ytit(" ") xlab(, grid gstyle(dot)) tit("Depressive Symptoms") ///
  saving(~/desktop/g1, replace)
  
twoway (rspike ub lb m if v == 2, hor xline(0, lc(black) lp(dash))) ///
  (scatter m e if v == 2), legend(off) xtit("average marginal effect") ///
  ylab(1 "M1" 2 "M2" 3 "M3", angle(h) grid gstyle(dot)) ///
  ytit(" ") xlab(, grid gstyle(dot)) tit("Activity Limitation") ///
  saving(~/desktop/g2, replace)

graph combine ~/desktop/g1.gph ~/desktop/g2.gph
graph export ~/desktop/ceah-ms-fig2.pdf
restore

* predicted values
gen pipe = "|"
gen radeg3 = adeg3 + rnormal(0,0.005)
gen where1 = 1
gen where2 = 0

qui reg dep1 adeg3 mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk ///
  aclo astr, vce(robust)
margins, at(adeg3 = (0(0.1)1))
marginsplot , ytit("predicted value") ylab(1(0.2)2, angle(h) ///
  grid gstyle(dot)) xlab(, grid gstyle(dot)) recastci(rarea) ///
  ciopts(fintensity(30) lwidth(none)) title("Depressive Symptoms") ///
  xtit("proportion children with BA+") addplot(scatter where1 radeg3, ///
  ms(none) mlabel(pipe) mlabcol(red) mlabpos(0) xlab(0(.1)1) ///
  ylab(1(0.2)2) legend(off)) saving(~/desktop/g4, replace)
  
qui logit adl1 adeg3 mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk ///
  aclo astr, vce(robust)
margins, at(adeg3 = (0(0.1)1))
marginsplot , ytit("predicted probability") ylab(0(0.2)1, angle(h) ///
  grid gstyle(dot)) xlab(, grid gstyle(dot)) recastci(rarea) ///
  ciopts(fintensity(30) lwidth(none)) title("Activity Limitation") ///
  xtit("proportion children with BA+") addplot(scatter where2 radeg3, ///
  ms(none) mlabel(pipe) mlabcol(red) mlabpos(0) xlab(0(.1)1) ///
  ylab(0(0.2)1) legend(off)) saving(~/desktop/g5, replace)
  
graph combine ~/desktop/g4.gph ~/desktop/g5.gph
graph export ~/desktop/ceah-ms-fig3.pdf

forval i = 1/5 {
  capture erase ~/desktop/g`i'.gph
}



*** Models for interactions
local iv adeg3 mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk ///
  aclo astr

eststo clear
foreach x in c.aliv c.asee c.atlk c.aclo c.astr {
  qui eststo: reg dep1 `iv' c.adeg3#`x', vce(robust)
}
esttab using ~/desktop/t3.csv, replace b(%9.2f) se(%9.2f) star nomti nogap

eststo clear
foreach x in c.aliv c.asee c.atlk c.aclo c.astr {
  qui eststo: logit adl1 `iv' c.adeg3#`x', vce(robust)
}
esttab using ~/desktop/t4.csv, replace b(%9.2f) se(%9.2f) star nomti nogap




  

*** Auxiliary analyses
* 1. Does including income matter? No -- estimates are not significantly
*    different than in reported analysis
mi est: reg dep1 aedu mage1 mwht i.mmar1 i.medu minc1, vce(robust)
mi est: logit adl1 aedu mage1 mwht i.mmar1 i.medu minc1, vce(robust)

* 2. What predicts average child education? white, marital status, and education
reg aedu mage1 mwht i.mmar1 i.medu, vce(robust)

* 3. Interactions
eststo clear
local IVs aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk astr aclo
foreach x in i.medu i.mwht i.mmar1 c.mnch c.afem c.amar c.aliv c.asee c.atlk ///
  c.astr c.aclo {
    qui eststo: reg dep1 `IVs' c.aedu#`x', vce(robust)
}
esttab , b(%9.3f) se(%9.3f) star nomti nogap                        ///
  keep(aedu 1.medu#c.aedu 2.medu#c.aedu 3.medu#c.aedu 1.mwht#c.aedu ///
       2.mmar1#c.aedu 3.mmar1#c.aedu c.aedu#c.mnch c.aedu#c.afem    ///
	   c.aedu#c.amar c.aedu#c.aliv c.aedu#c.asee c.aedu#c.atlk      ///
	   c.aedu#c.astr c.aedu#c.aclo) title("Depressive symptoms interactions")

eststo clear
foreach x in i.medu i.mwht i.mmar1 c.mnch c.afem c.amar c.aliv c.asee c.atlk ///
  c.astr c.aclo {
    qui eststo: logit adl1 `IVs' c.aedu#`x', vce(robust)
}
esttab , b(%9.3f) se(%9.3f) star nomti nogap                        ///
  keep(aedu 1.medu#c.aedu 2.medu#c.aedu 3.medu#c.aedu 1.mwht#c.aedu ///
       2.mmar1#c.aedu 3.mmar1#c.aedu c.aedu#c.mnch c.aedu#c.afem    ///
	   c.aedu#c.amar c.aedu#c.aliv c.aedu#c.asee c.aedu#c.atlk      ///
	   c.aedu#c.astr c.aedu#c.aclo) title("Functional limitations interactions")

* 4. Collinearity diagnostics
qui reg dep1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk ///
  astr aclo, vce(robust)
vif

* 5. Check whether ACE predicts improvement or decline in ADL
logit adl2 aedu mage1 mwht i.mmar1 i.medu mnch afem amar asee atlk aliv astr ///
  aclo aedu if adl1 == 0, vce(robust)
logit adl2 aedu mage1 mwht i.mmar1 i.medu mnch afem amar asee atlk aliv astr ///
  aclo aedu if adl1 == 1, vce(robust)
  
* 6. Check whether nonlinear relationship with sex composition
recode afem (0 = 0) (0.01/0.99 = 1) (1 = 2), gen(cfem)
reg dep1 aedu mage1 mwht i.mmar1 i.medu mnch i.cfem amar aliv asee atlk ///
  astr aclo, vce(robust)
logit adl1 aedu mage1 mwht i.mmar1 i.medu mnch i.cfem amar aliv asee atlk ///
  astr aclo, vce(robust)

* 7. 
	   
	   
	   
	   
	   
*** Descriptive statistics (Table 1)
tab mmar1, gen(mmr)
tab medu, gen(med)

foreach x of varlist dep1 dep2 adl1 adl2 mage1 mwht mmr1-mmr3 med1-med4 ///
  minc1 mnch afem amar aliv asee atlk astr aclo addp acre agtm agvm aedu {
  qui sum `x'
  dis "`x'   " %5.2f r(mean) "  " %5.2f r(sd) "  " %2.0f r(min) "-" %2.0f r(max)
}


*** Auxiliary descriptive statistics
corr dep1 dep2 adl1 adl2
corr dep1 adl1 aedu
corr mage1 mwhy mmr1-mmr3 med1-med3 minc1 
corr mnch afem amar aliv asee atlk aedu

recode aedu (0 = 0) (0.01/0.99 = 1) (1 = 2), gen(cedu)

table medu, c(mean dep1 mean adl1)
table cedu, c(mean dep1 mean adl1)

table medu if adl1 == 0, c(freq mean adl2)
table cedu if adl1 == 0, c(freq mean adl2)

table medu if adl1 == 1, c(freq mean adl2)
table cedu if adl1 == 1, c(freq mean adl2)


  
  
* 8. check for difference in highest educated son vs highest educated daughter
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch amar asee atlk aliv fmedu ///
  if cfem == 1, vce(robust)
  
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch amar asee atlk aliv mledu ///
  if cfem == 1, vce(robust)

logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch amar asee atlk aliv fmedu ///
  if cfem == 1, vce(robust)

logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch amar asee atlk aliv mledu ///
  if cfem == 1, vce(robust)
