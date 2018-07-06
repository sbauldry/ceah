*** Purpose: analysis of children's education and mother's health
*** Author: S Bauldry
*** Date: February 1, 2018

*** Set working directory and load data
cd ~/dropbox/research/hlthineq/mgah/ceah/ceah-work
use ceah-data, replace


*** Examining different measures of adult children's education
eststo clear
eststo d1: reg dep1 adeg2 adeg3, vce(robust)
eststo d2: reg dep1 asch, vce(robust)
eststo d3: reg dep1 xdeg, vce(robust)
eststo d4: reg dep1 xsch, vce(robust)
esttab d1 d2 d3 d4, b(%5.3f) se(%5.3f) star r2(%5.3f) bic(%5.3f)

eststo clear
eststo a1: logit adl1 adeg2 adeg3, vce(robust)
eststo a2: logit adl1 asch, vce(robust)
eststo a3: logit adl1 xdeg, vce(robust)
eststo a4: logit adl1 xsch, vce(robust)
esttab a1 a2 a3 a4, b(%5.3f) se(%5.3f) star pr2(%5.3f) bic(%5.3f) eform



  
  
*** Correlations
qui tab mmar1, gen(mm)
corr aedu asch xedu xsch
corr aedu asch mage1 mwht medu mnch mm1 mm2 mm3
corr aedu asch afem amar aliv asee atlk astr aclo


*** Models for depressive symptoms and ADL
preserve
keep if !mi(mwht, medu, astr)
eststo clear
qui eststo d1: reg dep1 aedu, vce(robust)
qui eststo d2: reg dep1 aedu mage1 mwht i.mmar1 i.medu, vce(robust)
qui eststo d3: reg dep1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv ///
  asee atlk astr aclo, vce(robust)
qui eststo d4: reg dep2 dep1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar ///
  aliv asee atlk astr aclo, vce(robust)

qui eststo a1: logit adl1 aedu, vce(robust)
qui eststo a2: logit adl1 aedu mage1 mwht i.mmar1 i.medu, vce(robust)
qui eststo a3: logit adl1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv ///
  asee atlk astr aclo, vce(robust)
qui eststo a4: logit adl2 adl1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar ///
  aliv asee atlk astr aclo, vce(robust)
  
esttab d1 d2 d3 d4, b(%5.3f) se(%5.3f) star r2(%5.3f) aic(%5.3f) ///
  bic(%5.3f) nobase nogap nomti title("Table 2: Depressive symptoms and ACE")

esttab a1 a2 a3 a4, eform b(%5.3f) se(%5.3f) star r2(%5.3f) aic(%5.3f) ///
  bic(%5.3f) nobase nogap nomti title("Table 2: Functional limitations and ACE")
restore


*** Predicted values for depressive symptoms and ADL
tempfile g1 g2
qui reg dep1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk ///
  astr aclo, vce(robust)
margins , at(aedu = (0(0.1)1))
marginsplot , ytit("predicted value of symptoms") ylab(1(0.2)2, angle(h) ///
  grid gstyle(dot)) xlab(, grid gstyle(dot)) recastci(rarea)             ///
  ciopts(fintensity(30) lwidth(none)) title("Depressive Symptoms")       ///
  xtit("proportion of children with BA+") saving(`g1')

qui logit adl1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk ///
  astr aclo, vce(robust)
margins , at(aedu = (0(0.1)1))
marginsplot , ytit("predicted probability of limitation")                  ///
  ylab(0(0.1)0.6, angle(h) grid gstyle(dot)) xlab(, grid gstyle(dot))      ///
  recastci(rarea) ciopts(fintensity(30) lwidth(none))                      ///
  title("Any Functional Limitation") xtit("proportion of children with BA+") ///
  saving(`g2')
  
graph combine "`g1'" "`g2'"
graph export ~/desktop/Fig1.pdf, replace
  

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
