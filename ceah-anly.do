*** Purpose: analysis of children's education and mother's health
*** Author: S Bauldry
*** Date: February 1, 2018

*** Set working directory and load data
cd ~/dropbox/research/hlthineq/mgah/ceah/ceah-work
use ceah-data, replace


*** Descriptive statistics (Table 1)
tab mmar1, gen(mmr)
tab medu, gen(med)

foreach x of varlist dep1 dep2 adl1 adl2 mage1 mwht mmr1-mmr3 med1-med4 ///
  minc1 mnch afem amar aliv asee atlk aedu {
  qui sum `x'
  dis "`x'  " %5.2f r(mean) "  " %5.2f r(sd) "  " %2.0f r(min) "-" %2.0f r(max)
}


*** Auxiliary descriptive statistics
corr dep1 dep2 adl1 adl2
corr dep1 adl1 aedu
corr mage1 mwhy mmr1-mmr3 med1-med3 minc1 
corr mnch afem amar aliv asee atlk aedu


*** Regression models (Table 2)
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv aedu, ///
  vce(robust)
eststo dep1

reg dep2 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu dep1 if t2nm, vce(robust)
eststo dep2
  
logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu, vce(robust)
eststo adl1

logit adl2 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu adl1 if t2nm, vce(robust)
eststo adl2

esttab dep1 dep2 adl1 adl2 using ~/desktop/Table2.csv, replace b(%5.3f) ///
  se(%5.3f) nobase nonum 
  

*** Post-estimation for depression and ADL at T1 (Figure 1)
tempfile g1 g2
qui reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu, vce(robust)
margins , at(aedu = (0(0.1)1))
marginsplot , ytit("predicted value of symptoms") ylab(1(0.2)2, angle(h) ///
  grid gstyle(dot)) xlab(, grid gstyle(dot)) recastci(rarea)             ///
  ciopts(fintensity(30) lwidth(none)) title("Depressive Symptoms")       ///
  xtit("proportion of children with BA+") saving(`g1')

qui logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu, vce(robust)
margins , at(aedu = (0(0.1)1))
marginsplot , ytit("predicted probability of limitation")                  ///
  ylab(0(0.1)0.6, angle(h) grid gstyle(dot)) xlab(, grid gstyle(dot))      ///
  recastci(rarea) ciopts(fintensity(30) lwidth(none))                      ///
  title("Any Activity Limitation") xtit("proportion of children with BA+") ///
  saving(`g2')
  
graph combine "`g1'" "`g2'"
graph export ~/desktop/Fig1.pdf, replace


*** Auxiliary analyses
* 1. predictors of proportion of children with BA+
reg aedu mage1 mwht i.mmar1 i.medu minc1 mnch


* 2. alternative measures of children's education
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv xedu
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv nedu

logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv xedu
logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv nedu


* 3. interactions
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu i.mwht#c.aedu
  
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu i.medu#c.aedu
  
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.mnch#c.aedu
  
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.asee#c.aedu
  
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.atlk#c.aedu
  
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.aliv#c.aedu
  

logit adl1 mage1 i.mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu i.mwht#c.aedu

logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu i.medu#c.aedu
  
logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.mnch#c.aedu
  
logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.asee#c.aedu
  
logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.atlk#c.aedu
  
logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.aliv#c.aedu


* 4. collinearity diagnostics
qui reg dep2 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu dep1 adl1
vif


* 5. check unusual relationship between mother's education and ADL
logit adl1 i.medu
logit adl1 i.medu aedu
logit adl1 i.medu aedu minc1 

