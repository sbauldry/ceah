*** Purpose: analysis of children's education and mother's health
*** Author: S Bauldry
*** Date: February 1, 2018

*** Set working directory and load data
cd ~/dropbox/research/hlthineq/mgah/mgacm/mgacm-work/mgacm-anly
use mgacm-wfds-data, replace
eststo clear


*** Descriptive statistics




*** Primary analysis
ologit srh1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv aedu
eststo srh1

ologit srh2 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu srh1
eststo srh2
  
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv aedu
eststo dep1

reg dep2 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu dep1
eststo dep2
  
logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv aedu
eststo adl1

logit adl2 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu adl1
eststo adl2

esttab srh1 srh2 dep1 dep2 adl1 adl2 using Table2.csv, b(%5.3f) se(%5.3f) ///
  nobase nonum

  
*** Post-estimation for depression and ADL at T1  
tempfile g1 g2
qui reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv aedu
margins , at(aedu = (0(0.1)1))
marginsplot , ytit("predicted value of depression") ylab(1(0.2)2, angle(h) ///
  grid gstyle(dot)) xlab(, grid gstyle(dot)) recastci(rarea) ///
  ciopts(fintensity(30) lwidth(none)) title("Depression") ///
  xtit("proportion of children with BA+") saving(`g1')

qui logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu
margins , at(aedu = (0(0.1)1))
marginsplot , ytit("predicted probability of ADL") ylab(0(0.1)0.6, angle(h) ///
  grid gstyle(dot)) xlab(, grid gstyle(dot)) recastci(rarea) ///
  ciopts(fintensity(30) lwidth(none)) title("ADL") ///
  xtit("proportion of children with BA+") saving(`g2')
  
graph combine "`g1'" "`g2'"
graph export Fig1.pdf, replace


*** Auxiliary analyses
* 1. predictors of proportion of children with BA+
reg aedu mage1 mwht i.mmar1 i.medu minc1 mnch

* 2. correlations among family context measures
corr aedu afem amar asee atlk aliv

* 3. alternative measures of children's education
ologit srh1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv xedu
ologit srh1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv nedu

reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv xedu
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv nedu

logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv xedu
logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv nedu

* 4. interactions
ologit srh1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu i.medu#c.aedu
  
ologit srh1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.mnch#c.aedu
  
ologit srh1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.asee#c.aedu
  
ologit srh1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.atlk#c.aedu
  
ologit srh1 mage1 mwht i.mmar1 i.medu minc1 mnch afem amar asee atlk aliv ///
  aedu c.aliv#c.aedu
  
  
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




