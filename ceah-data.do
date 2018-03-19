*** Purpose: prepare data for analysis of children's educ and mother's health
*** Author: S Bauldry
*** Date: February 1, 2018

*** Note: original data file names: 
* mom dyad level T1 Full Sample with mortality 8-1-2016.sent to Peng 1-2-18
* mom re kid T1T2 for dropbox.n1514.MGJS.07-25-2016

*** Loading data provided by Peng
cd ~/dropbox/research/hlthineq/mgah/ceah/ceah-work
use ceah-wfds-t1-data, replace

*** ID variables
rename (LSUID kidid) (mid cid)

*** Children's characteristics
rename (sex) (cfem)
recode educat (9 888 9997 9999 = .) (1/5 = 0) (6 7 = 1), gen(cedu)
recode x141 (1 2 = 1) (3/6 = 0) (9 = .), gen(cmar)
recode within2 (888 9997 9998 9999 = .), gen(cliv)
recode freqsee freqtalk (7 = 0) (6 = 1) (5 = 2) (4 = 3) (3 = 4) (2 = 5) ///
  (1 = 6) (888 9997 9998 9999 = .), gen(csee ctlk)
recode mtensenw mclosenw (888 9997 9998 9999 = .), gen(cstr cclo)

*** Mother's characteristics
rename (numalive) (mnch)
recode f1 (1 = 1) (4 6 = 2) (5 = 3), gen(mmar1)
gen mage1 = 2002 - g1y
recode mastrace (0 = 1) (1/23 = 0), gen(mwht)
recode g3 (9 = .) (1 2 = 0) (3 = 1) (4 5 = 2) (6 7 = 3), gen(medu)
recode income2 (1 = .5) (2 = 1.5) (3 = 2.5) (4 = 3.5) (5 = 4.5) (6 = 6.25) ///
  (7 = 10) (9 = .), gen(minc1)

*** Mother's health (T1)
recode health1 (-3 = .), gen(srh1)
recode e8a e8b e8c e8d e8e e8f e8g (9 = .)
alpha e8a e8b e8c e8d e8e e8f e8g, gen(dep1)
recode e2 (5 = 0), gen(adl1)

*** Mediators
recode got_ gave_ gotsick gotcomf (5 = 0) (8 9 9999 = .), ///
  gen(cgtm cgvm csck ccmf)
rename (mdrinkdr) (cddp)
gen ccre = (csck == 1 | ccmf == 1)
replace ccre = . if mi(csck) & mi(ccmf)

lab var mid   "mother id"
lab var cid   "child id"
lab var cfem  "child female"
lab var cedu  "child BA+"
lab var cmar  "child married"
lab var cliv  "child live within 2 hours"
lab var csee  "child frequency visit"
lab var ctlk  "child frequency talk"
lab var cstr  "child strain"
lab var cclo  "child closeness"
lab var cgtm  "child got money"
lab var cgvm  "child received money"
lab var cddp  "child drink/drug problem"
lab var csck  "child provided help when sick"
lab var ccmf  "child provided comfort"
lab var ccre  "child provided help or comfort"
lab var mnch  "mother # children"
lab var mage1 "mother age 1"
lab var mmar1 "mother marital status 1"
lab var mwht  "mother white"
lab var medu  "mother education"
lab var minc1 "mother household income 1"
lab var srh1  "mother srh 1"
lab var dep1  "mother depression 1 (a = 0.78)"
lab var adl1  "mother adl 1"

tab csck ccmf, m

*** Keep analysis variables
order mid cid cfem cedu cmar cliv csee ctlk cstr cclo mnch mage1 mmar1 mwht ///
  medu minc1 cgtm cgvm cddp csck ccmf ccre srh1 dep1 adl1
keep mid-adl1
tempfile d1
save `d1', replace


*** Merging T2 data
use ceah-wfds-t2-data, replace

*** ID variables
rename (LSUID kidid) (mid cid)

*** Mother's health (T2)
rename (D2T2 momhealtht2) (adl2 srh2)
recode D15aT2 D15bT2 D15cT2 D15dT2 D15eT2 D15fT2 D15gT2 (9 = .)
alpha D15aT2 D15bT2 D15cT2 D15dT2 D15eT2 D15fT2 D15gT2, gen(dep2)

lab var mid   "mother id"
lab var cid   "child id"
lab var srh2  "mother srh 2"
lab var dep2  "mother depression 2 (a = 0.82)"
lab var adl2  "mother adl 2"

order mid cid srh2 dep2 adl2
keep mid-adl2

merge 1:1 mid cid using `d1'
drop if _merge == 1 // discard 5 mothers (24 children) in T2 but not in T1
gen t2 = ( _merge == 3 )
drop _merge

*** Generate children context measures
egen aedu  = mean(cedu), by(mid)
egen xedu  = max(cedu), by(mid)
egen nedu  = min(cedu), by(mid)

egen fmedu = mean(cedu) if cfem == 1, by(mid)
egen mledu = mean(cedu) if cfem == 0, by(mid)

foreach x in fem mar see tlk liv str clo gtm gvm ddp cre {
  egen a`x' = mean(c`x'), by(mid)
}

lab var t2    "in T2"
lab var aedu  "pr of children with BA+"
lab var xedu  "at least one child with BA+"
lab var nedu  "all children with BA+"
lab var fmedu "pr of daughters with BA+"
lab var mledu "pr of sons with BA+"
lab var afem  "pr of children female"
lab var amar  "pr of children married"
lab var asee  "avg frequency of visit"
lab var atlk  "avg frequenc of talk"
lab var aliv  "pr of children live within 2 hours"
lab var astr  "avg strain"
lab var aclo  "avg closeness"
lab var agtm  "pr of children received money from"
lab var agvm  "pr of children gave money to"
lab var addp  "pr of children drink/drug problem"
lab var acre  "pr of children provided help or comfort"

*** Prepare mother data for analysis
egen pone = tag(mid)
keep if pone
order mid t2 mnch mage1 mmar1 minc1 mwht medu srh1 srh2 dep1 dep2 adl1 adl2 ///
  aedu xedu nedu fmedu mledu afem amar asee atlk aliv astr aclo agtm agvm   ///
  addp acre
keep mid-acre

*** Running multiple imputation for missing wave 1 measures
mi set wide
mi reg imp mage1 mmar1 minc1 mwht medu srh1 srh2 dep1 dep2 adl1 adl2 amar ///
  astr agtm agvm acre
mi imp chain (mlogit) mmar1 (ologit) medu srh1 srh2 (logit) mwht adl1 adl2  ///
  (regress) mage1 minc1 dep1 dep2 amar astr agtm agvm acre = mnch aedu xedu ///
  nedu afem asee atlk aliv aclo addp, add(20) augment rseed(91169)
save ceah-mi-data, replace
