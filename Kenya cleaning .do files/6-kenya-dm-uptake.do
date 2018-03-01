capture log close
set more off
clear all


log using "C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/6-kenya-dm-uptake.log", text replace

*--------------------------------------------
* 6-kenya-dm-uptake.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the Kenya trial uptake
* data for analysis
*
* note: unlike other dm scripts, this script
* relies on an earlier dm script (3-kenya-dm-diar.do)
* to grab child ages at survey measurements
* rather than re-calculate them here
*
*--------------------------------------------




*--------------------------------------------
* input files:
*
*  1. WASHB_Baseline_main_survey.dta
*  1. WASHB_Midline_main_survey_cleaned.dta
* 04. WASHB_Endline_main_survey_cleaned.dta
* 11. WASHB_Endline_LNS_cleaned.dta
*
* 02. WASHB_Endline_Arm_Identification.dta (real treatment assignments, after unblinding)
*
*  washb-kenya-diar.dta
*
* output files:
*  final/washb-kenya-uptake.dta / .csv
*--------------------------------------------

*--------------------------------------------
* Set working directory
*--------------------------------------------
cd "~/Dropbox/WBK-primary-analysis/Data/"

*--------------------------------------------
* format the treatment assignment information
*--------------------------------------------
use "Untouched/tr/washb-kenya-tr.dta", clear
destring clusterid, replace
tempfile trdata
save `trdata'



*--------------------------------------------
* load index child ages at each survey
* to subset nutrition uptake measures to 
* children 6-24 months old
*--------------------------------------------
*use "Final/Andrew/washb-kenya-diar.dta", clear
use "Untouched/msP_child_IDchar_20161006.dta", clear
* restrict to index children (study children+twins)
*keep if childid==1|childid==2
 
* restrict to target children 
keep if childtype==1
duplicates list hhid
keep hhid childid DOB 
sort hhid 
tempfile DOB
save `DOB' 




**********************************************************
*Figure 2: Intervention compliance
**********************************************************






*--------------------------------------------
* Load in household survey
*--------------------------------------------

use "~/Dropbox/WBK-primary-analysis/Data/Untouched/Baseline/msP_bl_append_ID_clean_20161010.dta", clear
sort  hhid

*merge in treatment data
sort clusterid
merge m:1 clusterid using `trdata'
drop _merge



*--------------------------------------------
* Uptake indicators
*--------------------------------------------

* visited by promoter in past month
gen byte promoter_vis=.

	
* store water with detectable free chlorine 
gen byte freechl = c1015a>0 & c1015a!=99.9
	replace freechl = 0 if c1015a==0 | c1005==2 | (c1005==1 & c100601!=1 & c100601!=2 & c100601!=11 & c100601!=12 & c100601!=13 &  c100602!=1 & c100602!=2 & c100602!=11 & c100602!=12 & c100602!=13)
	replace freechl = . if c1003_1==2 | c1014!=1| c1015a==99.9 | c1023==11 | c1023==12 
	label var freechl "Free chlorine detected in stored water (>0 mg/L)"
	tab freechl tr

	
	
* access to improved latrine
*  access to improved latrine
gen byte impr_lat = (c806_7==1 | c806_5==1)
	replace impr_lat=. if (c806_7==99 & c806_5==.) | (c806_7==. & c806_5==.)
	replace impr_lat=. if (to1b==2 | to1b==3)
	replace impr_lat=0 if c805==2
	
	label var impr_lat "Access to improved latrine"
tab impr_lat

* child feces safetly disposed
gen byte ch_feces_safe_disp= (c903_intoilet==1 | c903_diaper==1 | ((c903_potty_incourt==1 | c903_potty_inhouse==1) & (c904==2 | c904==7)))
	replace ch_feces_safe_disp=. if c903_intoilet==. & c903_diaper==. & c903_potty_incourt==. & c903_potty_inhouse==. & (c904==.|c904==99) 
	*EXCLUDE ALL HOUSEHOLDS ENROLLED IN THE PARASITE SUBSAMPLE since they were given nappies for stool collection
	*merge in Household tracking document for this. 
	merge 1:1 hhid using "Untouched/msP_household_tracking_20161006.dta"
	assert _merge==3 |_merge==2
	keep if _merge==3
	drop _merge
	replace ch_feces_safe_disp=. if parasite_bl==1
tab  ch_feces_safe_disp

	
* primary handwashing station has water and soap
gen byte watsoap_avail=(c702c_water==1 & (c702c_scentbar==1 | c702c_unscentbar==1 | c702c_powder==1 | c702c_soapywat==1)) | ( c703c_water==1 & (c703c_scentbar==1 | c703c_unscentbar==1 | c703c_powder==1 | c703c_soapywat==1))
	replace watsoap_avail=. if (c702c_water==. & (c702c_scentbar==. & c702c_unscentbar==. & c702c_powder==. & c702c_soapywat==.)) & ( c703c_water==. & (c703c_scentbar==. & c703c_unscentbar==. & c703c_powder==. & c703c_soapywat==.))
	*replace watsoap_avail=. if wat_avail==. & soap_avail==.
	label var watsoap_avail "Water and soap within 2 m of latrine or kitchen"
tab watsoap_avail

tempfile uptakeBL
save `uptakeBL' 


* mean sachels of LNS fed in prior week to index child 6-24 mos (not at baseline)









*--------------------------------------------
* load the year 1 dataset
*--------------------------------------------

use "Untouched/Midline/msP_ml_append_ID_clean_uptake_20161015.dta", clear

* format survey dates
gen svydate = ms_ml_up_date
	format svydate %d
	label var svydate "Survey date"
	codebook svydate

* identify survey round
gen studyyear = 1
	label var svy "Survey round (0,1,2)"
	
order hhid studyyear svydate
sort hhid



*--------------------------------------------
* Merge Treatment Assignment
*--------------------------------------------
sort clusterid
*capture drop clusterid block tr
merge m:1 clusterid using `trdata'
*assert _merge == 3
keep if _merge==3
drop _merge

*--------------------------------------------
* Uptake indicators
*--------------------------------------------


* visited by promoter in past month
gen byte promoter_vis= (f2==1)
	replace promoter_vis=. if inlist(f2,99,.,88)
	replace promoter_vis=0 if tr==8
	label var promoter_vis "visited by promoter in past month"
tab promoter_vis


* store water with detectable free chlorine (not at baseline)
gen byte freechl = c1015a>0 & c1015a!=99.9 & c1015a!=999 
	replace freechl = 0 if c1015a==0 | c1005==2 | (c1005==1 & c1006_cldisp!=1 & c1006_botcl!=1 & c1006_pur!=1 & c1006_aqua!=1 & c1006_cl!=1)
	replace freechl = . if c1003_1==2 | c1014!=1| c1015a==99.9 | c1015a==999
	label var freechl "Free chlorine detected in stored water (>0 mg/L)"
tab freechl


* access to improved latrine
gen byte impr_lat = (c806_7==1 | c806_5==1)
	replace impr_lat = . if (c806_7==. | c806_5==.)
	replace impr_lat=0 if c8052==4
	replace impr_lat=. if (c8052==2 | c8052==3)
	replace impr_lat=. if inlist(c8052,99,.,88)
	label var impr_lat "Access to improved latrine"
tab impr_lat


	
* child feces safetly disposed
gen byte ch_feces_safe_disp= c903_intoilet==1 |( c903_diaper==1 & c904==2) | ((c903_pottyincourt==1 | c903_pottyinhouse==1) & (c904==2 | c904==7))
	replace ch_feces_safe_disp=. if c903_intoilet==. & c903_diaper==. & c903_pottyincourt==. & c903_pottyinhouse==. & (c904==.|c904==99) 
	*EXCLUDE ALL HOUSEHOLDS ENROLLED IN THE PARASITE SUBSAMPLE since they were given nappies for stool collection
	*merge in Household tracking document for this. 
	merge 1:1 hhid using "Untouched/msP_household_tracking_20161006.dta"
	assert _merge==3 |_merge==2
	keep if _merge==3
	drop _merge
	replace ch_feces_safe_disp=. if parasite_bl==1

	tab ch_feces_safe_disp
	

	
* primary handwashing station has water and soap
gen byte watsoap=(c702c_water==1 & (c702c_scentbar==1 | c702c_unscentbar==1 | c702c_powder==1 | c702c_soapywat==1)) | ( c703c_water==1 & (c703c_scentbar==1 | c703c_unscentbar==1 | c703c_powder==1 | c703c_soapywat==1)) 
	replace watsoap=. if (c702c_water==. & (c702c_scentbar==. & c702c_unscentbar==. & c702c_powder==. & c702c_soapywat==.)) & ( c703c_water==. & (c703c_scentbar==. & c703c_unscentbar==. & c703c_powder==. & c703c_soapywat==.)) 
gen byte tippytap=(tt702b1==1 & tt702d==1 & tt702f==1) | (tt703b1==1 & tt703d==1 & tt703f==1)
	replace tippytap=. if (tt702b1==. & tt702d==. & tt702f==.) & (tt703b1==. & tt703d==. & tt703f==.)
	
	
gen byte watsoap_avail = (watsoap==1 | tippytap==1)
	replace watsoap_avail =. if watsoap==. & tippytap==.
	label var watsoap_avail "Water and soap within 2 m of latrine or kitchen"


* mean sachets of LNS reported fed in prior week to index child 6-24 mos
	* merge in ages of children
	merge 1:1 hhid using `DOB'
	*assert _merge==3
	tab _merge
	keep if _merge==3|_merge==1
	drop _merge
	
	gen aged = svydate-DOB
	label var aged "Age in days (uptake meas)"
	
	gen double agem = aged/30.4167
	label var agem "Age in months (anthro meas)"
	
	* Sachets consumed per week
	gen lnsn = (c1408*c1409)
	replace lnsn = . if (agem < 6) | (agem>24)
	label var lnsn "LNS sachets consumed per week"
 
	* percent of expected
	gen lnsp = (lnsn/14)
	label var lnsp "Percent of expected LNS sachets consumed (reported)"
	
	list c1408 c1409 lnsn if c1409>2 & c1409<.




label var hhid "Household ID"

* restrict to household level variables used in the analysis
* and save the data
keep hhid clusterid block tr studyyear svydate promoter_vis freechl impr_lat ch_feces_safe_disp watsoap_avail lnsn lnsp
*order dataid clusterid block tr studyyear svydate storewat freechl latseal latfeces humfeces hwsw hwss hwsws *lns*


*save a temporary midline file
	tempfile uptakeML
	save `uptakeML'




*--------------------------------------------
* load the year 2 main survey dataset
*--------------------------------------------

use "Untouched/Endline/msP_el_append_ID_clean_uptake_20161015.dta", clear

* format survey dates
rename ms_el_up_date svydate 
	format svydate %d
	label var svydate "Survey date"
	codebook svydate

	
* identify survey round
gen studyyear = 2
	label var studyyear "Survey round (0,1,2)"
	
order hhid svy svydate
sort hhid




*--------------------------------------------
* Merge Treatment Assignment
*--------------------------------------------
sort clusterid
*capture drop clusterid block tr
merge m:1 clusterid using `trdata'
*assert _merge == 3
keep if _merge==3
drop _merge

*--------------------------------------------
* Uptake indicators
*--------------------------------------------



* visited by promoter in past month
gen byte promoter_vis= (f2==1)
	replace promoter_vis=. if inlist(f2,99,.,88)
	replace promoter_vis=0 if tr==8
	label var promoter_vis "visited by promoter in past month"
	tab promoter_vis
	
	
* store water with detectable free chlorine (not at baseline)
gen byte freechl = c1015a>0 & c1015a!=99.9 & c1015a!=999 
	replace freechl = 0 if c1015a==0 | c1005==2 | (c1005==1 & c1006_cldisp!=1 & c1006_botcl!=1 & c1006_pur!=1 & c1006_aqua!=1 & c1006_cl!=1)
	replace freechl = . if c1003_1==2 | c1014!=1| c1015a==99.9 | c1015a==999
	label var freechl "Free chlorine detected in stored water (>0 mg/L)"
tab freechl

	
* access to improved latrine
gen byte impr_lat = (c806_7==1 | c806_5==1)
	replace impr_lat = . if (c806_7==. | c806_5==.)
	replace impr_lat=0 if c8052==4
	replace impr_lat=. if (c8052==2 | c8052==3)
	replace impr_lat=. if inlist(c8052,99,.,88)

	label var impr_lat "Access to improved latrine"
	
	tab impr_lat

* child feces safetly disposed
gen byte ch_feces_safe_disp= c903==7 | (c903==8 & c904==2) | ((c903 ==1 | c903 ==2) & (c904==2 | c904==7))
	replace ch_feces_safe_disp=. if inlist(c903,99,.,88) & inlist(c904,99,.,88) 
	*EXCLUDE ALL HOUSEHOLDS ENROLLED IN THE PARASITE SUBSAMPLE since they were given nappies for stool collection
	*merge in Household tracking document for this. 
	merge 1:1 hhid using "Untouched/msP_household_tracking_20161006.dta"
	assert _merge==3 |_merge==2
	keep if _merge==3
	drop _merge
	replace ch_feces_safe_disp=. if parasite_bl==1

	tab ch_feces_safe_disp
	
* primary handwashing station has water and soap
gen byte watsoap=(c702c_water==1 & (c702c_scentbar==1 | c702c_unscentbar==1 | c702c_powder==1 | c702c_soapywat==1)) | ( c703c_water==1 & (c703c_scentbar==1 | c703c_unscentbar==1 | c703c_powder==1 | c703c_soapywat==1)) 
tab tr watsoap 
	replace watsoap=. if (c702c_water==. & (c702c_scentbar==. & c702c_unscentbar==. & c702c_powder==. & c702c_soapywat==.)) & ( c703c_water==. & (c703c_scentbar==. & c703c_unscentbar==. & c703c_powder==. & c703c_soapywat==.)) 
tab tr watsoap 

	gen byte tippytap=(tt702b1==1 & tt702d==1 & tt702f==1) | (tt703b1==1 & tt703d==1 & tt703f==1)
tab tr tippytap 

	replace tippytap=. if tt702b1==. & tt702d==. & tt702f==. & tt703b1==. & tt703d==. & tt703f==.
tab tr tippytap 
tab tr 

	
gen byte watsoap_avail = (watsoap==1 | tippytap==1)
	replace watsoap_avail =. if watsoap==. & tippytap==.
	label var watsoap_avail "Water and soap within 2 m of latrine or kitchen"
tab tr watsoap 
tab tr tippytap 
tab tr watsoap_avail 

	


* mean sachets of LNS reported fed in prior week to index child 6-24 mos
	* merge in ages of children
	merge 1:1 hhid using `DOB'
	*assert _merge==3
	keep if _merge==3|_merge==1
	drop _merge
	
	gen aged = svydate-DOB
	label var aged "Age in days (uptake meas)"
	
	gen double agem = aged/30.4167
	label var agem "Age in months (anthro meas)"
	
	* Sachets consumed per week
	gen lnsn = (c1408*c1409)
	replace lnsn = . if (agem < 6) | (agem>24)
	label var lnsn "LNS sachets consumed per week"
 
	* percent of expected
	gen lnsp = (lnsn/14)
	label var lnsp "Percent of expected LNS sachets consumed (reported)"
	
	list c1408 c1409 lnsn if c1409>2 & c1409<.
	mean lnsp
	
	*Keep relavent variables for append 
	keep hhid clusterid block tr studyyear svydate promoter_vis freechl impr_lat ch_feces_safe_disp watsoap_avail lnsn lnsp

	*drop accidentally visited passive control compounds (uptake erroniously administered during attrition followup)
	drop if tr==8
	tab tr
	
	*save a temporary midline file
	tempfile uptakeEL
	save `uptakeEL'


*--------------------------------------------
* Save an uptake analysis dataset
* compound-survey level observations
*--------------------------------------------	

*--------------------------------------------
* Load in baseline uptake
*--------------------------------------------
*use "Final/Andrew/washb-kenya-enrol.dta"
use `uptakeBL'
keep hhid clusterid block tr promoter_vis freechl impr_lat ch_feces_safe_disp watsoap_avail 

*Generate uptake variables missing from baseline
generate studyyear=0
generate lnsn=.
generate lnsp=.


append using `uptakeML'
append using `uptakeEL'

	

	
	
compress
sort hhid studyyear
label data "Kenya uptake analysis dataset (compound-svy obs), created by 6-kenya-dm-uptake.do"
saveold "~/dropbox/WBK-primary-analysis/data/Final/Andrew/washb-kenya-uptake.dta", replace version(12)
outsheet using "~/dropbox/WBk-primary-analysis/data/Final/Andrew/washb-kenya-uptake.csv", comma replace



* write a codebook for the dataset
log close
log using "~/dropbox/WBK-primary-analysis/data/Final/Andrew/washb-kenya-uptake-codebook.txt", text replace
desc
codebook, c
codebook
log close


exit

