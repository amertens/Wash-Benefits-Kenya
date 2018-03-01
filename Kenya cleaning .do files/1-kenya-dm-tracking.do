*capture log close
*set more off
*clear all


*log using "~/1-kenya-dm-tracking.log", text replace

*--------------------------------------------
* 1-kenya-dm-diar.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the kenya household tracking dataset and treatment assignment document
* to create a household tracking dataset by treatment arms
*
*--------------------------------------------




*--------------------------------------------
* input files:
*
*  Treatment assignments (washb-kenya-tr)
*
*  
*  Household tracking (msP_household_tracking_20161006)
*
* output files:
*  final/washb-kenya-tracking.dta / .csv
*
*--------------------------------------------



*--------------------------------------------
* format the treatment assignment information
*--------------------------------------------
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/tr/washb-kenya-tr.dta", clear

destring clusterid, replace
sort clusterid
tempfile trdata
save `trdata'


*--------------------------------------------
* merge the tracking and treatment data
*--------------------------------------------
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/msP_household_tracking_20161006.dta", clear

sort clusterid
merge m:1 clusterid using `trdata'
keep if _merge==3
drop _merge


*--------------------------------------------
* format dates
*--------------------------------------------
format ms_bl_date ms_ml_up_date ms_ml_ad_date ms_ml_am_date ms_el_up_date ms_el_ad_date ms_el_am_date %d

*--------------------------------------------
* Save the tracking document with treatment arms
*--------------------------------------------

label data "Kenya household tracking dataset (HH obs), created by 1-kenya-dm-tracking.do"
saveold "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-tracking.dta", replace version(12)
outsheet using "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-tracking.csv", comma replace

*save to shared folder
saveold "~/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-tracking.dta", replace version(12)
outsheet using "~/dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-tracking.csv", comma replace


clear


