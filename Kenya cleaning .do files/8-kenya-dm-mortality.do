clear all


*--------------------------------------------
* format the treatment assignment information
*--------------------------------------------
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/tr/washb-kenya-tr.dta", clear

destring clusterid, replace
sort clusterid
tempfile trdata
save `trdata'

*--------------------------------------------
* baseline child data to match cluster/childid
*--------------------------------------------
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/msP_child_IDchar_20161103.dta", clear

*format childid
format childid %9.0f
keep childid clusterid
sort childid

tempfile id 
save `id'
*--------------------------------------------
* load the mortality dataset
*--------------------------------------------
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/Endline/msP_mortality_20161103.dta", clear

*format childid
format childid %9.0f
*merge in clusterid 
sort childid
merge 1:1 childid using `id'
tab _merge
drop if _merge!=3
drop _merge

*merge in treatment data
sort clusterid
merge m:1 clusterid using `trdata'
tab _merge
drop _merge

*Drop those lost in pregnancy
keep if pregloss==2

*Convert death to 0,1 indicator
tab childdeath
gen byte death= childdeath==1
tab death

drop pregloss childdeath

outsheet using "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-mortality.csv", comma replace
