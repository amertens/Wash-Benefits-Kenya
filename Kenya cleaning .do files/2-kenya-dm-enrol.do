capture log close
set more off
clear all


*log using "~/2-kenya-dm-enrol.log", text replace

*--------------------------------------------
* 2-kenya-dm-enrol.do
*
* andrew mertens (andrewmertens@berkeley.edu)
*
* process the Kenya trial enrollment dataset
* 
* this includes enrollment adjustement covariates
* along with uptake variable measures
*
* it creates 2 files: a household-level
* enrollment dataset with analysis covariates
* and a child-level enrollment dataset with
* diarrhea outcomes
*
*--------------------------------------------




*--------------------------------------------
* input files:
*
*  1. washb-kenya-tr.dta
*  2. msP_bl_append_ID_clean_20161010.dta
*  3.
*
*
* output files:
*  final/washb-kenya-enrol.dta
*--------------------------------------------

*setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched")

*--------------------------------------------
* format the treatment assignment information
*--------------------------------------------
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/tr/washb-kenya-tr.dta", clear

destring clusterid, replace
sort clusterid
tempfile trdata
save `trdata'

*--------------------------------------------
* Load in household survey
*--------------------------------------------

use "~/Dropbox/WBK-primary-analysis/Data/Untouched/Baseline/msP_bl_append_ID_clean_20161010.dta", clear
sort  hhid
codebook ms_bl_date
tempfile bhouse
save `bhouse'

*--------------------------------------------
* Load in maternal anthropometry survey
*--------------------------------------------
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/Midline/msP_ml_append_ID_clean_anthropometry_20160909.dta", clear
sort  hhid

* calculate median maternal height
for any c422 c423 c424: replace X = . if X >999
for any c422 c423 c424: replace X = X-resp_hair_height if resp_hair==2
egen float momheight1 = rowmedian(c422 c423 c424)
	replace momheight1 = . if momheight >999
	label var momheight1 "Maternal height (median), year 1 visit"


keep hhid momheight1 c422 c423 c424
tempfile ml_mat_anthro
save `ml_mat_anthro'



use "~/Dropbox/WBK-primary-analysis/Data/Untouched/Endline/msP_el_append_ID_clean_anthropometry_20160909.dta", clear
sort  hhid

* calculate median maternal height
for any c422 c423 c424: replace X = . if X >999
for any c422 c423 c424: replace X = X-resp_hair_height if resp_hair==2
egen float momheight2 = rowmedian(c422 c423 c424)
	replace momheight2 = . if momheight >999
	label var momheight2 "Maternal height (median), year 2 visit"

ren c422 c422_el 
ren c423 c423_el
ren c424 c424_el
keep hhid momheight2 c422_el c423_el c424_el
tempfile el_mat_anthro
save `el_mat_anthro'



*--------------------------------------------
* merge the household and child data
*--------------------------------------------

use "~/Dropbox/WBK-primary-analysis/Data/Untouched/msP_child_IDchar_20161006.dta", clear
sort childid

* merge census information to the data
sort childid hhid



*drop ms_bl_date; missing many dates, use var from anthro datasets
drop ms_bl_date
*Fix baseline date survey from float to date
format blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date ms_el_up_date %td

*--------------------------------------------
* merge in household survey
*--------------------------------------------
merge m:1 hhid using `bhouse'
tab _merge
codebook ms_bl_date


*--------------------------------------------
* Age, using survey date and birth date
*--------------------------------------------
label var DOB "Date of birth"
format DOB %d


generate diar_agem_bl=.
generate diar_agem_ml=.
generate diar_agem_el=.

replace diar_agem_bl=(ms_bl_date-DOB)/30.4167 if hasdiarr_bl==1
		label var diar_agem_bl "Age at BL diar"
replace diar_agem_ml=(ms_ml_ad_date-DOB)/30.4167 if hasdiarr_mld==1
		label var diar_agem_ml "Age at ML diar"
replace diar_agem_el=(ms_el_ad_date-DOB)/30.4167 if hasdiarr_ad_eld==1
		label var diar_agem_el "Age at EL diar"


gen under36=0
	replace under36=1 if  diar_agem_bl<36 & childtype!=1




tempfile bchild
save `bchild'






*br if _merge!=3
assert _merge!=1
* drop households in same compounds that were not actually surveyed 
keep if _merge==3
drop _merge


* merge in the treatment assignment info (keep only matching obs)
sort clusterid
merge m:1 clusterid using `trdata'
*assert _merge == 3
keep if _merge==3
drop _merge

order childid clusterid block tr
sort childid


* merge in maternal height and generate single maternal height variable
merge m:1 hhid using `ml_mat_anthro'

tab _merge
drop if _merge==2
drop _merge
merge m:1 hhid using `el_mat_anthro'
drop if _merge==2
drop _merge




* calculate the overall median
egen float momheight = rowmedian(c422 c423 c424 c422_el c423_el c424_el)
	label var momheight "Maternal height (median)"

* replace the height measurement with round 1 median if the two medians differ by >3 cm
gen diff = momheight1-momheight2
replace momheight = momheight1 if abs(diff)>3 & (diff!=.)


* format survey dates
gen svydate = ms_bl_date
	format svydate %d
	label var svydate "Survey date"
	codebook svydate


*--------------------------------------------
* Adjustment covariates
*--------------------------------------------
/*
The final adjustment covariate list is below:
○	Month of measurement, to account for seasonal variation 
○	Household food insecurity (4-level HFIAS categories)11
○	Child age (days)
○	Child sex
○	Mother’s age (years)
○	Mother’s height (cm)
○	Mother’s education level (no education, primary, secondary)
○	Number of children < 18 years in the household
○	Number of individuals living in the compound
○	Distance (in minutes) to the household’s primary water source 
○	Housing materials (floor, roof) and household assets
	-Assets measured. Has: electricity, radio, television, mobile phone, clock, bicycle, motorcycle, stove, gas cooker, car. Number of: cows, goats, dog s, chickens.
*/

*recode sex
replace sex=sex-1

* Month of measurement=month
*gen month = month(anthrodate)

* Household food insecurity

/* setting no to zero */
 replace c1907_fr = 0 if c1907 == 2
 replace c1908_fr = 0 if c1908 == 2
 replace c1909_fr = 0 if c1909 == 2

 /* assigning categories based on HHS definition */
 generate hhs_7b=.
 generate hhs_8b=.
 generate hhs_9b=.
 generate HHS_total=.

 replace hhs_7b=0 if c1907_fr==0
 replace hhs_7b=1 if c1907_fr==1
 replace hhs_7b=1 if c1907_fr==2
 replace hhs_7b=2 if c1907_fr==3
 replace hhs_8b=0 if c1908_fr==0
 replace hhs_8b=1 if c1908_fr==1
 replace hhs_8b=1 if c1908_fr==2
 replace hhs_8b=2 if c1908_fr==3
 replace hhs_9b=0 if c1909_fr==0
 replace hhs_9b=1 if c1909_fr==1
 replace hhs_9b=1 if c1909_fr==2
 replace hhs_9b=2 if c1909_fr==3
 replace HHS_total = (hhs_7b+hhs_8b+hhs_9b)


/* further assigning categories based on HHS definition */
generate HHS=.
    replace HHS = 1 if HHS_total == 0 
    replace HHS = 1 if HHS_total == 1 
    replace HHS = 2 if HHS_total == 2 
    replace HHS = 2 if HHS_total == 3 
    replace HHS = 3 if HHS_total == 4 
    replace HHS = 3 if HHS_total == 5 
    replace HHS = 3 if HHS_total == 6 
	replace HHS = 9 if HHS_total == . 


 generate HHS_bi=.
     replace HHS_bi = 0 if HHS_total == 0 
     replace HHS_bi = 0 if HHS_total == 1 
     replace HHS_bi = 1 if HHS_total == 2 
     replace HHS_bi = 1 if HHS_total == 3 
     replace HHS_bi = 1 if HHS_total == 4 
     replace HHS_bi = 1 if HHS_total == 5 
     replace HHS_bi = 1 if HHS_total == 6 
	 
label var  HHS_total "sum of hunger score responses"
label var  HHS "Hunger score"
		label define HHS 1 "little to none" 2 "moderate" 3 "Severe" 9 "missing"

label var  HHS_bi "Hunger dichotimized (little none (HHS=1) vs. moderate(HHS=2) and severe (HHS=3)"


*Child age (days)
	*made in anthro and diar datasets

*Child sex

*Mother’s age (years)
	*Imputation procedure for mom DOB: If just the day is missing, we assign their birth date as the 1st of the month? 
	*b.If just the month is missing, we assign the birth month as the same month as the baseline survey? 
	*c.If both day and month are missing, we assign their birth day and month as the same day and month as the baseline survey?
	*keep b6_dob ms_bl_date ms_ml_ad_date ms_el_ad_date b6_year b6_month b6_year b6_day


	format b6_dob ms_bl_date ms_ml_ad_date ms_el_ad_date %d
	*replace missing value "99" with a .
	replace b6_day=. if b6_day==99
	replace b6_month=. if b6_month==99
	replace b6_dob=. if b6_year==9999

	replace b6_year=. if b6_year==9999

	generate momdob=b6_dob
		replace momdob=mdy(b6_month,1,b6_year) if momdob==. & b6_month!=. //a.
		generate bl_month=month(ms_bl_date)
		generate bl_day=day(ms_bl_date) 
		replace momdob=mdy(bl_month,b6_day,b6_year) if momdob==. & b6_month==. //b.
		replace momdob=mdy(bl_month,bl_day,b6_year) if momdob==. & b6_month==.	& b6_day==. //c.
		format momdob %d
		*drop bl_month bl_day
	generate momage=(ms_bl_date-momdob)/365.25
	*replace momage=. if momage<0


*Mother’s height (cm)
	*variable: momheight
	
*Mother’s education level (no education, primary, secondary)
gen momedu = .
	replace momedu = 0 if (b13_school==1|b13_school==2)
	replace momedu = 1 if (b13_school==3)
	replace momedu = 2 if (b13_school>3 & b13_school<.)
	replace momedu=9 if momedu ==.
	label define momedu 0 "IncompletePrimary" 1 "Primary" 2 "AnySecondary" 9 "missing"
	label values momedu momedu
	label var momedu "Mother's education (category)"
codebook momedu


*Number of children < 18 years in the household
* Replace missingness
replace a4_4t18=. if a4_4t18==99
replace a5_0t3=. if a5_0t3==99

generate Nlt18=a4_4t18 + a5_0t3
	label var Nlt18 "Total children<18 in compound"

*Number of individuals living in the compound
rename a6_gross Ncomp
	label var Ncomp "Total indivs in compound"

	
*Distance (in minutes) to the household’s primary water source
	generate dminwat= d7_min
		replace dminwat=d7_hrs*60 if d71==2
	label var dminwat "Distance in minutes to primary water source"

*Housing materials (floor, roof, wall) and household assets
	*Need to determine how to handle "others"
    generate floor =0
		replace floor=1 if b1_floor==3 | b1_floor==77
		replace floor=9 if b1_floor==.
	generate roof =0
		replace roof=1 if b2_roof==2
		replace roof=9 if b2_roof==.
	* fix to match other's conventions
	generate wall=0
		replace wall=1 if b3_wall==3 
		replace wall=1 if b3_other=="BRICKS"|b3_other=="BRICK"| b3_other=="BRICKS AND MUD"|b3_other=="BRICK AND MUD"|b3_other=="MUD AND BRICKS"|b3_other=="BRICKS WITH MUD"|b3_other=="MUD AND BAKED BRICKS"|b3_other=="BRICK WITH SAND"|b3_other=="TILES"
		replace wall=1 if b3_other=="CEMENT ON MUD"|b3_other=="CONCRETE AND MUD"|b3_other=="MUD AND CONCRETE"|b3_other=="MUD BUT CEMENTED"|b3_other=="MUD CEMENTED"|b3_other=="MUD PLASTERED WITH CEMENT"|b3_other=="MUD WITH ROUGH CAST" 
		replace wall=9 if b3_wall==.
	*codebook b3_wall 
	*codebook b3_other
	*table b3_other
	



generate byte electricity= b22a==1
	replace electricity=9 if b22a==. //Add missing=9 code to be consistent with Jade
generate byte radio= b22b==1
	replace radio=9 if b22b==.
generate byte television= b22c==1	
	replace television=9 if b22c==.
generate byte mobile= b22d==1
	replace mobile=9 if b22d==.
generate byte clock= b22e==1
	replace clock=9 if b22e==.
generate byte bicycle= b22f==1
	replace bicycle=9 if b22f==.
generate byte motorcycle= b22g==1
	replace motorcycle=9 if b22g==.
generate byte stove= b22h==1
	replace stove=9 if b22h==.
generate byte gascook= b22i==1
	replace gascook=9 if b22i==.
	label var gascook "gas cooker in home"
generate byte car= b22j==1
	replace car=9 if b22j==.
drop b22*



ren b24a_hh cow
	replace cow=. if cow==999
ren b24c_hh goat
	replace goat=. if goat==999
ren b24g_hh dog
	replace dog=. if dog==999
rename b24e_hh chicken
	replace chicken=. if chicken==999

tab cow
tab goat
tab dog
tab chicken






*--------------------------------------------
* Save an enrollment analysis dataset
* household level observations
*--------------------------------------------

* restrict to household level variables used in the analysis
* and save the data
keep childid clusterid hhid block tr svydate childtype under36 sex DOB momage momheight ms_bl_date blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date ms_el_up_date attritioncase_el haslength_mla haslength_ela haslength hasdiarr_bl hasdiarr_mld hasdiarr_ad_eld hasdiarr_up_eld hasdiarr diar_agem_bl diar_agem_ml diar_agem_el  Ncomp Nlt18 momedu  electricity radio television mobile clock bicycle motorcycle stove gascook car cow  goat  dog chicken dminwat HHS floor roof wall 

*check childid formatting for R merge
format childid %9.0f


*order childid clusterid hhid block tr childtype sex DOB momdob momheight ms_bl_date blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date ms_el_up_date attritioncase_el haslength_mla haslength_ela haslength hasdiarr_bl hasdiarr_mld hasdiarr_ad_eld hasdiarr_up_eld hasdiarr diar_agem_bl diar_agem_ml diar_agem_el  Ncomp momedu  electricity radio television mobile clock bicycle motorcycle stove gascook car cow  goat   dog chicken dminwat HHS floor roof wall

** double check to ensure there are no duplicates
*duplicates drop  childid clusterid hhid block tr childtype sex DOB momdob ms_bl_date blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date ms_el_up_date attritioncase_el haslength_mla haslength_ela haslength hasdiarr_bl hasdiarr_mld hasdiarr_ad_eld hasdiarr_up_eld hasdiarr  Ncomp, force 

compress
sort childid 
label data "Kenya enrollment analysis dataset (HH obs), created by 2-kenya-dm-enrol.do"
saveold "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-enrol.dta", replace version(12)
outsheet using "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-enrol.csv", comma replace

*save to shared folder
saveold "~/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-enrol.dta", replace version(12)
outsheet using "~/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-enrol.csv", comma replace


/*
desc
codebook, c

* write a codebook for the dataset
log close
log using "~/dropbox/WBB-primary-analysis/data/final/ben/washb-bangladesh-enrol-codebook.txt", text replace
desc
codebook, c
codebook
log close


exit
*/
