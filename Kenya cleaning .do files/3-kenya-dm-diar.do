*capture log close
*set more off
*clear all


*log using "~/3-kenya-dm-diar.log", text replace

*--------------------------------------------
* 3-kenya-dm-diar.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the kenya trial datasets
* to create a diarrhea analysis dataset
* including data from enrollment, year 1 and year 2
*
* The diarrhea primary analysis includes all measurements
* for children <36 months at enrollment
*
* 
*--------------------------------------------




*--------------------------------------------
* input files:
*
*  Treatment assignments 
*
*  ENROLLMENT
*
*  YEAR 1
*
*
*  YEAR 2
*
* output files:
*  final/washb-bangladesh-diar.dta / .csv
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
* load in baseline child information
*--------------------------------------------
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/msP_child_IDchar_20161006.dta", clear
keep childid childtype sex DOB ms_bl_date blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date hasdiarr_bl hasdiarr_mld hasdiarr_ad_eld hasdiarr_up_eld hasdiarr

sort childid
tempfile childdata
save `childdata'


*--------------------------------------------
* Append diarrheal datasets
*--------------------------------------------
clear
cd "~/Dropbox"
use WBK-primary-analysis/Data/Untouched/Baseline/msP_bl_child_diarr_20161010
generate studyyear=0
rename *_1 *
save "Kenya Primary Analysis/Data-selected/clean/bl_diarr_clean.dta", replace

use WBK-primary-analysis/Data/Untouched/Midline/msP_ml_child_diarr_20161010
generate studyyear=1
generate compoundid=floor(hhid/10)
save "Kenya Primary Analysis/Data-selected/clean/ml_diarr_clean.dta", replace

use WBK-primary-analysis/Data/Untouched/Endline/msP_el_child_diarr_20161010
generate studyyear=2
rename *_ad *
generate compoundid=floor(hhid/10)
save "Kenya Primary Analysis/Data-selected/clean/el_diarr_clean.dta", replace

use "Kenya Primary Analysis/Data-selected/clean/bl_diarr_clean.dta", clear
append using "Kenya Primary Analysis/Data-selected/clean/ml_diarr_clean.dta", force nolabel
append using "Kenya Primary Analysis/Data-selected/clean/el_diarr_clean.dta", force nolabel

keep childid hhid vlgid compoundid clusterid staffid ms_bl_date bf studyyear liq fevera feverb feverc feverd diarra diarrb diarrc diarrd threedefa threedefb threedefc threedefd ndefa ndefb softa softb softc softd blooda bloodb bloodc bloodd rasha rashb rashc rashd cougha coughb coughc coughd conga congb congc congd breatha breathb breathc breathd bruisea bruiseb bruisec bruised tootha toothb toothc toothd ddiarr_day ddiarr_wks mal1 mal1a mal1b mal3 mal3aa mal3a mal3b
*save "Kenya Primary Analysis/Data-selected/clean/appended_diarr_clean.dta"

sort clusterid
tempfile diarsurv
save `diarsurv'


*--------------------------------------------
* merge in the treatment assignment information 
*--------------------------------------------

* drop clusterid and block variables to 
* ensure we get a complete set

* merge in the treatment assignment info (keep only matching obs)
sort clusterid
*capture drop clusterid block tr
merge m:1 clusterid using `trdata'
*assert _merge == 3
keep if _merge==3
drop _merge

*--------------------------------------------
* merge in the child information 
*--------------------------------------------
sort childid
*capture drop clusterid block tr
merge m:1 childid using `childdata'
*assert _merge == 3
keep if _merge==3
drop _merge

*--------------------------------------------
* Age, using survey date and birth date
*--------------------------------------------


format ms_bl_date DOB blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date %d


*Generate anthrodate variable based on survey year
generate anthrodate=0
replace anthrodate=ms_bl_date if studyyear==0
replace anthrodate=ms_ml_ad_date if studyyear==1
replace anthrodate=ms_el_ad_date if studyyear==2
	format anthrodate %d
	label var anthrodate "Date of anthro measurement"

gen aged = anthrodate-DOB
	label var aged "Age in days (anthro meas)"
gen double agem = aged/30.4167
	label var agem "Age in months (anthro meas)"
gen double agey = aged/365.25
	label var agey "Age in years (anthro meas)"
codebook agey

* Month of measurement
gen month = month(anthrodate)
	label var month "Month of measurement"

*Generate indicator variable for non-target children <36 months at baseline
*subset to primary outcome kids

*generate var if non-target under 36mo at baseline
gen u36=0
replace u36=1 if agem<36 & studyyear==0 & childtype>2
	
*Generate a variable to mark diarrhea cohort kids to be analyzed
gen dcohort=0
	replace dcohort=1 if (childtype<=2 & studyyear!=0)


/*

*--------------------------------------------
* Identify new births using the time between
* survey date and enrollment
*--------------------------------------------

gen svydate0 = svydate if svy==0
gen svydate1 = svydate if svy==1

sort dataid
by dataid: egen _date0 = min(svydate0)
by dataid: egen _date1 = min(svydate1)

gen double enrolage = (_date0 - dob)/365.25
label var enrolage "Age at enrollment, years"

gen byte newbirth = enrolage<0
	label var newbirth "New birth"

* create an indicator for non-target new births for easy identification
gen byte sibnewbirth = (tchild==0) & (newbirth==1)
	label var sibnewbirth "Sibling (non-index child) new birth"


*--------------------------------------------
* Identify children who were >36 months 
* at enrollment
*--------------------------------------------
	
* there was one child where enrollment was on his/her birthday, 
* and was exactly 3 years old, so screen >3.01 to retain this child
gen byte gt36mos = enrolage>3.01 & enrolage<.
	label var gt36mos "Older than 36 mos at enrollment"
tab svy gt36mos
sort dataid childid svy
* list dataid childid svy svydate dob enrolage gt36mos if enrolage>3 & enrolage<.
*/

*--------------------------------------------
* Diarrhea, defined as 3+ loose/watery stools in 24 hours or 1+ stool with blood
* calculate separate 2-day recall window and 7-day recall window
* include the (partial) interview day in the 2-day recall window
* to be consistent with the 7-day recall window
*
* assume for 7d prevalence, that if the child had
* prevalent symptoms in the past 2 days that they had symptoms
* within the past 7 days (there are a few inconsistent responses)
*
* likewise, assume that for 2d prevalence if
* the child a measurement for the 7d recall
* but had missing info for the past 2d
* then impute with the 7d recall value
*--------------------------------------------

*replace 99 codes with missing for all imput variables
foreach x of varlist threedef* soft* blood* bruise* tooth* {
   replace `x'= .  if `x' == 99|`x' == 9
}


******
/*
codebook threedefb
codebook threedefc
codebook threedefd

codebook softa
codebook softb
codebook softc
codebook softd

codebook blooda
codebook bloodb
codebook bloodc
codebook bloodd

codebook bruisea
codebook bruiseb
codebook bruisec
codebook bruised

codebook tootha
codebook toothb
codebook toothc
codebook toothd
*/

gen byte d3plus2d = (threedefa==1) | (threedefb==1) | (threedefc==1)
	replace d3plus2d = . if (threedefa==.) & (threedefb==.) & (threedefc==.)
gen byte d3plus7d = (threedefd==1)
	replace d3plus7d = . if (threedefd==.)
	label var d3plus2d "3+ stools in 24 hr, 2d recall"
	label var d3plus7d "3+ stools in 24 hr, 7d recall"

gen byte dloose2d = (softa==1) | (softb==1) | (softc==1)
	replace dloose2d = . if (softa==.) & (softb==.) & (softc==.)
gen byte dloose7d =  (softd==1)
	replace dloose7d = . if (softd==.) 
	label var dloose2d "loose or watery stool, 2d recall"
	label var dloose7d "loose or watery stool, 7d recall"

gen byte dblood2d = (blooda==1) | (bloodb==1) | (bloodc==1)
	replace dblood2d = . if (blooda==.) & (bloodb==.) & (bloodc==.)
gen byte dblood7d =  (bloodd==1)
	replace dblood7d = . if (bloodd==.) 
	label var dblood2d "blood in stool, 2d recall"
	label var dblood7d "blood in stool, 7d recall"

tab d3plus2d, missing
tab dloose2d, missing
tab dblood2d, missing



gen byte diar2d = (d3plus2d==1 & dloose2d==1) | (dblood2d==1)
	label var diar2d "Diarrhea case, 2d recall"
	notes diar2d: Defined as 3+ stools in 24 hrs or 1+ stool w/ blood; includes interview day
	replace diar2d = . if (d3plus2d==.) & (dloose2d==.) & (dblood2d==.) 
	replace diar2d = . if (d3plus2d==. | dloose2d==.) & (dblood2d!=1) 
	replace diar2d = . if (dblood2d==.) & (d3plus2d!=1 | dloose2d!=1) 
	
		tab diar2d, missing

gen byte diar7d = diar2d==1 |((d3plus7d==1 & dloose7d==1) | (dblood7d==1))
	label var diar7d "Diarrhea case, 7d recall"
	notes diar7d: Defined as 3+ stools in 24 hrs or 1+ stool w/ blood; includes interview day
	replace diar7d = . if (d3plus7d==. | dloose7d==.) & (dblood7d!=1) 
	replace diar7d = . if (dblood7d==.) & (d3plus7d!=1 | dloose7d!=1) 
	replace diar7d = . if d3plus7d==. & dloose7d==. & dblood7d==. 
	replace diar7d = 1 if (diar2d==1) 

tab1 diar2d diar7d, m
tab diar2d diar7d, m
tab diar7d
	
*Generate alternative definition of diarrhea only where 3+ and loose stool occur on the same day
gen byte sameday3plusloose_a = (threedefa==1 & softa==1)
		replace sameday3plusloose_a =. if (threedefa==. & softa==.)
gen byte sameday3plusloose_b = (threedefb==1 & softb==1)
		replace sameday3plusloose_a =. if (threedefb==. & softb==.)
gen byte sameday3plusloose_c = (threedefc==1 & softc==1)
		replace sameday3plusloose_a =. if (threedefc==. & softc==.)
gen byte sameday3plusloose_d = (threedefd==1 & softd==1)
		replace sameday3plusloose_a =. if (threedefd==. & softd==.)

gen byte sameday3plusloose2d = (sameday3plusloose_a==1 | sameday3plusloose_b==1 | sameday3plusloose_c==1)
		replace sameday3plusloose2d=. if (sameday3plusloose_a==. & sameday3plusloose_b==. & sameday3plusloose_c==.)

gen byte altdiar2d = (sameday3plusloose2d==1 | dblood2d==1)
	replace altdiar2d = . if (sameday3plusloose2d==. & dblood2d==.)

gen byte altdiar7d = sameday3plusloose_d==1| dblood2d==1
		
tab altdiar2d diar2d
tab altdiar7d diar7d

*bysort tchild: tab svy diar7d, row

*--------------------------------------------
* do some diagnostics on children missing
* diarrhea outcome measurements
*--------------------------------------------

* by sibling status
*tab tchild diar7d, m

* by randomization block
tab block diar7d, m

* by tr status (useful after unblinding)
*tab tr diar7d, m

*--------------------------------------------
* create negative control symptom variables
*--------------------------------------------
gen byte bruise2d = (bruisea==1) | (bruiseb==1) | (bruisec==1)
	replace bruise2d = . if (bruisea==.) & (bruiseb==.) & (bruisec==.)
	label var bruise2d "Bruising, 2d recall"
gen byte bruise7d = (bruised==1)
	replace bruise7d = . if (bruised==.)
	replace bruise7d = 1 if (bruise2d==1) & (bruise7d==0 | bruise7d==.)
	label var bruise7d "Bruising, 7d recall"
gen byte tooth2d = (tootha==1) | (toothb==1) | (toothc==1)
	replace tooth2d = . if (tootha==.) & (toothb==.) & (toothc==.)
	label var tooth2d "Toothache, 2d recall"
gen byte tooth7d = (toothd==1)
	replace tooth7d = . if (toothd==.)
	replace tooth7d = 1 if (tooth2d==1) & (tooth7d==0 | tooth7d==.)
	label var tooth7d "Toothache, 7d recall"
	
*--------------------------------------------
* Save a diarrhea dataset
*--------------------------------------------

* restrict to identifying vars and child level variables
keep hhid childid staffid u36 clusterid vlgid compoundid block tr studyyear dcohort childtype sex DOB aged agem agey month  d3plus* dloose* dblood* diar2d diar7d bruise* tooth*  threedefa threedefb threedefc threedefd softa softb softc softd blooda bloodb bloodc bloodd altdiar2d altdiar7d
order  hhid childid staffid u36 clusterid vlgid compoundid block tr studyyear dcohort childtype sex DOB aged agem agey month   d3plus* dloose* dblood* diar2d diar7d bruise* tooth* threedefa threedefb threedefc threedefd softa softb softc softd blooda bloodb bloodc bloodd altdiar2d altdiar7d 


 
*drop households at enrollment with no target children and no siblings
*assert svy==0 if childid==""
keep if childid!=.
compress
sort hhid childid 

tab diar7d
tab diar7d tr

tab diar2d




label data "Kenya diarrhea dataset, created by 3-kenya-dm-diar.do"
saveold "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-diar.dta", replace version(12)
outsheet using "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-diar.csv", comma replace

*save
saveold "~/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-diar.dta", replace version(12)
outsheet using "~/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-diar.csv", comma replace

/*
* write a codebook for the dataset
log close
log using "~/dropbox/WBB-primary-analysis/data/final/ben/washb-bangladesh-diar-codebook.txt", text replace
desc
codebook, c
codebook
log close


exit

*/
