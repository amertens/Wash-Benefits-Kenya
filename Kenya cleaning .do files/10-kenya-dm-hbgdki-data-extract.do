capture log close
set more off
clear all


log using "C:/Users/andre/Dropbox/WBK-primary-analysis/Logs/Andrew/10-kenya-hbgdki-data-extract.log", text replace


*--------------------------------------------
* 10-kenya-hbgdki-data-extract.do
*
* andrew mertens (amertens@berkeley.edu)
*
* extract data from the WASH Benefits Kenya
* trial to share with the HBGDki initiative
*
*--------------------------------------------




*--------------------------------------------
* input files:
*  final/washb-kenya-enrol.dta
*
* output files:
*  HBGDki-extract/washb-kenya-hbgdki-enrol.dta / .csv
*--------------------------------------------

*--------------------------------------------
* loss to follow-up dataset
*--------------------------------------------

use "~/dropbox/WBK-primary-analysis/Data/Untouched/msP_consort_20170310.dta", clear

sort childid hhid clusterid

saveold "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-child-tracking.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-child-tracking.csv", comma replace
desc
codebook, c


* write a codebook for the dataset
log close
log using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-child-tracking-codebook.txt", text replace
desc
notes
codebook, c
codebook
log close

log using "C:/Users/andre/Dropbox/WBK-primary-analysis/Logs/Andrew/10-kenya-hbgdki-data-extract.log", text replace
*--------------------------------------------
* enrollment dataset
*--------------------------------------------

* format the treatment assignment information
use "~/Dropbox/WBK-primary-analysis/Data/Untouched/tr/washb-kenya-tr.dta", clear

destring clusterid, replace
sort clusterid
tempfile trdata
save `trdata'


* Load in maternal anthropometry survey to extract mom heights
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
* Load in household survey
*--------------------------------------------

use "~/Dropbox/WBK-primary-analysis/Data/Untouched/Baseline/msP_bl_append_ID_clean_20161010.dta", clear


* merge in the treatment assignment info (keep only matching obs)
sort clusterid
merge m:1 clusterid using `trdata'
assert _merge == 3
keep if _merge==3
drop _merge

order hhid clusterid block tr
sort hhid


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
The adjustment covariate list for the primary outcomes analysis is below:
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

*Code Open Defication for HBGDki share
rename c801a odchu3
rename c801b odch37


* Month of measurement: in diarrhea or anthro datasets.


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
		label values HHS HHS

label var  HHS_bi "Hunger dichotimized (little none (HHS=1) vs. moderate(HHS=2) and severe (HHS=3)"


*Child age (days)
	*made in anthro and diar datasets

*Child sex: in anthro and diar datasets

*Mother’s age (years)
	*Imputation procedure for mom DOB: If just the day is missing, we assign their birth date as the 1st of the month? 
	*b.If just the month is missing, we assign the birth month as the same month as the baseline survey? 
	*c.If both day and month are missing, we assign their birth day and month as the same day and month as the baseline survey?
	*keep b6_dob ms_bl_date ms_ml_ad_date ms_el_ad_date b6_year b6_month b6_year b6_day


	format b6_dob ms_bl_date   %d
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
		label var floor "Improved floor"
		label define floor 0 "No" 1 "Yes" 9 "missing"
		label values floor floor

	generate roof =0
		replace roof=1 if b2_roof==2
		replace roof=9 if b2_roof==.
		label var roof "Improved roof"
		label define roof 0 "No" 1 "Yes" 9 "missing"
		label values roof roof

	* fix to match other's conventions
	generate wall=0
		replace wall=1 if b3_wall==3 
		replace wall=1 if b3_other=="BRICKS"|b3_other=="BRICK"| b3_other=="BRICKS AND MUD"|b3_other=="BRICK AND MUD"|b3_other=="MUD AND BRICKS"|b3_other=="BRICKS WITH MUD"|b3_other=="MUD AND BAKED BRICKS"|b3_other=="BRICK WITH SAND"|b3_other=="TILES"
		replace wall=1 if b3_other=="CEMENT ON MUD"|b3_other=="CONCRETE AND MUD"|b3_other=="MUD AND CONCRETE"|b3_other=="MUD BUT CEMENTED"|b3_other=="MUD CEMENTED"|b3_other=="MUD PLASTERED WITH CEMENT"|b3_other=="MUD WITH ROUGH CAST" 
		replace wall=9 if b3_wall==.
		label var wall "Improved wall"
		label define wall 0 "No" 1 "Yes" 9 "missing"
		label values wall wall
	



generate byte electricity= b22a==1
	replace electricity=9 if b22a==. //Add missing=9 code to be consistent with Jade
	label var electricity "home has electricity"
			label define electricity 0 "No" 1 "Yes" 9 "missing"
					label values electricity electricity

generate byte radio= b22b==1
	replace radio=9 if b22b==.
		label var radio "radio in home"
				label define radio 0 "No" 1 "Yes" 9 "missing"
					label values radio radio
				
generate byte television= b22c==1	
	replace television=9 if b22c==.
		label var television "television in home"
				label define television 0 "No" 1 "Yes" 9 "missing"
					label values television television 
				
generate byte mobile= b22d==1
	replace mobile=9 if b22d==.
		label var mobile "household owns a mobile phone"
				label define mobile 0 "No" 1 "Yes" 9 "missing"
					label values mobile mobile 

generate byte clock= b22e==1
	replace clock=9 if b22e==.
		label var clock "clock in home"
				label define clock 0 "No" 1 "Yes" 9 "missing"
					label values clock clock 

generate byte bicycle= b22f==1
	replace bicycle=9 if b22f==.
		label var bicycle "household owns bicycle"
				label define bicycle 0 "No" 1 "Yes" 9 "missing"
					label values bicycle bicycle 

generate byte motorcycle= b22g==1
	replace motorcycle=9 if b22g==.
		label var motorcycle "household owns motorcycle"
				label define motorcycle 0 "No" 1 "Yes" 9 "missing"
					label values motorcycle motorcycle 

generate byte stove= b22h==1
	replace stove=9 if b22h==.
		label var stove "stove in home"
				label define stove 0 "No" 1 "Yes" 9 "missing"
					label values stove stove 

				
generate byte gascook= b22i==1
	replace gascook=9 if b22i==.
	label var gascook "gas cooker in home"
			label define gascook 0 "No" 1 "Yes" 9 "missing"
					label values gascook gascook 
					
generate byte car= b22j==1
	replace car=9 if b22j==.
		label var car "household owns automobile"
				label define car 0 "No" 1 "Yes" 9 "missing"
					label values car car 
drop b22*



ren b24a_hh cow
	replace cow=. if cow==999
		label var cow "number of cows in compound"
ren b24c_hh goat
	replace goat=. if goat==999
			label var goat "number of goats in compound"
ren b24g_hh dog
	replace dog=. if dog==999
			label var dog "number of dogs in compound"
rename b24e_hh chicken
	replace chicken=. if chicken==999
			label var chicken "number of chickens in compound"

tab cow
tab goat
tab dog
tab chicken


* restrict to household level variables used in the analysis
* and save the data
keep clusterid hhid block tr svydate momage momheight ms_bl_date Ncomp Nlt18 momedu  electricity radio television mobile clock bicycle motorcycle stove gascook car cow  goat  dog chicken dminwat HHS floor roof wall odchu3 odch37


*set indicators as factors


* drop randomization and treatment assignments
drop block tr




* add some additional notes
notes odch37 : Open defecation among children missing if no children in that age range in household
notes odchu3 : Open defecation among children missing if no children in that age range in household

sort hhid 
label data "Kenya enrollment analysis dataset (HH obs), created by 10-kenya-dm-hbgdki-data-extract.do"


saveold "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-enrol.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-enrol.csv", comma replace
desc
codebook, c

* write a codebook for the dataset
log close
log using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-enrol-codebook.txt", text replace
desc
notes
codebook, c
codebook
log close


log using "C:/Users/andre/Dropbox/WBK-primary-analysis/Logs/Andrew/10-kenya-hbgdki-data-extract.log", text append
*--------------------------------------------
* anthropometry dataset
*--------------------------------------------

use "C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-anthro.dta", clear

*drop variables taken from other datasets for convenient analysis
drop acohort ms_bl_date blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date attritioncase_el haslength_mla haslength_ela haslength

* drop randomiation and treatment assignments
drop block tr

*Rename variables to match Bangladesh conventions
rename studyyear svy
label var svy "Year of anthropometry measurement (either approx. 1 or 2 years after intervention)"

* add some additional notes
note: Anthropometry measurements collected at approximately 1 and 2 years after intervention (svy=1|2)
note: Anthropometry Z-scores calculated using the zscore06 Stata macro (laz, waz, whz, bmiz) and the WHO igrowup Stata macro (hcz)

*rename and reorder raw child measurement variables
rename wch weight
	label var weight "Child weight (median)"
rename weight1 wght1
rename weight2 wght2
rename weight3 wght3

	label var hc1 "Head circumference of child - Measurement #1"
	label var hc2 "Head circumference of child - Measurement #2"
	label var hc3 "Head circumference of child - Measurement #3"
	label var len1 "Length of child (cm) - Measurement #1"
	label var len2 "Length of child (cm) - Measurement #2"
	label var len3 "Length of child (cm) - Measurement #3"

order childid childtype hhid compoundid staffid sex DOB month vlgid clusterid svy anthrodate aged agem agey laz waz whz bmiz hcz laz_x waz_x whz_x hcz_x bmiz_x lazminus2 lazminus3 wazminus2 wazminus3 whzminus3 whzminus2 length_method headcir hc1 hc2 hc3 weight wght1 wght2 wght3 length len1 len2 len3	

saveold "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-anthro.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-anthro.csv", comma replace
desc
codebook, c

* write a codebook for the dataset
log close
log using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-anthro-codebook.txt", text replace
desc
notes
codebook, c
codebook
log close





log using "C:/Users/andre/Dropbox/WBK-primary-analysis/Logs/Andrew/10-kenya-hbgdki-data-extract.log", text append
*--------------------------------------------
* diarrhea dataset
*--------------------------------------------

use "C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-diar.dta", clear

* drop randomiation and treatment assignments
drop block tr

*drop convienience variables used in the primary outcomes analysis
drop dcohort u36 altdiar2d altdiar7d

rename studyyear svy
	label var svy "Year of diarrhea measurement (either approx. 1 or 2 years after intervention)" 

saveold "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-diar.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-diar.csv", comma replace
desc
codebook, c


* write a codebook for the dataset
log close
log using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-diar-codebook.txt", text replace
desc
notes
codebook, c
codebook
log close



log using "C:/Users/andre/Dropbox/WBK-primary-analysis/Logs/Andrew/10-kenya-hbgdki-data-extract.log", text append
*------------------------------------------
* uptake dataset
*--------------------------------------------

use "C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-uptake.dta", clear

* drop randomiation and treatment assignments
drop block tr

rename studyyear svy
	label var svy "Year of uptake measurement (either approx. 1 or 2 years after intervention)" 

label var svydate "Date of uptake measurement" 
label var promoter_vis "visited by promoter in past month" 
label var ch_feces_safe_disp "child feces safely disposed (into latrine, into nappy then lat., or into potty then lat.)" 
label var lnsn "Number of LNS (Lipid-based nutrient supplement) sachets consumed" 
label var lnsp "Percent of recommended LNS (Lipid-based nutrient supplement) sachets consumed" 	
	
	
	
saveold "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-uptake.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-uptake.csv", comma replace
desc
codebook, c


* write a codebook for the dataset
log close
log using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-uptake-codebook.txt", text replace
desc
notes
codebook, c
codebook
log close


log using "C:/Users/andre/Dropbox/WBK-primary-analysis/Logs/Andrew/10-kenya-hbgdki-data-extract.log", text append
*--------------------------------------------
* treatment arm dataset
*--------------------------------------------

use "C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched/tr/washb-kenya-tr.dta", clear


saveold "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-tr.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-tr.csv", comma replace
desc
codebook, c

* write a codebook for the dataset
log close
log using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-tr-codebook.txt", text replace
desc
notes
codebook, c
codebook
log close

*Make a blinded treatment file with empty "tr" variable. 
* drop data from treatment assignment variable
replace tr=.

saveold "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-tr-blind.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/HBGDki/HBGDki-extract-Kenya/washb-kenya-hbgdki-tr-blind.csv", comma replace


exit

