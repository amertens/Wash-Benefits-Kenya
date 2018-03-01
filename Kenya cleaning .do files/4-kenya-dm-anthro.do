capture log close
set more off
clear all


*log using "~/WBBpa/src/dm/4-kenya-dm-anthro.log", text replace

*--------------------------------------------
* 4-kenya-dm-anthro.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the Kenya trial year 1 and 
* year 2 anthropometry data for analysis
*
*--------------------------------------------




*--------------------------------------------
* input files:
*
*  Treatment assignments (Year 2 directory)
*    02. WASHB_Endline_Arm_Identification.dta
* 
*
*  ENROLLMENT
*  1. WASHB_Baseline_main_survey.dta
*  3. WASHB_Baseline_childinfo.dta
*
*  YEAR 1
*  1. WASHB_Midline_main_survey_cleaned.dta
*  3. WASHB_Midline_childinfo_cleaned.dta
*  8. WASHB_Midline_anthropometry_cleaned.dta
*
*  YEAR 2
*  04. WASHB_Endline_main_survey_cleaned.dta
*  07. WASHB_Endline_childinformation_cleaned.dta
*  09. WASHB_Endline_anthropometry_cleaned.dta
*
* output files:
*  final/washb-bangladesh-anthro.dta / .csv
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
* Append and merge child and mother anthro datasets
*--------------------------------------------
clear
cd "~/Dropbox"

use WBK-primary-analysis/Data/Untouched/Endline/msP_el_append_ID_clean_anthropometry_20160909
tempfile m_anthr2
save `m_anthr2'

use WBK-primary-analysis/Data/Untouched/Endline/msP_el_child_anthro_20160909
generate studyyear=2
merge m:1 hhid using `m_anthr2'
drop if _merge!=3 
drop _merge
tempfile anthr2
save `anthr2'

use WBK-primary-analysis/Data/Untouched/Midline/msP_ml_append_ID_clean_anthropometry_20160909
tempfile m_anthr1
save `m_anthr1'

use WBK-primary-analysis/Data/Untouched/Midline/msP_ml_child_anthro_20160909
generate studyyear=1
merge m:1 hhid using `m_anthr1'
drop if _merge!=3 
drop _merge
tempfile anthr1
save `anthr1'

*Append with anthro year 2 and merge in treatment information
append using `anthr2'
merge m:1 clusterid using `trdata'
drop _merge

keep childid hhid vlgid clusterid staffid ms_ml_am_date headc1 headc2 headc3 muac1 muac2 muac3 weight_cloth weight_shoe weight1 weight2 weight3 length_method length1 length2 length3 studyyear c422 c423 c424 resp_length_possible resp_length_possible_explain resp_hair resp_hair_height ms_el_am_date block tr

sort studyyear clusterid childid
tempfile anthrosurv
save `anthrosurv'


*--------------------------------------------
* merge in baseline child information
*--------------------------------------------
use WBK-primary-analysis/Data/Untouched/msP_child_IDchar_20161006
keep childid compoundid childtype sex DOB ms_bl_date blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date attritioncase_el haslength_mla haslength_ela haslength

merge 1:m childid using `anthrosurv'
drop if _merge!=3
drop _merge



*--------------------------------------------
* check agreement between child tracking document and anthro surveys
*--------------------------------------------
*Should be no duplicate children
duplicates report childid studyyear

*Check if any children reported to be collected are missing from year 1
list childid if haslength_mla==1 & headc1==. & muac1==. 
*br if chin_mla==1 & headc1==. 
*Check if any children reported to be collected are missing from year 2
list childid if haslength_ela==1 & headc1==. & muac1==. 
*Check if any children collected are reported to be missing from year 1
list childid if haslength_mla==0 & headc1!=.
*Check if any children collected are reported to be missing from year 2
list childid if haslength_ela==0 & headc1!=.


*--------------------------------------------
* for a small number of children without age
* or sex information in one of the measurement
* rounds, fill it in using the other round
*--------------------------------------------


*--------------------------------------------
* Age, using measurement date and birth date
*--------------------------------------------
label define sex 0 "female" 1 "male"

format DOB ms_bl_date blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date %d

*Generate anthrodate variable based on survey year
generate anthrodate=0
replace anthrodate=ms_ml_am_date if studyyear==1
replace anthrodate=ms_el_am_date if studyyear==2
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

*generate var if non-target under 36mo at baseline
gen u36=0
replace u36=1 if agem<36 & studyyear==0 & childtype>2

*Generate a variable to mark anthro cohort kids to be analyzed
gen acohort=0
	replace acohort=1 if u36==1 | (childtype<=2 & studyyear!=0)


* check children who seem too old
list hhid childid DOB aged agem if agem>15 & agem<. & studyyear==1
list hhid childid DOB aged agem if agem>32 & agem<. & studyyear==2


*--------------------------------------------
* birth order of target children
* collected at year 2, so backfill
*--------------------------------------------
/*
gen int birthord = .
	replace birthord = q4020_1b if childid=="T1"
	replace birthord = q4021_1b if childid=="T2"
	label var birthord "Birth order (target child)"
	
sort dataid childid svy
by dataid childid: egen _x = min(birthord)
replace birthord = _x if birthord==.
drop _x

* some missing values in the measure
* 247 at year 1, 5 at year 2
tab birthord if svy==1
tab birthord if svy==2
*/
*--------------------------------------------
* ensure that all the anthro measurements 
* are rounded to the correct level of precision
* no more than 2 decimal places
*--------------------------------------------
for any headc1 headc2 headc3 muac1 muac2 muac3 weight1 weight2 weight3 length1 length2 length3 c422 c423 c424: replace X = round(X,0.01)

*--------------------------------------------
* Calculate median length measurements
*--------------------------------------------

gen len1 = length1
gen len2 = length2
gen len3 = length3
replace len1 = . if len1>999 | len1<=0
replace len2 = . if len2>999 | len2<=0
replace len3 = . if len3>999 | len3<=0


egen double length = rowmedian(len1 len2 len3)
	replace length = . if length>999
	replace length = round(length,0.001)
	label var length "Child length (median), cm"
	notes length: Median of replicate measures
*format %16.11g length

*--------------------------------------------
* Calculate median weight measurements
*--------------------------------------------

*Need to add mother's weight later
for any  weight1 weight2 weight3: replace X = . if (X>99 | X<=0)

*Note: Check with Jade if we need to do this
* create median of mom + child and mom alone
* we have to do it this way because this reflects
* the method used in the field to determine if a 
* 3rd measurement was required
*egen double wmc = rowmedian(c408 c409 c410)
*egen double wmm = rowmedian(c404 c405 c406)

* create median of the child (if measured alone)
egen double wch = rowmedian(weight1 weight2 weight3)
*154 missing values
 
/*
gen double weight = wch
	replace weight = wmc-wmm if (weight==.) & (wmc!=.) & (wmm!=.)
	replace weight = round(weight,0.001)
	label var weight "Child weight (median), kg"
	notes weight: Median of replicate measures

drop  wmc wmm wch
*/

*--------------------------------------------
* Calculate median head circumference measurements
*--------------------------------------------
gen hc1 = headc1  
gen hc2 = headc2
gen hc3 = headc3
replace hc1 = . if hc1>99 | hc1<=0
replace hc2 = . if hc2>99 | hc2<=0
replace hc3 = . if hc3>99 | hc3<=0

egen double headcir = rowmedian(hc1 hc2 hc3)
	replace headcir = . if headcir>99
	replace headcir = round(headcir,0.001)
	label var headcir "Child head circumference (median), cm"
	notes headcir: Median of replicate measures


*--------------------------------------------
* Calculate laz, waz, whz, using the zscore06
* add-on package. do not specify oedema
*--------------------------------------------
*recode sex(0=2)
codebook sex

*Run zcore function
zscore06, a(agem) s(sex) h(length) w(wch) female(2) male(1) measure(length_method) recum(1) stand(2)

* save a temporary dataset to merge with the WHO igrowup output
save "~/Dropbox/Kenya Primary Analysis/Data-selected/temp/washb-working-anthro.dta", replace

*--------------------------------------------
* Calculate laz, waz, whz, and wcz using the 
* WHO -igrowup- macro. 
*
* this is necessary because the zscore06 
* package does not calculate z-scores for 
* head circumference.  we are using both
* approaches because the zscore06 package is
* ostensibly more accurate (see the package's documentation)
* though as demonstrated below they provide identical results
* (just a good internal validity check)
*
* The WHO -igrowup- macro requires 15 parameters.
* Refer to the documentation for details on this
* finicky macro.
*
* We don't have all of the measurements that
* it processes, so need to create empty
* variables that allow it to run
*---------------------------------------------

/// Indicate to the Stata compiler where the igrowup_standard.ado file is stored ***
adopath + "C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup stata/"

*cd ~"\Dropbox\Kenya Primary Analysis\igrowup_stata"
gen str reflib="~/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup stata/"
	lab var reflib "Directory of reference tables"

gen str datalib="~/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup workdata/"
	lab var datalib "Directory for datafiles"

gen str datalab="TEMPanthro"
	lab var datalab "Working file"

gen str ageunit="days"
gen str measure="L" if length_method==1
replace measure="H" if length_method==2
replace measure=" " if measure == ""
	label var measure "Height measured lying -L- or standing -H-"

* create a temporary sex variable for WHO coding
*gen whosex = sex
*	replace whosex = 2 if sex==0

* create missing variables so that macro will run
for any uac triskin subskin oedema: gen X = .

* set sampling wgtghts to negative to make the prevalence
* calculations blow up -- impossible to run that piece of 
* code w/o Stata SE b/c requires 10,000 variables
gen sw = -10



*---------------------------------------------
* Save a temporary dataset and run -igrowup-
* (note: igrowup adds variables to the data in
* memory and saves a copy dataset with the 
* suffix "_z_st.dta"). Dataset name must correspond
* to the "datalab" variable (defined in last chunk)
*---------------------------------------------

*keep hhid childid studyyear aged whosex whosex aged ageunit wch length measure headcir uac triskin subskin oedema sw  reflib datalib datalab 
keep headcir hhid childid studyyear aged sex ageunit wch length measure headcir uac triskin subskin oedema sw  reflib datalib datalab length_method headcir hc1 hc2 hc3 wch weight1 weight2 weight3 length len1 len2 len3

generate weight=wch

save "~/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup workdata/TEMPanthro.dta", replace
*C:\Users\andre\Dropbox\Kenya Primary Analysis\Kenya cleaning .do files\WHO igrowup workdata

*igrowup_standard reflib datalib datalab sex aged ageunit weight length measure headcir uac triskin subskin oedema sw

#delimit;
igrowup_standard reflib datalib datalab 
sex aged ageunit weight length measure headcir uac triskin subskin oedema sw;
#delimit cr



*---------------------------------------------
* Retrieve WHO calculated output
* "_f" variables identify variables outside of
* reasonable bounds
*
* merge back to the main anthro dataset
*---------------------------------------------
use "C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup workdata/TEMPanthro_z_st", clear

keep headcir hhid childid studyyear _zwei _zlen _zbmi _zwfl _zhc _fwei _flen _fbmi _fwfl _fhc 

sort hhid childid studyyear
save  "C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup workdata/TEMPanthro", replace

use "~/Dropbox/Kenya Primary Analysis/Data-selected/temp/washb-working-anthro", clear
sort hhid childid studyyear
merge 1:1 hhid childid studyyear using "C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup workdata/TEMPanthro"
assert _merge==3
drop _merge


* compare measurements from the 2 packages to ensure they are identical
* (correlation = 1)
corr haz06 _zlen 
corr waz06 _zwei
corr whz06 _zwfl
corr bmiz06 _zbmi


* delete tempfiles
*erase "~/dropbox/washbenefits/bangladesh/trial/data/temp/washb-working-anthro.dta"
*erase "~/dropbox/washbenefits/bangladesh/trial/data/temp/TEMPanthro.dta"
*erase "~/dropbox/washbenefits/bangladesh/trial/data/temp/TEMPanthro_z_st.dta"
*erase "~/dropbox/washbenefits/bangladesh/trial/data/temp/TEMPanthro_z_st.xls"


*--------------------------------------------
* Set extreme values to missing and flag them
* based on the WHO 2006 standards
*--------------------------------------------
rename haz06 laz
rename waz06 waz
rename whz06 whz
rename bmiz06 bmiz
rename _zhc hcz

gen laz_x = (laz < -6 | laz >6)
	replace laz_x = . if laz==.
	label var laz_x "abs(LAZ)>6, set to missing"
	
gen waz_x = (waz < -6 | waz >5)
	replace waz_x = . if waz==.
	label var waz_x "WAZ < -6 or WAZ > 5, set to missing"

gen whz_x = (whz < -5 | whz >5)
	replace whz_x = . if whz==.
	label var whz_x "abs(WHZ)>5, set to missing"
	
gen bmiz_x = (bmiz < -5 | bmiz >5)
	replace bmiz_x = . if bmiz==.
	label var bmiz_x "abs(BMIZ)>5, set to missing"

gen hcz_x = _fhc
	replace hcz_x = . if hcz==.
	label var hcz_x "abs(HCZ)>5, set to missing"

* list extreme values before setting them to missing
/*
list childid aged length weight laz waz whz if laz_x==1
list childid aged length weight laz waz whz if waz_x==1
list childid aged length weight laz waz whz if whz_x==1
list childid aged length weight laz waz whz if bmiz_x==1
list childid aged headcir hcz if hcz_x==1
*/
replace laz = . if laz_x==1
replace waz = . if waz_x==1
replace whz = . if whz_x==1
replace bmiz = . if bmiz_x==1
replace hcz = . if hcz_x==1

* drop extra Z-score calculation variables
drop _z* _f*

*--------------------------------------------
* Identify children who are stunted, 
* underweight, or wasted based on their Z-scores
*--------------------------------------------

gen byte lazminus2 = laz < -2
	replace lazminus2 =. if laz==. | laz_x==1
	label var lazminus2 "Stunted (LAZ<-2)"
gen byte lazminus3 = laz < -3
	replace lazminus3 =. if laz==. | laz_x==1
	label var lazminus3 "Severely Stunted (LAZ<-3)"

gen byte wazminus2 = waz < -2
	replace wazminus2 =. if waz==. | waz_x==1
	label var wazminus2 "Underweight (WAZ<-2)"
gen byte wazminus3 = waz < -3
	replace wazminus3 =. if waz==. | waz_x==1
	label var wazminus3 "Severely Underweight (WAZ<-3)"

gen byte whzminus2 = whz < -2
	replace whzminus2 =. if whz==. | whz_x==1
	label var whzminus2 "Wasted (WHZ<-2)"
gen byte whzminus3 = whz < -3
	replace whzminus3 =. if whz==. | whz_x==1
	label var whzminus3 "Severely Wasted (WHZ<-3)"
	
	
*--------------------------------------------
* Save an analysis dataset
*--------------------------------------------


* restrict to variables used in the analysis
keep childid childtype hhid compoundid staffid acohort sex DOB month ms_bl_date blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date attritioncase_el haslength_mla haslength_ela haslength hhid vlgid clusterid studyyear block tr anthrodate aged agem agey month headcir laz waz whz bmiz hcz laz_x waz_x whz_x hcz_x lazminus2 bmiz_x lazminus3 wazminus2 wazminus3 whzminus3 whzminus2 length_method headcir hc1 hc2 hc3 wch weight1 weight2 weight3 length len1 len2 len3
order childid childtype hhid compoundid staffid acohort sex DOB month ms_bl_date blvisit1date ms_ml_ad_date ms_ml_am_date ms_el_ad_date ms_el_am_date attritioncase_el haslength_mla haslength_ela haslength hhid vlgid clusterid studyyear block tr anthrodate aged agem agey month headcir laz waz whz bmiz hcz laz_x waz_x whz_x hcz_x lazminus2 bmiz_x lazminus3 wazminus2 wazminus3 whzminus3 whzminus2 length_method headcir hc1 hc2 hc3 wch weight1 weight2 weight3 length len1 len2 len3

*check childid formatting for R merge
format childid %10.0f
*format childid %20.10f 

compress
sort hhid childid studyyear
label data "Kenya anthropometry analysis dataset, created by 4-kenya-dm-anthro.do"
saveold "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-anthro.dta", replace version(12)
outsheet using "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-anthro.csv", comma replace

*save
saveold "~/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-anthro.dta", replace version(12)
outsheet using "~/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-anthro.csv", comma replace
x
* write a codebook for the dataset
log close
log using "~/dropbox/WBB-primary-analysis/data/final/ben/washb-bangladesh-anthro-codebook.txt", text replace
desc
codebook, c
codebook
log close

exit

