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
* Load in household survey
*--------------------------------------------

use "~/Dropbox/WBK-primary-analysis/Data/Untouched/Baseline/msP_bl_append_ID_clean_20161207.dta", clear
sort  hhid

*merge in treatment data
sort clusterid
merge m:1 clusterid using `trdata'
drop _merge





*--------------------------------------------
* additional balance table characteristics
*--------------------------------------------
*Unless otherwise noted, exclude values of 88 (NA), 99 (don’t know), and missings 
*(set them all to missing when creating constructs so that they don’t count toward 
*either the numerator or the denominator of percentages and don’t mess up 
*calculations of means for continuous variables)

** maternal
*  age 
	format b6_dob ms_bl_date  %d
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
	

*  parity (not collected at enrollment)
*  Completed at least primary education 
gen byte momedu_prim = (b13_school >2 & b13_school!=. )
	replace momedu_prim=. if  b13_school==.
	label var momedu_prim "Mother has completed at least primary school"
	codebook momedu_prim
	tab momedu_prim
	
** paternal
*  age (not collected at enrollment)
*a.Completed at least primary (b14_spouse>2 & <99)
gen byte patedu = b14_spouse>2 & b14_spouse<99
	replace patedu=. if b14_spouse==88|b14_spouse==99|b14_spouse==.
	label var patedu "Father has completed at least primary school"
	tab patedu
	
*b.Works in agriculture (b18_occ==1, 2 or 3)
gen agwork=0
	replace agwork=1 if b18_occ==1|b18_occ==2|b18_occ==3
	replace agwork=. if b18_occ==88|b18_occ==99|b18_occ==.
	label var agwork "Father works in agriculture"

	
	
*Compound characteristics
*a.Number of households (a01_hh)
rename a01_hh numHHs
	replace numHHs=. if  numHHs==88|numHHs==99
	label var numHHs "Number of households in compound"

*b.Number of persons (a6_total)
rename a6_gross numPPL
	replace numPPL=. if  numPPL==88|numPPL==99
	label var numPPL "Number of people in compound"


** household 
*  number of persons under 18
generate Nlt18=a4_4t18 + a5_0t3
	label var Nlt18 "Total children<18 in compound"
	
*  has electricity (above)
generate byte electricity= b22a==1
	replace electricity=. if  inlist(b22a,.,88,99)

*c.Has cement floor 
gen cementfloor=0
	replace cementfloor=1 if b1_floor==3
	replace cementfloor=. if b1_floor==88|b1_floor==99|b1_floor==.
	label var cementfloor "House has cement floor"

*d. “has iron roof” (b2_roof==2)
gen ironroof=0
	replace ironroof=1 if b2_roof==2
	replace ironroof=. if b2_roof==88|b2_roof==99|b2_roof==.
	label var ironroof "House has iron roof"




*5. Water section
*a.Instead of “shallow tubewell primary water source” use “primary drinking water source is improved” 
*i.if they drink from their primary water source (d8c_drink==1 or d8c7==1), use the type of the
*primary water source (d2) - to determine if it’s improved (d2==1, 4, 6, 7, 11, or 12)

label variable d8c_drink "Drink water from primary water source"
label variable d8c2_drink "Drink water from secondary water source"


gen byte improvedwater1=(d8c_drink==1|d8c7==1) & (d2==1|d2==4|d2==6|d2==7|d2==11|d2==12)
	replace improvedwater=. if ((d8c_drink==.|d8c_drink==2)&(d8c7==.|d8c7==2))

	label var improvedwater1 "primary drinking water source is improved"
*ii.if they don’t drink from primary water source, use type of secondary water source 
*(d3) if they drink from it (d8c2_drink==1 or d8c14==1) following same values for d3 being improved as for d2

gen byte improvedwater2=(d8c2_drink==1 | d8c14==1) & (d3==1|d3==4|d3==6|d3==7|d3==11|d3==12)
replace improvedwater2=. if (d8c2_drink==.|d8c2_drink==2)&(d8c14==.|d8c14==2)

*iii.if they don’t drink from either primary or secondary water source, use type of water
* source of the drinking water stored in the home (c1023, same values count as improved)
gen byte improvedwaterS= (c1023==1|c1023==4|c1023==6|c1023==7|c1023==11|c1023==12)
	replace improvedwaterS=. if  c1023==.
*iv.if no drinking water stored in the home, leave as missing for that household
replace improvedwaterS=. if c1003_1==.

*Create overall improved water variable:
gen byte improvedwater= (improvedwater1==1)
tab improvedwater
	replace improvedwater=1 if improvedwater1==. & improvedwater2==1
	tab improvedwater
		replace improvedwater=1 if improvedwater1==. & improvedwater2==. & improvedwaterS==1
		tab improvedwater
	replace improvedwater=. if  improvedwater1==. & improvedwater2==. & improvedwaterS==.
	

*  stored water observed at home
*b.Instead of “stored water observed at home” use “one-way walking time to primary 
*water source (in minutes)” – note that for this one we do not want to restrict to 
*sources used for drinking since time will influence quantity which is also relevant 
*for hygiene (d71 tells you whether measured in minutes or hours for each HH, 
*then you use d7_min or d7_hours to calculate)
	generate dminwat= d7_min
		replace dminwat=d7_hrs*60 if d71==2
		replace dminwat=. if d71==.
		codebook dminwat
	label var dminwat "Distance in minutes to primary water source"

*  reported treating water yesterday
*c.Instead of “reported treating water yesterday” use “reported treating 
*currently stored water” (variable c1005==1 is whether they treated at all, then
* include all methods of treatment listed in variables c100601 or c100602 or c100603 EXCEPT options 4 and 7) 
gen treatwat=0
	replace treatwat=1 if c1005==1
		replace treatwat=0 if (c100601==4|c100601==7|c100601==.)&(c100602==4|c100602==7|c100602==.)&(c100603==4|c100603==7|c100603==.)
	replace treatwat=. if c1005==99|c1005==.
	label var treatwat "reported treating currently stored water"



	
** Sanitation
*  daily OD
*  adult men
gen byte odmen = (c809d==1|c809d==2)
	replace odmen = . if inlist(c809d,.,88,99)
	replace odmen =0 if c805==2
	label var odmen "Daily OD, adult men (q801a)"

*  adult women
gen byte odwom = (c809e==1|c809e==2)
	replace odwom = . if inlist(c809e,.,88,99)
	replace odwom =0 if c805==2
	label var odwom "Daily OD, adult women (q801b)"

*  children 3-<8 years
gen byte odch38 = (c801b==1)
	replace odch38 = . if inlist(c801b,.,88,99)
	label var odch38 "Daily OD, children 3-8 (q801d)"
*  children <3 years
gen byte odchu3 = (c801a==1)
	replace odchu3 = . if inlist(c801a,.,88,99)
	label var odchu3 "Daily OD, children <3 (q801c)"

** latrine
*create variable of no access to latrine (included in the denominator) and "toilet
*can'te be observed" (excluded from denominator). 
gen byte no_acc_toil = (c805==2)
	label var no_acc_toil "No HH toilet access"
	tab no_acc_toil
	
gen byte no_obs_toil =(to1b==2 | to1b==3)
	label var no_obs_toil "Latrine not observed"
	
*  owned (%)
gen byte latown = (c813!=4 & c813!=5 )
	replace latown =. if c813==99 | c813==88 | c813==.
	replace latown=0 if c805==2
	label var latown "Own their latrine (not shared) (q816)"
	tab latown

*  access to improved latrine
gen byte impr_lat = (c806_7==1 | c806_5==1)
	replace impr_lat=. if (c806_7==99 & c806_5==.) | (c806_7==. & c806_5==.)
	replace impr_lat=. if (to1b==2 | to1b==3)
	replace impr_lat=0 if c805==2
	
	label var impr_lat "Access to improved latrine"


* human feces observed in the household compound
gen byte humfeces = 0
	replace humfeces = 1 if (c827>0 & c827<99) | (c828>0 & c828<99) | (c830>0 & c830<99)
	replace humfeces = . if inlist(c827,99,.) & inlist(c828,99,.) & inlist(c830,99,.)
	label var humfeces "Human feces observed in house/compound (c827,c828,c830)"
	tab humfeces
	

** handwashing


*Water within 2 m of latrine or kitchen: 
*gen byte wat_avail=((c702a_inlat==1 | c702a_incook==1) & c702c_water==1) | ((c703a_inlat==1 | c703a_incook==1) & c703c_water==1)
*	replace wat_avail=. if (c702a_inlat==.&c702a_incook==.& c702c_water==. & c703c_water==.)	
gen wat_avail=0
 replace wat_avail=1 if c702c_water==1 | c703c_water==1
	replace wat_avail=. if c702c_water==. & c703c_water==.
		label var wat_avail "Water within 2 m of latrine or kitchen"

*Soap within 2 m of latrine or kitchen: 
*gen byte soap_avail=((c702a_inlat==1 | c702a_incook==1) & (c702c_scentbar==1 | c702c_unscentbar==1 | c702c_powder==1 | c702c_soapywat==1)) | ((c703a_inlat==1 | c703a_incook==1) & (c703c_scentbar==1 | c703c_unscentbar==1 | c703c_powder==1 | c703c_soapywat==1))
*	replace soap_avail=0 if (c702a_inlat==2 & c702a_incook==2)
*	replace soap_avail=. if (c702a_inlat==. |c702a_incook==.) & (c702c_scentbar==. | c702c_unscentbar==. | c702c_powder==. | c702c_soapywat==.) 
gen byte soap_avail=((c702c_scentbar==1 | c702c_unscentbar==1 | c702c_powder==1 | c702c_soapywat==1)) | ((c703c_scentbar==1 | c703c_unscentbar==1 | c703c_powder==1 | c703c_soapywat==1))
	replace soap_avail=. if  c702c_scentbar==. & c702c_unscentbar==. & c702c_powder==. & c702c_soapywat==.  & c703c_scentbar==. & c703c_unscentbar==. & c703c_powder==. & c703c_soapywat==.

	label var soap_avail "Soap within 2 m of latrine or kitchen"

	tab wat_avail tr, col
	tab soap_avail tr, col

	gen water2m = 0
replace water2m =1 if c702c_water==1 | c703c_water==1
replace water2m = . if c702c_water==. & c703c_water==.

*#delimit; 
gen soap2m = 0
replace soap2m=1 if (c702c_scentbar==1 | c702c_unscentbar==1 | c702c_powder==1 | c702c_soapywat==1 | c703c_scentbar==1 | c703c_unscentbar==1 | c703c_powder==1 | c703c_soapywat==1)
	replace soap2m=. if  c702c_scentbar==. & c702c_unscentbar==. & c702c_powder==. & c702c_soapywat==.  & c703c_scentbar==. & c703c_unscentbar==. & c703c_powder==. & c703c_soapywat==.


replace soap2m=. if (c702c_scentbar==. | c702c_unscentbar==. | c702c_powder==. | c702c_soapywat==.) & (c703c_scentbar==. | c703c_unscentbar==. | c703c_powder==. | c703c_soapywat==.)
*#delimit cr
	tab water2m tr, col
	tab soap2m tr, col
	
	
*prevalence of moderate to severe household hunger
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

	 
label var  HHS "Hunger score"
		label define HHS 1 "little to none" 2 "moderate" 3 "Severe" 9 "missing"

gen byte HHSmod_sev=(HHS>1)
	replace HHSmod_sev=. if HHS==9
	label var HHSmod_sev "Moderate to severe houshold hunger"



**************************************************
*Additional variables beyond primary paper table 1
**************************************************

*Is there any chance you could make a few tweeks to table 1 (enrollment characteristics) for the nearly-identical, but just slightly different version that we wanted to include in our "implementation paper" (a complement to the primary outcomes paper that provides more detail on the interventions and study logistics)?  Specifically, could you:
*1) add a column with the average across all arms? (our goal in this other paper is really just to describe the sample, not so much to show balance)

*2) add "works in agriculture" for respondents 
*(based on variable b17 but otherwise the same as the one for spouses based on b18)
gen RESagwork=0
	replace RESagwork=1 if b17_occ==1|b17_occ==2|b17_occ==3
	replace RESagwork=. if b17_occ==88|b17_occ==99|b17_occ==.
	label var RESagwork "Respondent works in agriculture"
table RESagwork

*3) add "lives with spouse or partner" as another row under "Maternal" 
*(b5-stay==1; careful for skip pattern since both b4_spouse==2 and b5_stay==2 belong in the denominator for this characteristic)
gen live_spouse=0
	replace live_spouse=1 if b4_spouse==1 & b5_stay==1
	replace live_spouse=0 if b4_spouse==2 | b5_stay==2
	replace live_spouse=. if b4_spouse==. & b5_stay==.
table live_spouse 
table b4_spouse b5_stay
table b4_spouse 

*4) in the nutrition section, add "gave previous child liquids before six months" 
*(bf2a<4, exclude bf2a=88 or 99 from denominator) 
gen child_liq=0
	replace child_liq=1 if bf2a_when<4
	replace child_liq=. if bf2a_when==.|bf2a_when==88|bf2a_when==99
table child_liq


*5) "gave previous child solid food before six months" (bf3a, same specification as above)
gen child_food=0
	replace child_food=1 if bf3a_when<4
	replace child_food=. if bf3a_when==.|bf3a_when==88|bf3a_when==99
table child_food

	
	
	* restrict to household level variables used in the analysis
* and save the data
keep  clusterid hhid block tr numPPL Nlt18 momedu momage electricity dminwat  patedu agwork cementfloor ironroof improvedwater treatwat odmen odwom odch38 odchu3 latown impr_lat  humfeces wat_avail soap_avail momedu_prim HHSmod_sev numHHs numPPL RESagwork live_spouse child_liq child_food


compress
label data "Kenya implementation analysis dataset (HH obs), created by 9-kenya-dm-table1.do"
saveold "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-implementation.dta", replace version(12)
outsheet using "~/dropbox/Kenya Primary Analysis/Data-selected/clean/washb-kenya-implementation.csv", comma replace

*save to shared folder
saveold "~/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-implementation.dta", replace version(12)
outsheet using "~/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-implementation.csv", comma replace

	
	
	
	
	
	
	
	
	
