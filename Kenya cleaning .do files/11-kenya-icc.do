
* -----------------------------------
*
* Calculate ICC of WBK clusters
*
* -----------------------------------

* diarrhea dataset
use "C:\Users\andre\Dropbox\WBK-primary-analysis\Data\Final\Andrew\washb-kenya-diar.dta", clear

keep if dcohort==1 & diar7d!=.

*anova methos
anova diar7d clusterid 

*n=12628
xtmelogit  diar7d || clusterid: 
 estat icc
 
 keep if studyyear==2
 xtmelogit  diar7d || clusterid: 
 estat icc
 
 
* anthropometry dataset
use "C:\Users\andre\Dropbox\WBK-primary-analysis\Data\Final\Andrew\washb-kenya-anthro.dta", clear

keep if studyyear==2 & laz_x!=1 & laz !=.

keep if childtype==1|childtype==2

*n=6801
mixed laz || clusterid: 
 estat icc
 
 