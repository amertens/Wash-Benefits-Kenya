/*Baseline household hunger scale */
Libname WASH  "C:\Users\Anne\Documents\Nov14 EE data";
Options nofmterr;
Run;
Data wash;
    Set WASH.Baseline;
run;

Proc contents data=wash; run;

Proc freq data=wash;
    tables c1907*c1907_fr c1908*c1908_fr c1909*c1909_fr/ list missing; 
run;
/* 28 observations fully missing */

/* setting no to zero */
Data  wash;
   Set wash;
   If c1907 = 2 then c1907_fr = 0;
   If c1908 = 2 then c1908_fr = 0;
   If c1909 = 2 then c1909_fr = 0; 
/* 
   Not validated hunger scale variables
   If c19111 = '2 3' then c19111 = 4; *both reduced quantity and quality;
   If c1910=2 then c1910_fr=0;
*/
run;

/* assigning categories based on HHS definition */
Data  wash;
   Set wash;
    hhs_7b=.;
	hhs_8b=.;
	hhs_9b=.;
	HHS_total=.;
	if c1907_fr = 0 then hhs_7b = 0;
	if c1907_fr = 1 then hhs_7b = 1;
    if c1907_fr = 2 then hhs_7b = 1;
    if c1907_fr = 3 then hhs_7b = 2;
	if c1908_fr = 0 then hhs_8b = 0;
	if c1908_fr = 1 then hhs_8b = 1;
    if c1908_fr = 2 then hhs_8b = 1;
	if c1908_fr = 3 then hhs_8b = 2;
	if c1909_fr = 0 then hhs_9b = 0;
	if c1909_fr = 1 then hhs_9b = 1;
    if c1909_fr = 2 then hhs_9b = 1;
	if c1909_fr = 3 then hhs_9b = 2;
	HHS_total = sum(hhs_7b, hhs_8b, hhs_9b);
Run;

/* QA */
Proc Freq data=wash;
    tables c1907_fr*hhs_7b c1908_fr*hhs_8b c1909_fr*hhs_9b;
Run;

/* further assigning categories based on HHS definition */
Data  wash;
   Set wash;
    HHS=.;
    If HHS_total = 0 then HHS = 1;
	If HHS_total = 1 then HHS = 1;
	If HHS_total = 2 then HHS = 2;
	If HHS_total = 3 then HHS = 2;
	If HHS_total = 4 then HHS = 3;
	If HHS_total = 5 then HHS = 3;
	If HHS_total = 6 then HHS = 3;

    HHS_bi=.;
    If HHS_total = 0 then HHS_bi = 0;
	If HHS_total = 1 then HHS_bi = 0;
	If HHS_total = 2 then HHS_bi = 1;
	If HHS_total = 3 then HHS_bi = 1;
	If HHS_total = 4 then HHS_bi = 1;
	If HHS_total = 5 then HHS_bi = 1;
	If HHS_total = 6 then HHS_bi = 1;

label     HHS_total = "sum of hunger score responses"
          HHS = "Hunger score"
          HHS_bi = "Hunger dichotimized (little none (HHS=1) vs. moderate(HHS=2) and severe (HHS=3)";
run;

/*QA*/
Proc Freq data=wash;
	tables HHS_total*HHS  HHS_bi*HHS;
Run;
