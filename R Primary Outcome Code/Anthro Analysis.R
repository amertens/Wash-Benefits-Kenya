
#---------------------------------------
# Anthro Analysis.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The outcome analysis in Wash Benefits Kenya for all anthropometry measures and indices.
#---------------------------------------

#---------------------------------------
# preamble
#---------------------------------------
###Load in data
rm(list=ls())
library(foreign)
try(detach(package:plyr))
library(dplyr)
library(magrittr)
library(rmarkdown)
library(washb)
library(SuperLearner)

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew")
#enrol<-read.csv("washb-kenya-enrol.csv", stringsAsFactors = TRUE)
#anthro<-read.csv("washb-kenya-anthro.csv", stringsAsFactors = TRUE)


enrol<-read.dta("washb-kenya-enrol.dta")
anthro<-read.dta("washb-kenya-anthro.dta")


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")



# source the base functions
# which includes the design matrix function used below
source("C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/R Primary Outcome Code/washb_functions.R")

####set arms
arms<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N","PassiveControl")



#-------------------
#Load and clean data
#-------------------



#merge in adjustment covariates
enroltomerge<-subset(enrol, select=c('childid','Ncomp','momage','momedu','momheight','dminwat','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','cow','goat','dog','chicken','roof', 'floor','HHS' ))


#-------------------
#Load and clean data and create shell function for analysis
#-------------------

#merge in adjustment covariates
dim(anthro)
anthro_enrol<-merge(anthro, enroltomerge, by=c("childid"),all.x=T,all.y=F)
dim(anthro_enrol)




#enroltomerge<-select(enrol, childid, diar_agem_bl, diar_agem_ml, diar_agem_el)
#laz <- merge(enrol,anthro,by=c("childid"),all.x=F,all.y=T)
laz<-subset(anthro_enrol, childtype=="Study Child"|childtype=="Study Child Twin")


laz$tr <- factor(laz$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
laz$month <- factor(laz$month)
laz$floor <- factor(laz$floor)

# re-order the treatment factor for convenience
laz$tr <- factor(laz$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

laz <- laz[order(laz$block,laz$clusterid,laz$hhid,laz$childid),]

##Adjusted
#Adjustment variables to include
#month included within diar and anthro datasets.
Wvars<-c('sex','aged','month','momage','momedu','momheight','Ncomp','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','roof','floor','cow','goat','dog','chicken','dminwat','HHS','fracode')

#Create fracode staffid experience factore
laz$staffid[laz$staffid==231] = 0
laz$staffid[laz$staffid==431] = 0
laz$staffid[laz$staffid==542] = 0
laz$staffid[laz$staffid==545] = 0
laz$staffid[laz$staffid==551] = 0
laz$staffid[laz$staffid==860] = 0
laz$staffid[laz$staffid==1727] = 0
laz$staffid[laz$staffid==2312] = 0
laz$staffid[laz$staffid==2687] = 0
laz$staffid[laz$staffid==3334] = 0
laz$staffid[laz$staffid==3433] = 0
laz$staffid[laz$staffid==3436] = 0
laz$staffid[laz$staffid==3438] = 0
laz$staffid[laz$staffid==3446] = 0
laz$staffid[laz$staffid==3450] = 0
laz$staffid[laz$staffid==3508] = 0
laz$staffid[laz$staffid==3683] = 0
laz$staffid[laz$staffid==3783] = 0
laz$staffid[laz$staffid==3785] = 0
laz$staffid[laz$staffid==3787] = 0
laz$staffid[laz$staffid==4218] = 0
laz$staffid[laz$staffid==4313] = 0
laz$staffid[laz$staffid==4314] = 0
laz$staffid[laz$staffid==4347] = 0
laz$staffid[laz$staffid==4345] = 0
laz$staffid[laz$staffid==4348] = 0
laz$staffid[laz$staffid==4405] = 0
laz$staffid[laz$staffid==4515] = 0
laz$staffid[laz$staffid==4534] = 0
laz$staffid[laz$staffid==4538] = 0
laz$staffid[laz$staffid==4548] = 0
laz$staffid[laz$staffid==4617] = 0
laz$staffid[laz$staffid==4630] = 0
laz$staffid[laz$staffid==4714] = 0
laz$staffid[laz$staffid==4715] = 0
laz$staffid[laz$staffid==4717] = 0
laz$staffid[laz$staffid==4785] = 0
laz$staffid[laz$staffid==4812] = 0
laz$staffid[laz$staffid==4835] = 0
laz$staffid[laz$staffid==5324] = 0
laz$staffid[laz$staffid==5421] = 0
laz$staffid[laz$staffid==5422] = 0
laz$staffid[laz$staffid==5818] = 0
laz$staffid[laz$staffid==7540] = 0
laz$staffid[laz$staffid==7640] = 0
laz$staffid[laz$staffid==7848] = 0
laz$staffid[laz$staffid==8246] = 0
laz$staffid[laz$staffid==8274] = 0
laz$staffid[laz$staffid==8303] = 0
laz$staffid[laz$staffid==8604] = 0
laz$staffid[laz$staffid==8681] = 0
laz$staffid[laz$staffid==8787] = 0
laz$staffid[laz$staffid==8803] = 0
laz$staffid[laz$staffid==8884] = 0

laz$fracode <- factor(laz$staffid)




#Save DF without dropped missing values to use as basis for secondary outcomes
anthroDF<-laz

#Drop children missing laz
laz<-subset(laz, !is.na(laz))
dim(laz)

# Drop children with extreme LAZ values
laz <- subset(laz,laz_x!=1)
dim(laz)







#set nreps
nreps=100000


#Get median and IQR for child age at year 1 for table XX year 1
  laz1 <- subset(laz,studyyear==1)

fivenum(laz1$agey)
fivenum(laz1$aged)

#Get median and IQR for child age at year 2 for table 2
  laz2 <- subset(laz,studyyear==2)

fivenum(laz2$agey)
fivenum(laz2$aged)

#Get WHZ sd for manuscript
d<-subset(laz, tr=="Control" & studyyear==2 & whz_x!=1)
sd(d$whz)
mean(d$whz)

table(laz$block, laz$tr)

##################
#LAZ outcome
##################
laz_outcome<-anthrocalc("laz",laz, family="gaussian",nreps)

laz_t1_n_A<- laz_outcome$t1_n
laz_t2_n_A<- laz_outcome$t2_n

laz_t1_h1_rd_unadj_A<- laz_outcome$t1_h1_rd_unadj
laz_t1_h1_pval_unadj_A<- laz_outcome$t1_h1_pval_unadj
laz_t1_h3_rd_unadj_A<- laz_outcome$t1_h3_rd_unadj
laz_t1_h3_pval_unadj_A<- laz_outcome$t1_h3_pval_unadj
laz_t2_h1_rd_unadj_A<- laz_outcome$t2_h1_rd_unadj
laz_t2_h1_pval_unadj_A<- laz_outcome$t2_h1_pval_unadj
laz_t2_h3_rd_unadj_A<- laz_outcome$t2_h3_rd_unadj
laz_t2_h3_pval_unadj_A<- laz_outcome$t2_h3_pval_unadj

laz_t1_h1_rd_adj_A<- laz_outcome$t1_h1_rd_adj
laz_t1_h1_pval_adj_A<- laz_outcome$t1_h1_pval_adj
laz_t1_h3_rd_adj_A<- laz_outcome$t1_h3_rd_adj
laz_t1_h3_pval_adj_A<- laz_outcome$t1_h3_pval_adj
laz_t2_h1_rd_adj_A<- laz_outcome$t2_h1_rd_adj
laz_t2_h1_pval_adj_A<- laz_outcome$t2_h1_pval_adj
laz_t2_h3_rd_adj_A<- laz_outcome$t2_h3_rd_adj
laz_t2_h3_pval_adj_A<- laz_outcome$t2_h3_pval_adj


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
save(laz_t1_n_A, laz_t2_n_A, laz_t1_h1_rd_unadj_A, laz_t1_h1_pval_unadj_A, laz_t1_h3_rd_unadj_A, laz_t1_h3_pval_unadj_A, laz_t2_h1_rd_unadj_A, laz_t2_h1_pval_unadj_A, laz_t2_h3_rd_unadj_A,laz_t2_h3_pval_unadj_A,laz_t1_h1_rd_adj_A,laz_t1_h3_rd_adj_A,laz_t2_h1_rd_adj_A,laz_t2_h3_rd_adj_A,laz_t1_h1_pval_adj_A,laz_t1_h3_pval_adj_A,laz_t2_h1_pval_adj_A,laz_t2_h3_pval_adj_A, file="LAZ_Andrew.RData")








  ##################
  #waz outcome
  ##################
  #drop missing outcome
  waz<-subset(anthroDF, !is.na(waz))
  #drop extreme values
  waz <- subset(waz,waz_x!=1)

  waz_outcome<-anthrocalc("waz",waz, family="gaussian",nreps)

  waz_t1_n_A<- waz_outcome$t1_n
  waz_t2_n_A<- waz_outcome$t2_n

  waz_t1_h1_rd_unadj_A<- waz_outcome$t1_h1_rd_unadj
  waz_t1_h1_pval_unadj_A<- waz_outcome$t1_h1_pval_unadj
  waz_t1_h3_rd_unadj_A<- waz_outcome$t1_h3_rd_unadj
  waz_t1_h3_pval_unadj_A<- waz_outcome$t1_h3_pval_unadj
  waz_t2_h1_rd_unadj_A<- waz_outcome$t2_h1_rd_unadj
  waz_t2_h1_pval_unadj_A<- waz_outcome$t2_h1_pval_unadj
  waz_t2_h3_rd_unadj_A<- waz_outcome$t2_h3_rd_unadj
  waz_t2_h3_pval_unadj_A<- waz_outcome$t2_h3_pval_unadj

  waz_t1_h1_rd_adj_A<- waz_outcome$t1_h1_rd_adj
  waz_t1_h1_pval_adj_A<- waz_outcome$t1_h1_pval_adj
  waz_t1_h3_rd_adj_A<- waz_outcome$t1_h3_rd_adj
  waz_t1_h3_pval_adj_A<- waz_outcome$t1_h3_pval_adj
  waz_t2_h1_rd_adj_A<- waz_outcome$t2_h1_rd_adj
  waz_t2_h1_pval_adj_A<- waz_outcome$t2_h1_pval_adj
  waz_t2_h3_rd_adj_A<- waz_outcome$t2_h3_rd_adj
  waz_t2_h3_pval_adj_A<- waz_outcome$t2_h3_pval_adj


  setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
  save(waz_t1_n_A, waz_t2_n_A, waz_t1_h1_rd_unadj_A, waz_t1_h1_pval_unadj_A, waz_t1_h3_rd_unadj_A, waz_t1_h3_pval_unadj_A, waz_t2_h1_rd_unadj_A, waz_t2_h1_pval_unadj_A, waz_t2_h3_rd_unadj_A,waz_t2_h3_pval_unadj_A,waz_t1_h1_rd_adj_A,waz_t1_h3_rd_adj_A,waz_t2_h1_rd_adj_A,waz_t2_h3_rd_adj_A,waz_t1_h1_pval_adj_A,waz_t1_h3_pval_adj_A,waz_t2_h1_pval_adj_A,waz_t2_h3_pval_adj_A, file="waz_Andrew.RData")




  ##################
  #whz outcome
  ##################

  #drop missing outcome
  whz<-subset(anthroDF, !is.na(whz))
  #drop extreme values
  whz <- subset(whz,whz_x!=1)


  whz_outcome<-anthrocalc("whz",whz, family="gaussian",nreps)

  whz_t1_n_A<- whz_outcome$t1_n
  whz_t2_n_A<- whz_outcome$t2_n

  whz_t1_h1_rd_unadj_A<- whz_outcome$t1_h1_rd_unadj
  whz_t1_h1_pval_unadj_A<- whz_outcome$t1_h1_pval_unadj
  whz_t1_h3_rd_unadj_A<- whz_outcome$t1_h3_rd_unadj
  whz_t1_h3_pval_unadj_A<- whz_outcome$t1_h3_pval_unadj
  whz_t2_h1_rd_unadj_A<- whz_outcome$t2_h1_rd_unadj
  whz_t2_h1_pval_unadj_A<- whz_outcome$t2_h1_pval_unadj
  whz_t2_h3_rd_unadj_A<- whz_outcome$t2_h3_rd_unadj
  whz_t2_h3_pval_unadj_A<- whz_outcome$t2_h3_pval_unadj

  whz_t1_h1_rd_adj_A<- whz_outcome$t1_h1_rd_adj
  whz_t1_h1_pval_adj_A<- whz_outcome$t1_h1_pval_adj
  whz_t1_h3_rd_adj_A<- whz_outcome$t1_h3_rd_adj
  whz_t1_h3_pval_adj_A<- whz_outcome$t1_h3_pval_adj
  whz_t2_h1_rd_adj_A<- whz_outcome$t2_h1_rd_adj
  whz_t2_h1_pval_adj_A<- whz_outcome$t2_h1_pval_adj
  whz_t2_h3_rd_adj_A<- whz_outcome$t2_h3_rd_adj
  whz_t2_h3_pval_adj_A<- whz_outcome$t2_h3_pval_adj


  setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
  save(whz_t1_n_A, whz_t2_n_A, whz_t1_h1_rd_unadj_A, whz_t1_h1_pval_unadj_A, whz_t1_h3_rd_unadj_A, whz_t1_h3_pval_unadj_A, whz_t2_h1_rd_unadj_A, whz_t2_h1_pval_unadj_A, whz_t2_h3_rd_unadj_A,whz_t2_h3_pval_unadj_A,whz_t1_h1_rd_adj_A,whz_t1_h3_rd_adj_A,whz_t2_h1_rd_adj_A,whz_t2_h3_rd_adj_A,whz_t1_h1_pval_adj_A,whz_t1_h3_pval_adj_A,whz_t2_h1_pval_adj_A,whz_t2_h3_pval_adj_A, file="whz_Andrew.RData")






  ##################
  #hcz outcome
  ##################

  #drop missing outcome
  hcz<-subset(anthroDF, !is.na(hcz))
  #drop extreme values
  hcz <- subset(hcz,hcz_x!=1)

  hcz_outcome<-anthrocalc("hcz",hcz, family="gaussian",nreps)

  hcz_t1_n_A<- hcz_outcome$t1_n
  hcz_t2_n_A<- hcz_outcome$t2_n

  hcz_t1_h1_rd_unadj_A<- hcz_outcome$t1_h1_rd_unadj
  hcz_t1_h1_pval_unadj_A<- hcz_outcome$t1_h1_pval_unadj
  hcz_t1_h3_rd_unadj_A<- hcz_outcome$t1_h3_rd_unadj
  hcz_t1_h3_pval_unadj_A<- hcz_outcome$t1_h3_pval_unadj
  hcz_t2_h1_rd_unadj_A<- hcz_outcome$t2_h1_rd_unadj
  hcz_t2_h1_pval_unadj_A<- hcz_outcome$t2_h1_pval_unadj
  hcz_t2_h3_rd_unadj_A<- hcz_outcome$t2_h3_rd_unadj
  hcz_t2_h3_pval_unadj_A<- hcz_outcome$t2_h3_pval_unadj

  hcz_t1_h1_rd_adj_A<- hcz_outcome$t1_h1_rd_adj
  hcz_t1_h1_pval_adj_A<- hcz_outcome$t1_h1_pval_adj
  hcz_t1_h3_rd_adj_A<- hcz_outcome$t1_h3_rd_adj
  hcz_t1_h3_pval_adj_A<- hcz_outcome$t1_h3_pval_adj
  hcz_t2_h1_rd_adj_A<- hcz_outcome$t2_h1_rd_adj
  hcz_t2_h1_pval_adj_A<- hcz_outcome$t2_h1_pval_adj
  hcz_t2_h3_rd_adj_A<- hcz_outcome$t2_h3_rd_adj
  hcz_t2_h3_pval_adj_A<- hcz_outcome$t2_h3_pval_adj


  setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
  save(hcz_t1_n_A, hcz_t2_n_A, hcz_t1_h1_rd_unadj_A, hcz_t1_h1_pval_unadj_A, hcz_t1_h3_rd_unadj_A, hcz_t1_h3_pval_unadj_A, hcz_t2_h1_rd_unadj_A, hcz_t2_h1_pval_unadj_A, hcz_t2_h3_rd_unadj_A,hcz_t2_h3_pval_unadj_A,hcz_t1_h1_rd_adj_A,hcz_t1_h3_rd_adj_A,hcz_t2_h1_rd_adj_A,hcz_t2_h3_rd_adj_A,hcz_t1_h1_pval_adj_A,hcz_t1_h3_pval_adj_A,hcz_t2_h1_pval_adj_A,hcz_t2_h3_pval_adj_A, file="hcz_Andrew.RData")






  ##################
  #stunt outcome
  ##################
  #drop missing outcome
  stunt_outcome<-subset(anthroDF, !is.na(lazminus2))

  stunt_outcome<-anthrocalc("lazminus2", stunt_outcome, family="binomial",nreps)

  stunt_t1_n_A<- stunt_outcome$t1_n
  stunt_t2_n_A<- stunt_outcome$t2_n
  stunt_t1_prev_A<- stunt_outcome$t1_prev
  stunt_t2_prev_A<- stunt_outcome$t2_prev


  stunt_t1_h1_pr_unadj_A<- stunt_outcome$t1_h1_pr_unadj
  stunt_t1_h1_pval_unadj_A<- stunt_outcome$t1_h1_pval_unadj
  stunt_t1_h3_pr_unadj_A<- stunt_outcome$t1_h3_pr_unadj
  stunt_t1_h3_pval_unadj_A<- stunt_outcome$t1_h3_pval_unadj
  stunt_t2_h1_pr_unadj_A<- stunt_outcome$t2_h1_pr_unadj
  stunt_t2_h1_pval_unadj_A<- stunt_outcome$t2_h1_pval_unadj
  stunt_t2_h3_pr_unadj_A<- stunt_outcome$t2_h3_pr_unadj
  stunt_t2_h3_pval_unadj_A<- stunt_outcome$t2_h3_pval_unadj

  stunt_t1_h1_pr_adj_A<- stunt_outcome$t1_h1_pr_adj
  stunt_t1_h1_pval_adj_A<- stunt_outcome$t1_h1_pval_adj
  stunt_t1_h3_pr_adj_A<- stunt_outcome$t1_h3_pr_adj
  stunt_t1_h3_pval_adj_A<- stunt_outcome$t1_h3_pval_adj
  stunt_t2_h1_pr_adj_A<- stunt_outcome$t2_h1_pr_adj
  stunt_t2_h1_pval_adj_A<- stunt_outcome$t2_h1_pval_adj
  stunt_t2_h3_pr_adj_A<- stunt_outcome$t2_h3_pr_adj
  stunt_t2_h3_pval_adj_A<- stunt_outcome$t2_h3_pval_adj

  stunt_t1_h1_rd_unadj_A<- stunt_outcome$t1_h1_rd_unadj
  stunt_t1_h3_rd_unadj_A<- stunt_outcome$t1_h3_rd_unadj
  stunt_t2_h1_rd_unadj_A<- stunt_outcome$t2_h1_rd_unadj
  stunt_t2_h3_rd_unadj_A<- stunt_outcome$t2_h3_rd_unadj
  stunt_t1_h1_rd_adj_A<- stunt_outcome$t1_h1_rd_adj
  stunt_t1_h3_rd_adj_A<- stunt_outcome$t1_h3_rd_adj
  stunt_t2_h1_rd_adj_A<- stunt_outcome$t2_h1_rd_adj
  stunt_t2_h3_rd_adj_A<- stunt_outcome$t2_h3_rd_adj



  setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
  save(stunt_t1_n_A,stunt_t2_n_A,stunt_t1_prev_A, stunt_t2_prev_A, stunt_t1_h1_pr_unadj_A, stunt_t1_h1_pval_unadj_A, stunt_t1_h3_pr_unadj_A, stunt_t1_h3_pval_unadj_A, stunt_t2_h1_pr_unadj_A, stunt_t2_h1_pval_unadj_A, stunt_t2_h3_pr_unadj_A,stunt_t2_h3_pval_unadj_A,stunt_t1_h1_pr_adj_A,stunt_t1_h3_pr_adj_A,stunt_t2_h1_pr_adj_A,stunt_t2_h3_pr_adj_A,stunt_t1_h1_rd_unadj_A, stunt_t1_h3_rd_unadj_A, stunt_t2_h1_rd_unadj_A, stunt_t2_h3_rd_unadj_A, stunt_t1_h1_rd_adj_A, stunt_t1_h3_rd_adj_A, stunt_t2_h1_rd_adj_A, stunt_t2_h3_rd_adj_A,stunt_t1_h1_pval_adj_A,stunt_t1_h3_pval_adj_A,stunt_t2_h1_pval_adj_A,stunt_t2_h3_pval_adj_A, file="stunt_Andrew.RData")





  ##################
  #sstunt outcome
  ##################

  #drop missing outcome
  sstunt_outcome<-subset(anthroDF, !is.na(lazminus3))

  sstunt_outcome<-anthrocalc("lazminus3",sstunt_outcome, family="binomial",nreps)


  sstunt_t1_n_A<- sstunt_outcome$t1_n
  sstunt_t2_n_A<- sstunt_outcome$t2_n
  sstunt_t1_prev_A<- sstunt_outcome$t1_prev
  sstunt_t2_prev_A<- sstunt_outcome$t2_prev

  sstunt_t1_h1_pr_unadj_A<- sstunt_outcome$t1_h1_pr_unadj
  sstunt_t1_h1_pval_unadj_A<- sstunt_outcome$t1_h1_pval_unadj
  sstunt_t1_h3_pr_unadj_A<- sstunt_outcome$t1_h3_pr_unadj
  sstunt_t1_h3_pval_unadj_A<- sstunt_outcome$t1_h3_pval_unadj
  sstunt_t2_h1_pr_unadj_A<- sstunt_outcome$t2_h1_pr_unadj
  sstunt_t2_h1_pval_unadj_A<- sstunt_outcome$t2_h1_pval_unadj
  sstunt_t2_h3_pr_unadj_A<- sstunt_outcome$t2_h3_pr_unadj
  sstunt_t2_h3_pval_unadj_A<- sstunt_outcome$t2_h3_pval_unadj

  sstunt_t1_h1_pr_adj_A<- sstunt_outcome$t1_h1_pr_adj
  sstunt_t1_h1_pval_adj_A<- sstunt_outcome$t1_h1_pval_adj
  sstunt_t1_h3_pr_adj_A<- sstunt_outcome$t1_h3_pr_adj
  sstunt_t1_h3_pval_adj_A<- sstunt_outcome$t1_h3_pval_adj
  sstunt_t2_h1_pr_adj_A<- sstunt_outcome$t2_h1_pr_adj
  sstunt_t2_h1_pval_adj_A<- sstunt_outcome$t2_h1_pval_adj
  sstunt_t2_h3_pr_adj_A<- sstunt_outcome$t2_h3_pr_adj
  sstunt_t2_h3_pval_adj_A<- sstunt_outcome$t2_h3_pval_adj

  sstunt_t1_h1_rd_unadj_A<- sstunt_outcome$t1_h1_rd_unadj
  sstunt_t1_h3_rd_unadj_A<- sstunt_outcome$t1_h3_rd_unadj
  sstunt_t2_h1_rd_unadj_A<- sstunt_outcome$t2_h1_rd_unadj
  sstunt_t2_h3_rd_unadj_A<- sstunt_outcome$t2_h3_rd_unadj
  sstunt_t1_h1_rd_adj_A<- sstunt_outcome$t1_h1_rd_adj
  sstunt_t1_h3_rd_adj_A<- sstunt_outcome$t1_h3_rd_adj
  sstunt_t2_h1_rd_adj_A<- sstunt_outcome$t2_h1_rd_adj
  sstunt_t2_h3_rd_adj_A<- sstunt_outcome$t2_h3_rd_adj



  setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
  save(sstunt_t1_n_A, sstunt_t2_n_A,sstunt_t1_prev_A, sstunt_t2_prev_A, sstunt_t1_h1_pr_unadj_A, sstunt_t1_h1_pval_unadj_A, sstunt_t1_h3_pr_unadj_A, sstunt_t1_h3_pval_unadj_A, sstunt_t2_h1_pr_unadj_A, sstunt_t2_h1_pval_unadj_A, sstunt_t2_h3_pr_unadj_A,sstunt_t2_h3_pval_unadj_A,sstunt_t1_h1_pr_adj_A,sstunt_t1_h3_pr_adj_A,sstunt_t2_h1_pr_adj_A,sstunt_t2_h3_pr_adj_A,sstunt_t1_h1_rd_unadj_A, sstunt_t1_h3_rd_unadj_A, sstunt_t2_h1_rd_unadj_A, sstunt_t2_h3_rd_unadj_A, sstunt_t1_h1_rd_adj_A, sstunt_t1_h3_rd_adj_A, sstunt_t2_h1_rd_adj_A, sstunt_t2_h3_rd_adj_A,sstunt_t1_h1_pval_adj_A,sstunt_t1_h3_pval_adj_A,sstunt_t2_h1_pval_adj_A,sstunt_t2_h3_pval_adj_A, file="sstunt_Andrew.RData")




  ##################
  #wast outcome
  ##################

  #drop missing outcome
  wast_outcome<-subset(anthroDF, !is.na(whzminus2))

  wast_outcome<-anthrocalc("whzminus2",wast_outcome, family="binomial",nreps)


  wast_t1_n_A<- wast_outcome$t1_n
  wast_t2_n_A<- wast_outcome$t2_n
  wast_t1_prev_A<- wast_outcome$t1_prev
  wast_t2_prev_A<- wast_outcome$t2_prev


  wast_t1_h1_pr_unadj_A<- wast_outcome$t1_h1_pr_unadj
  wast_t1_h1_pval_unadj_A<- wast_outcome$t1_h1_pval_unadj
  wast_t1_h3_pr_unadj_A<- wast_outcome$t1_h3_pr_unadj
  wast_t1_h3_pval_unadj_A<- wast_outcome$t1_h3_pval_unadj
  wast_t2_h1_pr_unadj_A<- wast_outcome$t2_h1_pr_unadj
  wast_t2_h1_pval_unadj_A<- wast_outcome$t2_h1_pval_unadj
  wast_t2_h3_pr_unadj_A<- wast_outcome$t2_h3_pr_unadj
  wast_t2_h3_pval_unadj_A<- wast_outcome$t2_h3_pval_unadj

  wast_t1_h1_pr_adj_A<- wast_outcome$t1_h1_pr_adj
  wast_t1_h1_pval_adj_A<- wast_outcome$t1_h1_pval_adj
  wast_t1_h3_pr_adj_A<- wast_outcome$t1_h3_pr_adj
  wast_t1_h3_pval_adj_A<- wast_outcome$t1_h3_pval_adj
  wast_t2_h1_pr_adj_A<- wast_outcome$t2_h1_pr_adj
  wast_t2_h1_pval_adj_A<- wast_outcome$t2_h1_pval_adj
  wast_t2_h3_pr_adj_A<- wast_outcome$t2_h3_pr_adj
  wast_t2_h3_pval_adj_A<- wast_outcome$t2_h3_pval_adj

  wast_t1_h1_rd_unadj_A<- wast_outcome$t1_h1_rd_unadj
  wast_t1_h3_rd_unadj_A<- wast_outcome$t1_h3_rd_unadj
  wast_t2_h1_rd_unadj_A<- wast_outcome$t2_h1_rd_unadj
  wast_t2_h3_rd_unadj_A<- wast_outcome$t2_h3_rd_unadj
  wast_t1_h1_rd_adj_A<- wast_outcome$t1_h1_rd_adj
  wast_t1_h3_rd_adj_A<- wast_outcome$t1_h3_rd_adj
  wast_t2_h1_rd_adj_A<- wast_outcome$t2_h1_rd_adj
  wast_t2_h3_rd_adj_A<- wast_outcome$t2_h3_rd_adj






  setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
  save(wast_t1_n_A, wast_t2_n_A,wast_t1_prev_A, wast_t2_prev_A, wast_t1_h1_pr_unadj_A, wast_t1_h1_pval_unadj_A, wast_t1_h3_pr_unadj_A, wast_t1_h3_pval_unadj_A, wast_t2_h1_pr_unadj_A, wast_t2_h1_pval_unadj_A, wast_t2_h3_pr_unadj_A,wast_t2_h3_pval_unadj_A,wast_t1_h1_pr_adj_A,wast_t1_h3_pr_adj_A,wast_t2_h1_pr_adj_A,wast_t2_h3_pr_adj_A,wast_t1_h1_rd_unadj_A, wast_t1_h3_rd_unadj_A, wast_t2_h1_rd_unadj_A, wast_t2_h3_rd_unadj_A, wast_t1_h1_rd_adj_A, wast_t1_h3_rd_adj_A, wast_t2_h1_rd_adj_A, wast_t2_h3_rd_adj_A,wast_t1_h1_pval_adj_A,wast_t1_h3_pval_adj_A,wast_t2_h1_pval_adj_A,wast_t2_h3_pval_adj_A, file="wast_Andrew.RData")







  ##################
  #underwt outcome
  ##################

  underwt_outcome<-subset(anthroDF, !is.na(wazminus2))

  underwt_outcome<-anthrocalc("wazminus2",underwt_outcome, family="binomial",nreps)


  underwt_t1_n_A<- underwt_outcome$t1_n
  underwt_t2_n_A<- underwt_outcome$t2_n
  underwt_t1_prev_A<- underwt_outcome$t1_prev
  underwt_t2_prev_A<- underwt_outcome$t2_prev

  underwt_t1_h1_pr_unadj_A<- underwt_outcome$t1_h1_pr_unadj
  underwt_t1_h1_pval_unadj_A<- underwt_outcome$t1_h1_pval_unadj
  underwt_t1_h3_pr_unadj_A<- underwt_outcome$t1_h3_pr_unadj
  underwt_t1_h3_pval_unadj_A<- underwt_outcome$t1_h3_pval_unadj
  underwt_t2_h1_pr_unadj_A<- underwt_outcome$t2_h1_pr_unadj
  underwt_t2_h1_pval_unadj_A<- underwt_outcome$t2_h1_pval_unadj
  underwt_t2_h3_pr_unadj_A<- underwt_outcome$t2_h3_pr_unadj
  underwt_t2_h3_pval_unadj_A<- underwt_outcome$t2_h3_pval_unadj

  underwt_t1_h1_pr_adj_A<- underwt_outcome$t1_h1_pr_adj
  underwt_t1_h1_pval_adj_A<- underwt_outcome$t1_h1_pval_adj
  underwt_t1_h3_pr_adj_A<- underwt_outcome$t1_h3_pr_adj
  underwt_t1_h3_pval_adj_A<- underwt_outcome$t1_h3_pval_adj
  underwt_t2_h1_pr_adj_A<- underwt_outcome$t2_h1_pr_adj
  underwt_t2_h1_pval_adj_A<- underwt_outcome$t2_h1_pval_adj
  underwt_t2_h3_pr_adj_A<- underwt_outcome$t2_h3_pr_adj
  underwt_t2_h3_pval_adj_A<- underwt_outcome$t2_h3_pval_adj


  underwt_t1_h1_rd_unadj_A<- underwt_outcome$t1_h1_rd_unadj
  underwt_t1_h3_rd_unadj_A<- underwt_outcome$t1_h3_rd_unadj
  underwt_t2_h1_rd_unadj_A<- underwt_outcome$t2_h1_rd_unadj
  underwt_t2_h3_rd_unadj_A<- underwt_outcome$t2_h3_rd_unadj
  underwt_t1_h1_rd_adj_A<- underwt_outcome$t1_h1_rd_adj
  underwt_t1_h3_rd_adj_A<- underwt_outcome$t1_h3_rd_adj
  underwt_t2_h1_rd_adj_A<- underwt_outcome$t2_h1_rd_adj
  underwt_t2_h3_rd_adj_A<- underwt_outcome$t2_h3_rd_adj






  setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
  save(underwt_t1_n_A, underwt_t2_n_A,underwt_t1_prev_A, underwt_t2_prev_A, underwt_t1_h1_pr_unadj_A, underwt_t1_h1_pval_unadj_A, underwt_t1_h3_pr_unadj_A, underwt_t1_h3_pval_unadj_A, underwt_t2_h1_pr_unadj_A, underwt_t2_h1_pval_unadj_A, underwt_t2_h3_pr_unadj_A,underwt_t2_h3_pval_unadj_A,underwt_t1_h1_pr_adj_A,underwt_t1_h3_pr_adj_A,underwt_t2_h1_pr_adj_A,underwt_t2_h3_pr_adj_A,underwt_t1_h1_rd_unadj_A, underwt_t1_h3_rd_unadj_A, underwt_t2_h1_rd_unadj_A, underwt_t2_h3_rd_unadj_A, underwt_t1_h1_rd_adj_A, underwt_t1_h3_rd_adj_A, underwt_t2_h1_rd_adj_A, underwt_t2_h3_rd_adj_A,underwt_t1_h1_pval_adj_A,underwt_t1_h3_pval_adj_A,underwt_t2_h1_pval_adj_A,underwt_t2_h3_pval_adj_A, file="underwt_Andrew.RData")


