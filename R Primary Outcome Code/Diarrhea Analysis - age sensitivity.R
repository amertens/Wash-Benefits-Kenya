
#---------------------------------------
# Diarrhea Sensitivity Analysis - child age
#
# andrew mertens (amertens@berkeley.edu)
#
# The diar outcome analysis in Wash Benefits Kenya.
#---------------------------------------



#---------------------------------------
# 18 - 21 months
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(magrittr)
library(rmarkdown)
library(washb)
library(SuperLearner)

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched/tr")
treatment<-read.csv("washb-kenya-tr.csv")

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew")
enrol<-read.dta("washb-kenya-enrol.dta")
#diar<-read.dta("washb-kenya-diar.dta")
#HHtracking<-read.dta("washb-kenya-tracking.dta")

#enrol<-read.csv("washb-kenya-enrol.csv",stringsAsFactors = T)
diar<-read.csv("washb-kenya-diar.csv",stringsAsFactors = T)
HHtracking<-read.csv("washb-kenya-tracking.csv",stringsAsFactors = T)


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")

#set tr arms
arms<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N","PassiveControl")

# re-order the treatment factor for convenience
diar$tr <- factor(diar$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

#Subset to children 18-21 months
diar <- diar %>% filter(agem > 18 & agem <= 21 & studyyear==2)

#Diarrhea
##Unadjusted
####Enrollment: n and N by arm
#Should this variable be subset to diarrhea cohort dcohort?
#try(detach(package:plyr))
N.kids <-
  diar %>%
  subset(., !is.na(.$diar7d) & dcohort==1|!is.na(.$diar7d) &u36==1)  %>%
  group_by(studyyear,tr) %>%
  summarise(N.kids=n())
N.diar <-
  diar %>%
  subset(., !is.na(.$diar7d) & dcohort==1|!is.na(.$diar7d) &u36==1)  %>%
  group_by(diar7d,studyyear,tr) %>%
  summarise(N.diar=n())

diar_n<-cbind(N.kids[,3], N.diar[9:16,4])

#Fix up this ordering
#diar_n<-diar_n[c(1,8,2:7,9,16,10:15,17,24,18:23),]
diar_t2_n<-diar_n[1:8,]
colnames(diar_t2_n)<-c("N","n")



####Enrollment: Prevalence and 95% CI by arm
grouped.diarprev <-
  diar %>%
  subset(., !is.na(.$diar7d) & dcohort==1|!is.na(.$diar7d) &u36==1)  %>%
  group_by(studyyear,tr) %>%
  do(as.data.frame(washb_mean(Y=.$diar7d, id=.$clusterid, print=F)))


diar_t2_prev<-cbind(grouped.diarprev[1:8,4], grouped.diarprev[1:8,7], grouped.diarprev[1:8,8])%>%set_rownames(arms)

colnames(diar_t2_prev)<-c("Mean","95%CI.L","95%CI.U")



####H1: Unadjusted prevalence ratios; each arm vs. control. PR, CI, MH P-value
#Merge in variables needed from the enrolment dataset.
enroltomerge<-subset(enrol, select=c('childid','Ncomp','momage','momedu','momheight','dminwat','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','cow','goat','dog','chicken','roof', 'floor','HHS' ))

dim(diar)

diar_enrol<-merge(diar, enroltomerge, by=c("childid"),all.x=T,all.y=F)
colnames(diar_enrol)

dim(diar_enrol)


#subset the diarrhea to children <36 mos at enrollment
#or target children
ad <-
  diar_enrol %>%
  subset(., dcohort==1) %>%
  subset(., !is.na(.$diar7d))

dim(ad)
table(ad$diar7d, ad$tr,ad$studyyear)
table(ad$tr,ad$studyyear)
table(ad$block,ad$tr )

h1.contrasts <- list(c("Control","Passive Control"), c("Control","Water"), c("Control","Sanitation"), c("Control","Handwashing"), c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))

#Run mh function to calculate PR and RD for H1
diar_h1_pr_unadj<-matrix(0, nrow=7, ncol=7)
diar_h1_rd_unadj<-matrix(0, nrow=7, ncol=6)

for(i in 1:7){
  diar_h1_pr_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h1.contrasts[[i]],measure="RR")
  diar_h1_rd_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h1.contrasts[[i]],measure="RD")
}
rownames(diar_h1_rd_unadj) <- rownames(diar_h1_pr_unadj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
colnames(diar_h1_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(diar_h1_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")




####H2: Unadjusted prevalence ratios and risk differences; combined WSH vs. single arms.  PR, CI, MH P-value
h2.contrasts <- list(c("Water","WSH"), c("Sanitation","WSH"), c("Handwashing","WSH"))

#Run mh function to calculate RD and RR for H2
diar_h2_pr_unadj<-matrix(0, nrow=3, ncol=7)
diar_h2_rd_unadj<-matrix(0, nrow=3, ncol=6)

for(i in 1:3){
  diar_h2_pr_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h2.contrasts[[i]],measure="RR")
  diar_h2_rd_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h2.contrasts[[i]],measure="RD")
}
rownames(diar_h2_rd_unadj) <-rownames(diar_h2_pr_unadj) <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
colnames(diar_h2_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(diar_h2_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")


####H3 contrasts for alternative diarrhea figure
h3.contrasts <- list(c("Nutrition","Nutrition + WSH"), c("WSH","Nutrition + WSH"))

#Run mh function to calculate RD and RR for H2
diar_h3_pr_unadj<-matrix(0, nrow=2, ncol=7)
diar_h3_rd_unadj<-matrix(0, nrow=2, ncol=6)

for(i in 1:2){
  diar_h3_pr_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h3.contrasts[[i]],measure="RR")
  diar_h3_rd_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h3.contrasts[[i]],measure="RD")
}
rownames(diar_h3_rd_unadj) <-rownames(diar_h3_pr_unadj) <-c("WSH+N v Nutrition","WSH+n v WSH")
colnames(diar_h3_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(diar_h3_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")



####H1: P-value from permutation test for unadjusted prevalence ratios; each arm vs. control
#Order data to replicate SL
ad <- ad[order(ad$block,ad$clusterid,ad$hhid,ad$childid,ad$studyyear),]

# X is not a balanced factor error in permutation test
# #Run permutation function
# diar_h1_pval_unadj<-t(sapply(h1.contrasts, washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=100000, seed=67890))
# diar_h2_pval_unadj<-t(sapply(h2.contrasts, washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=100000, seed=67890))
# 
# diar_h1_pval_unadj<-(as.matrix(unlist(diar_h1_pval_unadj[,1])))
# diar_h2_pval_unadj<-(as.matrix(unlist(diar_h2_pval_unadj[,1])))
# 
# rownames(diar_h1_pval_unadj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
# rownames(diar_h2_pval_unadj) <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")




##Adjusted
####H1: adjusted prevalence ratios; each arm vs. control. PR, CI
#Adjustment variables to include
#month included within diar and anthro datasets.
Wvars<-c('sex','aged','month','momage','momedu','momheight','Ncomp','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','roof','floor','cow','goat','dog','chicken','dminwat','HHS','fracode')

ad %>% subset(., select=c(Wvars)) %>% summarise_all(mean)

#Create the fracode staff experience factor
ad$staffid[ad$staffid==327] = 0
ad$staffid[ad$staffid==460] = 0
ad$staffid[ad$staffid==1213] = 0
ad$staffid[ad$staffid==1400] = 0
ad$staffid[ad$staffid==1405] = 0
ad$staffid[ad$staffid==1723] = 0
ad$staffid[ad$staffid==1727] = 0
ad$staffid[ad$staffid==1728] = 0
ad$staffid[ad$staffid==1830] = 0
ad$staffid[ad$staffid==2105] = 0
ad$staffid[ad$staffid==2112] = 0
ad$staffid[ad$staffid==2174] = 0
ad$staffid[ad$staffid==2217] = 0
ad$staffid[ad$staffid==2242] = 0
ad$staffid[ad$staffid==2311] = 0
ad$staffid[ad$staffid==2321] = 0
ad$staffid[ad$staffid==2328] = 0
ad$staffid[ad$staffid==2674] = 0
ad$staffid[ad$staffid==2847] = 0
ad$staffid[ad$staffid==3102] = 0
ad$staffid[ad$staffid==3322] = 0
ad$staffid[ad$staffid==3323] = 0
ad$staffid[ad$staffid==3352] = 0
ad$staffid[ad$staffid==3357] = 0
ad$staffid[ad$staffid==3408] = 0
ad$staffid[ad$staffid==3410] = 0
ad$staffid[ad$staffid==3418] = 0
ad$staffid[ad$staffid==3420] = 0
ad$staffid[ad$staffid==3421] = 0
ad$staffid[ad$staffid==3424] = 0
ad$staffid[ad$staffid==3425] = 0
ad$staffid[ad$staffid==3435] = 0
ad$staffid[ad$staffid==3436] = 0
ad$staffid[ad$staffid==3437] = 0
ad$staffid[ad$staffid==3783] = 0
ad$staffid[ad$staffid==4187] = 0
ad$staffid[ad$staffid==4328] = 0
ad$staffid[ad$staffid==4345] = 0
ad$staffid[ad$staffid==4347] = 0
ad$staffid[ad$staffid==4348] = 0
ad$staffid[ad$staffid==4382] = 0
ad$staffid[ad$staffid==4433] = 0
ad$staffid[ad$staffid==4438] = 0
ad$staffid[ad$staffid==4471] = 0
ad$staffid[ad$staffid==4515] = 0
ad$staffid[ad$staffid==4518] = 0
ad$staffid[ad$staffid==4522] = 0
ad$staffid[ad$staffid==4531] = 0
ad$staffid[ad$staffid==4548] = 0
ad$staffid[ad$staffid==4645] = 0
ad$staffid[ad$staffid==5422] = 0
ad$staffid[ad$staffid==7383] = 0
ad$staffid[ad$staffid==8152] = 0
ad$staffid[ad$staffid==8274] = 0
ad$staffid[ad$staffid==8604] = 0
ad$staffid[ad$staffid==8787] = 0
ad$staffid[ad$staffid==8883] = 0
ad$staffid[ad$staffid==8884] = 0
ad$staffid[ad$staffid==9999] = 0

ad$fracode <- factor(ad$staffid)



#Order data to replicate SL
ad <- ad[order(ad$block,ad$clusterid,ad$hhid,ad$childid,ad$studyyear),]



#subset W adjustment set
diarW<- subset(ad, select=Wvars)

#check that all the factor variables are set
for(i in 1:length(Wvars)){
  print(colnames(diarW)[i])
  print(class(diarW[,i])  )
}

#set covariates as factors
diarW$month<-as.factor(diarW$month)
diarW$HHS<-as.factor(diarW$HHS)
diarW$electricity<-as.factor(diarW$electricity)
diarW$radio<-as.factor(diarW$radio)
diarW$television<-as.factor(diarW$television)
diarW$mobile<-as.factor(diarW$mobile)
diarW$clock<-as.factor(diarW$clock)
diarW$bicycle<-as.factor(diarW$bicycle)
diarW$motorcycle<-as.factor(diarW$motorcycle)
diarW$stove<-as.factor(diarW$stove)
diarW$roof<-as.factor(diarW$roof)
diarW$floor<-as.factor(diarW$floor)
ad$block<-as.factor(ad$block)




#Run TMLE for the adjusted parameter estimates
 #Create empty matrices to hold results
diar_h1_pr_adj<-matrix(0, nrow=7, ncol=3)
diar_h1_rd_adj<-matrix(0, nrow=7, ncol=3)





for(i in 1:7){
  temp<-washb_tmle(Y=ad$diar7d, tr=ad$tr, W=diarW, id=ad$block,pair=ad$block,family="binomial", contrast= h1.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=T)
  diar_h1_pr_adj[i,1]<-temp$estimates$RR$psi
  diar_h1_pr_adj[i,2:3]<-temp$estimates$RR$CI
  #diar_h1_pr_adj[i,4]<-temp$estimates$RR$pvalue
  diar_h1_rd_adj[i,1]<-temp$estimates$ATE$psi
  diar_h1_rd_adj[i,2:3]<-temp$estimates$ATE$CI
  #diar_h1_rd_adj[i,4]<-temp$estimates$ATE$pvalue
  
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h1.contrasts[[i]], id=ad$clusterid, family=poisson(link="log"), print=F, verbose=F)
  #diar_h1_pr_adj[i,]<-as.matrix(temp$TR)
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h1.contrasts[[i]], id=ad$clusterid, family="gaussian", print=F, verbose=F)
  #diar_h1_rd_adj[i,]<-as.matrix(temp$TR)
}


#colnames(diar_h1_pr_adj)<-c("PR","95CI lb","95CI ub","logPR","logSE","Zval","Pval")
#colnames(diar_h1_rd_adj)<-c("Risk Diff","95CI lb","95CI ub","SE","Zval","Pval")
colnames(diar_h1_pr_adj)<-c("PR","95CI lb","95CI ub")
colnames(diar_h1_rd_adj)<-c("RD","95CI lb","95CI ub")
rownames(diar_h1_rd_adj) <- rownames(diar_h1_pr_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")


####H2: Adjusted prevalence ratios; combined WSH vs. single arms.  PR, CI
diar_h2_pr_adj<-matrix(0, nrow=3, ncol=3)
diar_h2_rd_adj<-matrix(0, nrow=3, ncol=3)
for(i in 1:3){
  temp<-washb_tmle(Y=ad$diar7d, tr=ad$tr, W=diarW, id=ad$block,pair=ad$block,family="binomial", contrast= h2.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=F)
  diar_h2_pr_adj[i,1]<-temp$estimates$RR$psi
  diar_h2_pr_adj[i,2:3]<-temp$estimates$RR$CI
  #diar_h1_pr_adj[i,4]<-temp$estimates$RR$pvalue
  diar_h2_rd_adj[i,1]<-temp$estimates$ATE$psi
  diar_h2_rd_adj[i,2:3]<-temp$estimates$ATE$CI
  
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h2.contrasts[[i]], id=ad$clusterid, family=poisson(link="log"), print=FALSE)
  #diar_h2_pr_adj[i,]<-as.matrix(temp$TR)
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h2.contrasts[[i]], id=ad$clusterid, family="gaussian", print=FALSE)
  #diar_h2_rd_adj[i,]<-as.matrix(temp$TR)
}


#colnames(diar_h2_pr_adj)<-c("PR","95CI lb","95CI ub","logPR","logSE","Zval","Pval")
#colnames(diar_h2_rd_adj)<-c("Risk Diff","95CI lb","95CI ub","SE","Zval","Pval")
colnames(diar_h2_pr_adj)<-c("PR","95CI lb","95CI ub")
colnames(diar_h2_rd_adj)<-c("RD","95CI lb","95CI ub")
rownames(diar_h2_rd_adj) <- rownames(diar_h2_pr_adj)  <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")





# 
# 
# ####H2: Adjusted difference in prevalence; combined WSH vs. single arms.  PR, CI
# SLd <- data.frame(id=ad$clusterid,block=ad$block,tr=ad$tr,Y=ad$diar7d, studyyear=ad$studyyear,diarW)
# SLd$fracode<-factor(SLd$fracode)
# 
# 
# 
# 
# 
# # pre-screen the covariates for those associated with the outcome (LR test P<0.2)
# # see Wprescreen() and design.matrix() in the base functions
# Wscreen <- washb_prescreen(Y=SLd$Y,Ws=SLd[,6:ncol(SLd)],family="binomial")
# Wselect <- subset(SLd,select=Wscreen)
# 
# 
# 
# # restrict to complete cases
# SLd <- data.frame(childid=ad$childid,svy=ad$studyyear,id=ad$clusterid,hhid=ad$hhid,block=ad$block,tr=ad$tr,Y=ad$diar7d,Wselect)
# dim(SLd)
# SLd <- SLd[complete.cases(SLd),]
# dim(SLd)
# 
# #Order to match Jade
# SLd <- SLd[order(SLd$block,SLd$id,SLd$hhid,SLd$childid,SLd$svy),]
# 
# Wselect<-SLd[,c(8:ncol(SLd))]
# Wselect <- design_matrix(Wselect)
# dim(Wselect)
# colnames(Wselect)
# 
# 
# set.seed(67890)
# SLfit1 <- SuperLearner(Y=SLd$Y,X=Wselect,id=SLd$id,
#                        family="binomial",
#                        SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
# )
# SLfit1
# SLd$pY <- as.vector(predict(SLfit1)$pred)
# SLd$r <- SLd$Y-SLd$pY
# 
# 
# 
# 
# colnames(SLfit1)
# 
# 
# #create empty matrices for permutation function results
# diar_h1_pval_adj<-matrix(nrow=7,ncol=1)
# diar_h2_pval_adj<-matrix(nrow=3,ncol=1)
# 
# #Run permutation function for H1
# for(i in 1:7){
#   temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=100000,contrast=h1.contrasts[[i]], seed=67890)
#   diar_h1_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
# }
# 
# #washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=100000,contrast=c("Control","Nutrition + WSH"), seed=4397157)
# 
# 
# 
# #Run permutation function for H2
# for(i in 1:3){
#   temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=100000,contrast=h2.contrasts[[i]], seed=67890)
#   diar_h2_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
# }
# 
# rownames(diar_h1_pval_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
# rownames(diar_h2_pval_adj) <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
# 

#-----------------------------------
#save data
#-----------------------------------

diar_t2_n_18s_A<-diar_t2_n
diar_t2_prev_18s_A<-diar_t2_prev

diar_h1_pr_unadj_18s_A<- diar_h1_pr_unadj
diar_h1_rd_unadj_18s_A<-  diar_h1_rd_unadj
diar_h2_pr_unadj_18s_A<- diar_h2_pr_unadj
diar_h2_rd_unadj_18s_A<- diar_h2_rd_unadj
diar_h3_pr_unadj_18s_A<- diar_h3_pr_unadj
diar_h3_rd_unadj_18s_A<- diar_h3_rd_unadj
diar_h1_pr_adj_18s_A<- diar_h1_pr_adj
diar_h1_rd_adj_18s_A<-  diar_h1_rd_adj
diar_h2_pr_adj_18s_A<- diar_h2_pr_adj
diar_h2_rd_adj_18s_A<- diar_h2_rd_adj
# diar_h1_pval_unadj_18s_A<-  diar_h1_pval_unadj
# diar_h2_pval_unadj_18s_A<- diar_h2_pval_unadj
# diar_h1_pval_adj_18s_A<-  diar_h1_pval_adj
# diar_h2_pval_adj_18s_A<- diar_h2_pval_adj

save(diar_t2_n_18s_A, diar_t2_prev_18s_A, 
     diar_h1_pr_unadj_18s_A, diar_h1_rd_unadj_18s_A, 
     diar_h2_pr_unadj_18s_A, diar_h2_rd_unadj_18s_A, 
     diar_h1_pr_adj_18s_A, diar_h1_rd_adj_18s_A, 
     diar_h2_pr_adj_18s_A, diar_h2_rd_adj_18s_A,
     # diar_h1_pval_unadj_18s_A,diar_h2_pval_unadj_18s_A,
     # diar_h1_pval_adj_18s_A,diar_h2_pval_adj_18s_A, 
     diar_h3_pr_unadj_18s_A, diar_h3_rd_unadj_18s_A,
     file="diar_18s_Andrew.RData")




#---------------------------------------
# 21 - 24 months
#---------------------------------------


###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(magrittr)
library(rmarkdown)
library(washb)
library(SuperLearner)

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched/tr")
treatment<-read.csv("washb-kenya-tr.csv")

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew")
enrol<-read.dta("washb-kenya-enrol.dta")
diar<-read.csv("washb-kenya-diar.csv",stringsAsFactors = T)
HHtracking<-read.csv("washb-kenya-tracking.csv",stringsAsFactors = T)


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")

#set tr arms
arms<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N","PassiveControl")

# re-order the treatment factor for convenience
diar$tr <- factor(diar$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

#Subset to children 21-24 months
diar <- diar %>% filter(agem > 21 & agem <= 24 & studyyear==2)

#Diarrhea
##Unadjusted
####Enrollment: n and N by arm
#Should this variable be subset to diarrhea cohort dcohort?
#try(detach(package:plyr))
N.kids <-
  diar %>%
  subset(., !is.na(.$diar7d) & dcohort==1|!is.na(.$diar7d) &u36==1)  %>%
  group_by(studyyear,tr) %>%
  summarise(N.kids=n())
N.diar <-
  diar %>%
  subset(., !is.na(.$diar7d) & dcohort==1|!is.na(.$diar7d) &u36==1)  %>%
  group_by(diar7d,studyyear,tr) %>%
  summarise(N.diar=n())

diar_n<-cbind(N.kids[,3], N.diar[9:16,4])

#Fix up this ordering
#diar_n<-diar_n[c(1,8,2:7,9,16,10:15,17,24,18:23),]
diar_t2_n<-diar_n[1:8,]
colnames(diar_t2_n)<-c("N","n")



####Enrollment: Prevalence and 95% CI by arm
grouped.diarprev <-
  diar %>%
  subset(., !is.na(.$diar7d) & dcohort==1|!is.na(.$diar7d) &u36==1)  %>%
  group_by(studyyear,tr) %>%
  do(as.data.frame(washb_mean(Y=.$diar7d, id=.$clusterid, print=F)))


diar_t2_prev<-cbind(grouped.diarprev[1:8,4], grouped.diarprev[1:8,7], grouped.diarprev[1:8,8])%>%set_rownames(arms)

colnames(diar_t2_prev)<-c("Mean","95%CI.L","95%CI.U")



####H1: Unadjusted prevalence ratios; each arm vs. control. PR, CI, MH P-value
#Merge in variables needed from the enrolment dataset.
enroltomerge<-subset(enrol, select=c('childid','Ncomp','momage','momedu','momheight','dminwat','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','cow','goat','dog','chicken','roof', 'floor','HHS' ))

dim(diar)

diar_enrol<-merge(diar, enroltomerge, by=c("childid"),all.x=T,all.y=F)
colnames(diar_enrol)

dim(diar_enrol)


#subset the diarrhea to children <36 mos at enrollment
#or target children
ad <-
  diar_enrol %>%
  subset(., dcohort==1) %>%
  subset(., !is.na(.$diar7d))

dim(ad)
table(ad$diar7d, ad$tr,ad$studyyear)
table(ad$tr,ad$studyyear)
table(ad$block,ad$tr )

h1.contrasts <- list(c("Control","Passive Control"), c("Control","Water"), c("Control","Sanitation"), c("Control","Handwashing"), c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))

#Run mh function to calculate PR and RD for H1
diar_h1_pr_unadj<-matrix(0, nrow=7, ncol=7)
diar_h1_rd_unadj<-matrix(0, nrow=7, ncol=6)

for(i in 1:7){
  diar_h1_pr_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h1.contrasts[[i]],measure="RR")
  diar_h1_rd_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h1.contrasts[[i]],measure="RD")
}
rownames(diar_h1_rd_unadj) <- rownames(diar_h1_pr_unadj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
colnames(diar_h1_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(diar_h1_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")




####H2: Unadjusted prevalence ratios and risk differences; combined WSH vs. single arms.  PR, CI, MH P-value
h2.contrasts <- list(c("Water","WSH"), c("Sanitation","WSH"), c("Handwashing","WSH"))

#Run mh function to calculate RD and RR for H2
diar_h2_pr_unadj<-matrix(0, nrow=3, ncol=7)
diar_h2_rd_unadj<-matrix(0, nrow=3, ncol=6)

for(i in 1:3){
  diar_h2_pr_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h2.contrasts[[i]],measure="RR")
  diar_h2_rd_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h2.contrasts[[i]],measure="RD")
}
rownames(diar_h2_rd_unadj) <-rownames(diar_h2_pr_unadj) <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
colnames(diar_h2_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(diar_h2_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")


####H3 contrasts for alternative diarrhea figure
h3.contrasts <- list(c("Nutrition","Nutrition + WSH"), c("WSH","Nutrition + WSH"))

#Run mh function to calculate RD and RR for H2
diar_h3_pr_unadj<-matrix(0, nrow=2, ncol=7)
diar_h3_rd_unadj<-matrix(0, nrow=2, ncol=6)

for(i in 1:2){
  diar_h3_pr_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h3.contrasts[[i]],measure="RR")
  diar_h3_rd_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h3.contrasts[[i]],measure="RD")
}
rownames(diar_h3_rd_unadj) <-rownames(diar_h3_pr_unadj) <-c("WSH+N v Nutrition","WSH+n v WSH")
colnames(diar_h3_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(diar_h3_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")



####H1: P-value from permutation test for unadjusted prevalence ratios; each arm vs. control
#Order data to replicate SL
ad <- ad[order(ad$block,ad$clusterid,ad$hhid,ad$childid,ad$studyyear),]

# X is not a balanced factor error in permutation test
# #Run permutation function
# diar_h1_pval_unadj<-t(sapply(h1.contrasts, washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=100000, seed=67890))
# diar_h2_pval_unadj<-t(sapply(h2.contrasts, washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=100000, seed=67890))
# 
# diar_h1_pval_unadj<-(as.matrix(unlist(diar_h1_pval_unadj[,1])))
# diar_h2_pval_unadj<-(as.matrix(unlist(diar_h2_pval_unadj[,1])))
# 
# rownames(diar_h1_pval_unadj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
# rownames(diar_h2_pval_unadj) <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")




##Adjusted
####H1: adjusted prevalence ratios; each arm vs. control. PR, CI
#Adjustment variables to include
#month included within diar and anthro datasets.
Wvars<-c('sex','aged','month','momage','momedu','momheight','Ncomp','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','roof','floor','cow','goat','dog','chicken','dminwat','HHS','fracode')

ad %>% subset(., select=c(Wvars)) %>% summarise_all(mean)

#Create the fracode staff experience factor
ad$staffid[ad$staffid==327] = 0
ad$staffid[ad$staffid==460] = 0
ad$staffid[ad$staffid==1213] = 0
ad$staffid[ad$staffid==1400] = 0
ad$staffid[ad$staffid==1405] = 0
ad$staffid[ad$staffid==1723] = 0
ad$staffid[ad$staffid==1727] = 0
ad$staffid[ad$staffid==1728] = 0
ad$staffid[ad$staffid==1830] = 0
ad$staffid[ad$staffid==2105] = 0
ad$staffid[ad$staffid==2112] = 0
ad$staffid[ad$staffid==2174] = 0
ad$staffid[ad$staffid==2217] = 0
ad$staffid[ad$staffid==2242] = 0
ad$staffid[ad$staffid==2311] = 0
ad$staffid[ad$staffid==2321] = 0
ad$staffid[ad$staffid==2328] = 0
ad$staffid[ad$staffid==2674] = 0
ad$staffid[ad$staffid==2847] = 0
ad$staffid[ad$staffid==3102] = 0
ad$staffid[ad$staffid==3322] = 0
ad$staffid[ad$staffid==3323] = 0
ad$staffid[ad$staffid==3352] = 0
ad$staffid[ad$staffid==3357] = 0
ad$staffid[ad$staffid==3408] = 0
ad$staffid[ad$staffid==3410] = 0
ad$staffid[ad$staffid==3418] = 0
ad$staffid[ad$staffid==3420] = 0
ad$staffid[ad$staffid==3421] = 0
ad$staffid[ad$staffid==3424] = 0
ad$staffid[ad$staffid==3425] = 0
ad$staffid[ad$staffid==3435] = 0
ad$staffid[ad$staffid==3436] = 0
ad$staffid[ad$staffid==3437] = 0
ad$staffid[ad$staffid==3783] = 0
ad$staffid[ad$staffid==4187] = 0
ad$staffid[ad$staffid==4328] = 0
ad$staffid[ad$staffid==4345] = 0
ad$staffid[ad$staffid==4347] = 0
ad$staffid[ad$staffid==4348] = 0
ad$staffid[ad$staffid==4382] = 0
ad$staffid[ad$staffid==4433] = 0
ad$staffid[ad$staffid==4438] = 0
ad$staffid[ad$staffid==4471] = 0
ad$staffid[ad$staffid==4515] = 0
ad$staffid[ad$staffid==4518] = 0
ad$staffid[ad$staffid==4522] = 0
ad$staffid[ad$staffid==4531] = 0
ad$staffid[ad$staffid==4548] = 0
ad$staffid[ad$staffid==4645] = 0
ad$staffid[ad$staffid==5422] = 0
ad$staffid[ad$staffid==7383] = 0
ad$staffid[ad$staffid==8152] = 0
ad$staffid[ad$staffid==8274] = 0
ad$staffid[ad$staffid==8604] = 0
ad$staffid[ad$staffid==8787] = 0
ad$staffid[ad$staffid==8883] = 0
ad$staffid[ad$staffid==8884] = 0
ad$staffid[ad$staffid==9999] = 0

ad$fracode <- factor(ad$staffid)



#Order data to replicate SL
ad <- ad[order(ad$block,ad$clusterid,ad$hhid,ad$childid,ad$studyyear),]



#subset W adjustment set
diarW<- subset(ad, select=Wvars)

#check that all the factor variables are set
for(i in 1:length(Wvars)){
  print(colnames(diarW)[i])
  print(class(diarW[,i])  )
}

#set covariates as factors
diarW$month<-as.factor(diarW$month)
diarW$HHS<-as.factor(diarW$HHS)
diarW$electricity<-as.factor(diarW$electricity)
diarW$radio<-as.factor(diarW$radio)
diarW$television<-as.factor(diarW$television)
diarW$mobile<-as.factor(diarW$mobile)
diarW$clock<-as.factor(diarW$clock)
diarW$bicycle<-as.factor(diarW$bicycle)
diarW$motorcycle<-as.factor(diarW$motorcycle)
diarW$stove<-as.factor(diarW$stove)
diarW$roof<-as.factor(diarW$roof)
diarW$floor<-as.factor(diarW$floor)
ad$block<-as.factor(ad$block)




#Run TMLE for the adjusted parameter estimates
 #Create empty matrices to hold results
diar_h1_pr_adj<-matrix(0, nrow=7, ncol=3)
diar_h1_rd_adj<-matrix(0, nrow=7, ncol=3)





for(i in 1:7){
  temp<-washb_tmle(Y=ad$diar7d, tr=ad$tr, W=diarW, id=ad$block,pair=ad$block,family="binomial", contrast= h1.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=T)
  diar_h1_pr_adj[i,1]<-temp$estimates$RR$psi
  diar_h1_pr_adj[i,2:3]<-temp$estimates$RR$CI
  #diar_h1_pr_adj[i,4]<-temp$estimates$RR$pvalue
  diar_h1_rd_adj[i,1]<-temp$estimates$ATE$psi
  diar_h1_rd_adj[i,2:3]<-temp$estimates$ATE$CI
  #diar_h1_rd_adj[i,4]<-temp$estimates$ATE$pvalue
  
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h1.contrasts[[i]], id=ad$clusterid, family=poisson(link="log"), print=F, verbose=F)
  #diar_h1_pr_adj[i,]<-as.matrix(temp$TR)
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h1.contrasts[[i]], id=ad$clusterid, family="gaussian", print=F, verbose=F)
  #diar_h1_rd_adj[i,]<-as.matrix(temp$TR)
}


#colnames(diar_h1_pr_adj)<-c("PR","95CI lb","95CI ub","logPR","logSE","Zval","Pval")
#colnames(diar_h1_rd_adj)<-c("Risk Diff","95CI lb","95CI ub","SE","Zval","Pval")
colnames(diar_h1_pr_adj)<-c("PR","95CI lb","95CI ub")
colnames(diar_h1_rd_adj)<-c("RD","95CI lb","95CI ub")
rownames(diar_h1_rd_adj) <- rownames(diar_h1_pr_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")


####H2: Adjusted prevalence ratios; combined WSH vs. single arms.  PR, CI
diar_h2_pr_adj<-matrix(0, nrow=3, ncol=3)
diar_h2_rd_adj<-matrix(0, nrow=3, ncol=3)
for(i in 1:3){
  temp<-washb_tmle(Y=ad$diar7d, tr=ad$tr, W=diarW, id=ad$block,pair=ad$block,family="binomial", contrast= h2.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=F)
  diar_h2_pr_adj[i,1]<-temp$estimates$RR$psi
  diar_h2_pr_adj[i,2:3]<-temp$estimates$RR$CI
  #diar_h1_pr_adj[i,4]<-temp$estimates$RR$pvalue
  diar_h2_rd_adj[i,1]<-temp$estimates$ATE$psi
  diar_h2_rd_adj[i,2:3]<-temp$estimates$ATE$CI
  
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h2.contrasts[[i]], id=ad$clusterid, family=poisson(link="log"), print=FALSE)
  #diar_h2_pr_adj[i,]<-as.matrix(temp$TR)
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h2.contrasts[[i]], id=ad$clusterid, family="gaussian", print=FALSE)
  #diar_h2_rd_adj[i,]<-as.matrix(temp$TR)
}


#colnames(diar_h2_pr_adj)<-c("PR","95CI lb","95CI ub","logPR","logSE","Zval","Pval")
#colnames(diar_h2_rd_adj)<-c("Risk Diff","95CI lb","95CI ub","SE","Zval","Pval")
colnames(diar_h2_pr_adj)<-c("PR","95CI lb","95CI ub")
colnames(diar_h2_rd_adj)<-c("RD","95CI lb","95CI ub")
rownames(diar_h2_rd_adj) <- rownames(diar_h2_pr_adj)  <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")





# 
# 
# ####H2: Adjusted difference in prevalence; combined WSH vs. single arms.  PR, CI
# SLd <- data.frame(id=ad$clusterid,block=ad$block,tr=ad$tr,Y=ad$diar7d, studyyear=ad$studyyear,diarW)
# SLd$fracode<-factor(SLd$fracode)
# 
# 
# 
# 
# 
# # pre-screen the covariates for those associated with the outcome (LR test P<0.2)
# # see Wprescreen() and design.matrix() in the base functions
# Wscreen <- washb_prescreen(Y=SLd$Y,Ws=SLd[,6:ncol(SLd)],family="binomial")
# Wselect <- subset(SLd,select=Wscreen)
# 
# 
# 
# # restrict to complete cases
# SLd <- data.frame(childid=ad$childid,svy=ad$studyyear,id=ad$clusterid,hhid=ad$hhid,block=ad$block,tr=ad$tr,Y=ad$diar7d,Wselect)
# dim(SLd)
# SLd <- SLd[complete.cases(SLd),]
# dim(SLd)
# 
# #Order to match Jade
# SLd <- SLd[order(SLd$block,SLd$id,SLd$hhid,SLd$childid,SLd$svy),]
# 
# Wselect<-SLd[,c(8:ncol(SLd))]
# Wselect <- design_matrix(Wselect)
# dim(Wselect)
# colnames(Wselect)
# 
# 
# set.seed(67890)
# SLfit1 <- SuperLearner(Y=SLd$Y,X=Wselect,id=SLd$id,
#                        family="binomial",
#                        SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
# )
# SLfit1
# SLd$pY <- as.vector(predict(SLfit1)$pred)
# SLd$r <- SLd$Y-SLd$pY
# 
# 
# 
# 
# colnames(SLfit1)
# 
# 
# #create empty matrices for permutation function results
# diar_h1_pval_adj<-matrix(nrow=7,ncol=1)
# diar_h2_pval_adj<-matrix(nrow=3,ncol=1)
# 
# #Run permutation function for H1
# for(i in 1:7){
#   temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=100000,contrast=h1.contrasts[[i]], seed=67890)
#   diar_h1_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
# }
# 
# #washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=100000,contrast=c("Control","Nutrition + WSH"), seed=4397157)
# 
# 
# 
# #Run permutation function for H2
# for(i in 1:3){
#   temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=100000,contrast=h2.contrasts[[i]], seed=67890)
#   diar_h2_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
# }
# 
# rownames(diar_h1_pval_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
# rownames(diar_h2_pval_adj) <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
# 

#-----------------------------------
#save data
#-----------------------------------

diar_t2_n_21s_A<-diar_t2_n
diar_t2_prev_21s_A<-diar_t2_prev

diar_h1_pr_unadj_21s_A<- diar_h1_pr_unadj
diar_h1_rd_unadj_21s_A<-  diar_h1_rd_unadj
diar_h2_pr_unadj_21s_A<- diar_h2_pr_unadj
diar_h2_rd_unadj_21s_A<- diar_h2_rd_unadj
diar_h3_pr_unadj_21s_A<- diar_h3_pr_unadj
diar_h3_rd_unadj_21s_A<- diar_h3_rd_unadj
diar_h1_pr_adj_21s_A<- diar_h1_pr_adj
diar_h1_rd_adj_21s_A<-  diar_h1_rd_adj
diar_h2_pr_adj_21s_A<- diar_h2_pr_adj
diar_h2_rd_adj_21s_A<- diar_h2_rd_adj
# diar_h1_pval_unadj_21s_A<-  diar_h1_pval_unadj
# diar_h2_pval_unadj_21s_A<- diar_h2_pval_unadj
# diar_h1_pval_adj_21s_A<-  diar_h1_pval_adj
# diar_h2_pval_adj_21s_A<- diar_h2_pval_adj

save(diar_t2_n_21s_A, diar_t2_prev_21s_A, 
     diar_h1_pr_unadj_21s_A, diar_h1_rd_unadj_21s_A, 
     diar_h2_pr_unadj_21s_A, diar_h2_rd_unadj_21s_A, 
     diar_h1_pr_adj_21s_A, diar_h1_rd_adj_21s_A, 
     diar_h2_pr_adj_21s_A, diar_h2_rd_adj_21s_A,
     # diar_h1_pval_unadj_21s_A,diar_h2_pval_unadj_21s_A,
     # diar_h1_pval_adj_21s_A,diar_h2_pval_adj_21s_A, 
     diar_h3_pr_unadj_21s_A, diar_h3_rd_unadj_21s_A,
     file="diar_21s_Andrew.RData")











#---------------------------------------
# 24 - 27 months
#---------------------------------------


###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(magrittr)
library(rmarkdown)
library(washb)
library(SuperLearner)

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched/tr")
treatment<-read.csv("washb-kenya-tr.csv")

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew")
enrol<-read.dta("washb-kenya-enrol.dta")

diar<-read.csv("washb-kenya-diar.csv",stringsAsFactors = T)
HHtracking<-read.csv("washb-kenya-tracking.csv",stringsAsFactors = T)


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")

#set tr arms
arms<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N","PassiveControl")

# re-order the treatment factor for convenience
diar$tr <- factor(diar$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

#Subset to children 24-27 months
diar <- diar %>% filter(agem > 24 & agem <= 27 & studyyear==2)

#Diarrhea
##Unadjusted
####Enrollment: n and N by arm
#Should this variable be subset to diarrhea cohort dcohort?
#try(detach(package:plyr))
N.kids <-
  diar %>%
  subset(., !is.na(.$diar7d) & dcohort==1|!is.na(.$diar7d) &u36==1)  %>%
  group_by(studyyear,tr) %>%
  summarise(N.kids=n())
N.diar <-
  diar %>%
  subset(., !is.na(.$diar7d) & dcohort==1|!is.na(.$diar7d) &u36==1)  %>%
  group_by(diar7d,studyyear,tr) %>%
  summarise(N.diar=n())

diar_n<-cbind(N.kids[,3], N.diar[9:16,4])

#Fix up this ordering
#diar_n<-diar_n[c(1,8,2:7,9,16,10:15,17,24,18:23),]
diar_t2_n<-diar_n[1:8,]
colnames(diar_t2_n)<-c("N","n")



####Enrollment: Prevalence and 95% CI by arm
grouped.diarprev <-
  diar %>%
  subset(., !is.na(.$diar7d) & dcohort==1|!is.na(.$diar7d) &u36==1)  %>%
  group_by(studyyear,tr) %>%
  do(as.data.frame(washb_mean(Y=.$diar7d, id=.$clusterid, print=F)))


diar_t2_prev<-cbind(grouped.diarprev[1:8,4], grouped.diarprev[1:8,7], grouped.diarprev[1:8,8])%>%set_rownames(arms)

colnames(diar_t2_prev)<-c("Mean","95%CI.L","95%CI.U")



####H1: Unadjusted prevalence ratios; each arm vs. control. PR, CI, MH P-value
#Merge in variables needed from the enrolment dataset.
enroltomerge<-subset(enrol, select=c('childid','Ncomp','momage','momedu','momheight','dminwat','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','cow','goat','dog','chicken','roof', 'floor','HHS' ))

dim(diar)

diar_enrol<-merge(diar, enroltomerge, by=c("childid"),all.x=T,all.y=F)
colnames(diar_enrol)

dim(diar_enrol)


#subset the diarrhea to children <36 mos at enrollment
#or target children
ad <-
  diar_enrol %>%
  subset(., dcohort==1) %>%
  subset(., !is.na(.$diar7d))

dim(ad)
table(ad$diar7d, ad$tr,ad$studyyear)
table(ad$tr,ad$studyyear)
table(ad$block,ad$tr )

h1.contrasts <- list(c("Control","Passive Control"), c("Control","Water"), c("Control","Sanitation"), c("Control","Handwashing"), c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))

#Run mh function to calculate PR and RD for H1
diar_h1_pr_unadj<-matrix(0, nrow=7, ncol=7)
diar_h1_rd_unadj<-matrix(0, nrow=7, ncol=6)

for(i in 1:7){
  diar_h1_pr_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h1.contrasts[[i]],measure="RR")
  diar_h1_rd_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h1.contrasts[[i]],measure="RD")
}
rownames(diar_h1_rd_unadj) <- rownames(diar_h1_pr_unadj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
colnames(diar_h1_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(diar_h1_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")




####H2: Unadjusted prevalence ratios and risk differences; combined WSH vs. single arms.  PR, CI, MH P-value
h2.contrasts <- list(c("Water","WSH"), c("Sanitation","WSH"), c("Handwashing","WSH"))

#Run mh function to calculate RD and RR for H2
diar_h2_pr_unadj<-matrix(0, nrow=3, ncol=7)
diar_h2_rd_unadj<-matrix(0, nrow=3, ncol=6)

for(i in 1:3){
  diar_h2_pr_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h2.contrasts[[i]],measure="RR")
  diar_h2_rd_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h2.contrasts[[i]],measure="RD")
}
rownames(diar_h2_rd_unadj) <-rownames(diar_h2_pr_unadj) <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
colnames(diar_h2_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(diar_h2_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")


####H3 contrasts for alternative diarrhea figure
h3.contrasts <- list(c("Nutrition","Nutrition + WSH"), c("WSH","Nutrition + WSH"))

#Run mh function to calculate RD and RR for H2
diar_h3_pr_unadj<-matrix(0, nrow=2, ncol=7)
diar_h3_rd_unadj<-matrix(0, nrow=2, ncol=6)

for(i in 1:2){
  diar_h3_pr_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h3.contrasts[[i]],measure="RR")
  diar_h3_rd_unadj[i,]<-washb_mh(Y=ad$diar7d, tr=ad$tr, strat=ad$block, contrast = h3.contrasts[[i]],measure="RD")
}
rownames(diar_h3_rd_unadj) <-rownames(diar_h3_pr_unadj) <-c("WSH+N v Nutrition","WSH+n v WSH")
colnames(diar_h3_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(diar_h3_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")



####H1: P-value from permutation test for unadjusted prevalence ratios; each arm vs. control
#Order data to replicate SL
ad <- ad[order(ad$block,ad$clusterid,ad$hhid,ad$childid,ad$studyyear),]

# X is not a balanced factor error in permutation test
# #Run permutation function
# diar_h1_pval_unadj<-t(sapply(h1.contrasts, washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=100000, seed=67890))
# diar_h2_pval_unadj<-t(sapply(h2.contrasts, washb_permute, Y=ad$diar7d, tr=ad$tr, pair=ad$block, nreps=100000, seed=67890))
# 
# diar_h1_pval_unadj<-(as.matrix(unlist(diar_h1_pval_unadj[,1])))
# diar_h2_pval_unadj<-(as.matrix(unlist(diar_h2_pval_unadj[,1])))
# 
# rownames(diar_h1_pval_unadj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
# rownames(diar_h2_pval_unadj) <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")




##Adjusted
####H1: adjusted prevalence ratios; each arm vs. control. PR, CI
#Adjustment variables to include
#month included within diar and anthro datasets.
Wvars<-c('sex','aged','month','momage','momedu','momheight','Ncomp','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','roof','floor','cow','goat','dog','chicken','dminwat','HHS','fracode')

ad %>% subset(., select=c(Wvars)) %>% summarise_all(mean)

#Create the fracode staff experience factor
ad$staffid[ad$staffid==327] = 0
ad$staffid[ad$staffid==460] = 0
ad$staffid[ad$staffid==1213] = 0
ad$staffid[ad$staffid==1400] = 0
ad$staffid[ad$staffid==1405] = 0
ad$staffid[ad$staffid==1723] = 0
ad$staffid[ad$staffid==1727] = 0
ad$staffid[ad$staffid==1728] = 0
ad$staffid[ad$staffid==1830] = 0
ad$staffid[ad$staffid==2105] = 0
ad$staffid[ad$staffid==2112] = 0
ad$staffid[ad$staffid==2174] = 0
ad$staffid[ad$staffid==2217] = 0
ad$staffid[ad$staffid==2242] = 0
ad$staffid[ad$staffid==2311] = 0
ad$staffid[ad$staffid==2321] = 0
ad$staffid[ad$staffid==2328] = 0
ad$staffid[ad$staffid==2674] = 0
ad$staffid[ad$staffid==2847] = 0
ad$staffid[ad$staffid==3102] = 0
ad$staffid[ad$staffid==3322] = 0
ad$staffid[ad$staffid==3323] = 0
ad$staffid[ad$staffid==3352] = 0
ad$staffid[ad$staffid==3357] = 0
ad$staffid[ad$staffid==3408] = 0
ad$staffid[ad$staffid==3410] = 0
ad$staffid[ad$staffid==3418] = 0
ad$staffid[ad$staffid==3420] = 0
ad$staffid[ad$staffid==3421] = 0
ad$staffid[ad$staffid==3424] = 0
ad$staffid[ad$staffid==3425] = 0
ad$staffid[ad$staffid==3435] = 0
ad$staffid[ad$staffid==3436] = 0
ad$staffid[ad$staffid==3437] = 0
ad$staffid[ad$staffid==3783] = 0
ad$staffid[ad$staffid==4187] = 0
ad$staffid[ad$staffid==4328] = 0
ad$staffid[ad$staffid==4345] = 0
ad$staffid[ad$staffid==4347] = 0
ad$staffid[ad$staffid==4348] = 0
ad$staffid[ad$staffid==4382] = 0
ad$staffid[ad$staffid==4433] = 0
ad$staffid[ad$staffid==4438] = 0
ad$staffid[ad$staffid==4471] = 0
ad$staffid[ad$staffid==4515] = 0
ad$staffid[ad$staffid==4518] = 0
ad$staffid[ad$staffid==4522] = 0
ad$staffid[ad$staffid==4531] = 0
ad$staffid[ad$staffid==4548] = 0
ad$staffid[ad$staffid==4645] = 0
ad$staffid[ad$staffid==5422] = 0
ad$staffid[ad$staffid==7383] = 0
ad$staffid[ad$staffid==8152] = 0
ad$staffid[ad$staffid==8274] = 0
ad$staffid[ad$staffid==8604] = 0
ad$staffid[ad$staffid==8787] = 0
ad$staffid[ad$staffid==8883] = 0
ad$staffid[ad$staffid==8884] = 0
ad$staffid[ad$staffid==9999] = 0

ad$fracode <- factor(ad$staffid)



#Order data to replicate SL
ad <- ad[order(ad$block,ad$clusterid,ad$hhid,ad$childid,ad$studyyear),]



#subset W adjustment set
diarW<- subset(ad, select=Wvars)

#check that all the factor variables are set
for(i in 1:length(Wvars)){
  print(colnames(diarW)[i])
  print(class(diarW[,i])  )
}

#set covariates as factors
diarW$month<-as.factor(diarW$month)
diarW$HHS<-as.factor(diarW$HHS)
diarW$electricity<-as.factor(diarW$electricity)
diarW$radio<-as.factor(diarW$radio)
diarW$television<-as.factor(diarW$television)
diarW$mobile<-as.factor(diarW$mobile)
diarW$clock<-as.factor(diarW$clock)
diarW$bicycle<-as.factor(diarW$bicycle)
diarW$motorcycle<-as.factor(diarW$motorcycle)
diarW$stove<-as.factor(diarW$stove)
diarW$roof<-as.factor(diarW$roof)
diarW$floor<-as.factor(diarW$floor)
ad$block<-as.factor(ad$block)




#Run TMLE for the adjusted parameter estimates
 #Create empty matrices to hold results
diar_h1_pr_adj<-matrix(0, nrow=7, ncol=3)
diar_h1_rd_adj<-matrix(0, nrow=7, ncol=3)





for(i in 1:7){
  temp<-washb_tmle(Y=ad$diar7d, tr=ad$tr, W=diarW, id=ad$block,pair=ad$block,family="binomial", contrast= h1.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=T)
  diar_h1_pr_adj[i,1]<-temp$estimates$RR$psi
  diar_h1_pr_adj[i,2:3]<-temp$estimates$RR$CI
  #diar_h1_pr_adj[i,4]<-temp$estimates$RR$pvalue
  diar_h1_rd_adj[i,1]<-temp$estimates$ATE$psi
  diar_h1_rd_adj[i,2:3]<-temp$estimates$ATE$CI
  #diar_h1_rd_adj[i,4]<-temp$estimates$ATE$pvalue
  
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h1.contrasts[[i]], id=ad$clusterid, family=poisson(link="log"), print=F, verbose=F)
  #diar_h1_pr_adj[i,]<-as.matrix(temp$TR)
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h1.contrasts[[i]], id=ad$clusterid, family="gaussian", print=F, verbose=F)
  #diar_h1_rd_adj[i,]<-as.matrix(temp$TR)
}


#colnames(diar_h1_pr_adj)<-c("PR","95CI lb","95CI ub","logPR","logSE","Zval","Pval")
#colnames(diar_h1_rd_adj)<-c("Risk Diff","95CI lb","95CI ub","SE","Zval","Pval")
colnames(diar_h1_pr_adj)<-c("PR","95CI lb","95CI ub")
colnames(diar_h1_rd_adj)<-c("RD","95CI lb","95CI ub")
rownames(diar_h1_rd_adj) <- rownames(diar_h1_pr_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")


####H2: Adjusted prevalence ratios; combined WSH vs. single arms.  PR, CI
diar_h2_pr_adj<-matrix(0, nrow=3, ncol=3)
diar_h2_rd_adj<-matrix(0, nrow=3, ncol=3)
for(i in 1:3){
  temp<-washb_tmle(Y=ad$diar7d, tr=ad$tr, W=diarW, id=ad$block,pair=ad$block,family="binomial", contrast= h2.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=F)
  diar_h2_pr_adj[i,1]<-temp$estimates$RR$psi
  diar_h2_pr_adj[i,2:3]<-temp$estimates$RR$CI
  #diar_h1_pr_adj[i,4]<-temp$estimates$RR$pvalue
  diar_h2_rd_adj[i,1]<-temp$estimates$ATE$psi
  diar_h2_rd_adj[i,2:3]<-temp$estimates$ATE$CI
  
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h2.contrasts[[i]], id=ad$clusterid, family=poisson(link="log"), print=FALSE)
  #diar_h2_pr_adj[i,]<-as.matrix(temp$TR)
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h2.contrasts[[i]], id=ad$clusterid, family="gaussian", print=FALSE)
  #diar_h2_rd_adj[i,]<-as.matrix(temp$TR)
}


#colnames(diar_h2_pr_adj)<-c("PR","95CI lb","95CI ub","logPR","logSE","Zval","Pval")
#colnames(diar_h2_rd_adj)<-c("Risk Diff","95CI lb","95CI ub","SE","Zval","Pval")
colnames(diar_h2_pr_adj)<-c("PR","95CI lb","95CI ub")
colnames(diar_h2_rd_adj)<-c("RD","95CI lb","95CI ub")
rownames(diar_h2_rd_adj) <- rownames(diar_h2_pr_adj)  <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")





# 
# 
# ####H2: Adjusted difference in prevalence; combined WSH vs. single arms.  PR, CI
# SLd <- data.frame(id=ad$clusterid,block=ad$block,tr=ad$tr,Y=ad$diar7d, studyyear=ad$studyyear,diarW)
# SLd$fracode<-factor(SLd$fracode)
# 
# 
# 
# 
# 
# # pre-screen the covariates for those associated with the outcome (LR test P<0.2)
# # see Wprescreen() and design.matrix() in the base functions
# Wscreen <- washb_prescreen(Y=SLd$Y,Ws=SLd[,6:ncol(SLd)],family="binomial")
# Wselect <- subset(SLd,select=Wscreen)
# 
# 
# 
# # restrict to complete cases
# SLd <- data.frame(childid=ad$childid,svy=ad$studyyear,id=ad$clusterid,hhid=ad$hhid,block=ad$block,tr=ad$tr,Y=ad$diar7d,Wselect)
# dim(SLd)
# SLd <- SLd[complete.cases(SLd),]
# dim(SLd)
# 
# #Order to match Jade
# SLd <- SLd[order(SLd$block,SLd$id,SLd$hhid,SLd$childid,SLd$svy),]
# 
# Wselect<-SLd[,c(8:ncol(SLd))]
# Wselect <- design_matrix(Wselect)
# dim(Wselect)
# colnames(Wselect)
# 
# 
# set.seed(67890)
# SLfit1 <- SuperLearner(Y=SLd$Y,X=Wselect,id=SLd$id,
#                        family="binomial",
#                        SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
# )
# SLfit1
# SLd$pY <- as.vector(predict(SLfit1)$pred)
# SLd$r <- SLd$Y-SLd$pY
# 
# 
# 
# 
# colnames(SLfit1)
# 
# 
# #create empty matrices for permutation function results
# diar_h1_pval_adj<-matrix(nrow=7,ncol=1)
# diar_h2_pval_adj<-matrix(nrow=3,ncol=1)
# 
# #Run permutation function for H1
# for(i in 1:7){
#   temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=100000,contrast=h1.contrasts[[i]], seed=67890)
#   diar_h1_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
# }
# 
# #washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=100000,contrast=c("Control","Nutrition + WSH"), seed=4397157)
# 
# 
# 
# #Run permutation function for H2
# for(i in 1:3){
#   temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=100000,contrast=h2.contrasts[[i]], seed=67890)
#   diar_h2_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
# }
# 
# rownames(diar_h1_pval_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
# rownames(diar_h2_pval_adj) <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
# 

#-----------------------------------
#save data
#-----------------------------------

diar_t2_n_24s_A<-diar_t2_n
diar_t2_prev_24s_A<-diar_t2_prev

diar_h1_pr_unadj_24s_A<- diar_h1_pr_unadj
diar_h1_rd_unadj_24s_A<-  diar_h1_rd_unadj
diar_h2_pr_unadj_24s_A<- diar_h2_pr_unadj
diar_h2_rd_unadj_24s_A<- diar_h2_rd_unadj
diar_h3_pr_unadj_24s_A<- diar_h3_pr_unadj
diar_h3_rd_unadj_24s_A<- diar_h3_rd_unadj
diar_h1_pr_adj_24s_A<- diar_h1_pr_adj
diar_h1_rd_adj_24s_A<-  diar_h1_rd_adj
diar_h2_pr_adj_24s_A<- diar_h2_pr_adj
diar_h2_rd_adj_24s_A<- diar_h2_rd_adj
# diar_h1_pval_unadj_24s_A<-  diar_h1_pval_unadj
# diar_h2_pval_unadj_24s_A<- diar_h2_pval_unadj
# diar_h1_pval_adj_24s_A<-  diar_h1_pval_adj
# diar_h2_pval_adj_24s_A<- diar_h2_pval_adj

save(diar_t2_n_24s_A, diar_t2_prev_24s_A, 
     diar_h1_pr_unadj_24s_A, diar_h1_rd_unadj_24s_A, 
     diar_h2_pr_unadj_24s_A, diar_h2_rd_unadj_24s_A, 
     diar_h1_pr_adj_24s_A, diar_h1_rd_adj_24s_A, 
     diar_h2_pr_adj_24s_A, diar_h2_rd_adj_24s_A,
     # diar_h1_pval_unadj_24s_A,diar_h2_pval_unadj_24s_A,
     # diar_h1_pval_adj_24s_A,diar_h2_pval_adj_24s_A, 
     diar_h3_pr_unadj_24s_A, diar_h3_rd_unadj_24s_A,
     file="diar_24s_Andrew.RData")















