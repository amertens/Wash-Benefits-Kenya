##############################################
# WASH Benefits Kenya

# Table with year 1 anthro results

# by Andrew
##############################################

rm(list=ls())

source("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Table/0-table-base-functions.R")

load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/laz_mean.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/laz_mean.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/whz_mean.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/hcz_mean.RData")

load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/laz_rd_unadj.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/laz_rd_unadj.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/whz_rd_unadj.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/hcz_rd_unadj.RData")

load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/laz_t1_pval_unadj.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/laz_t1_pval_unadj.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/whz_t1_pval_unadj.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/hcz_t1_pval_unadj.RData")


#----------------- control prevalence ----------------- 
laz_t1_prev=sprintf("%0.02f",laz_t1_n_j[,2])
laz_t1_prev=sprintf("%0.02f",laz_t1_n_j[,2])
whz_t1_prev=sprintf("%0.02f",whz_t1_n_j[,2])
hcz_t1_prev=sprintf("%0.02f",hcz_t1_n_j[,2])

#----------------- risk differences ----------------- 
rd.laz.h1.pt.est=c("",apply(as.matrix(laz_t1_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.laz.h3.pt.est=rbind(matrix("",7,2),apply(as.matrix(laz_t1_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.laz.h1.pt.est=c("",apply(as.matrix(laz_t1_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.laz.h3.pt.est=rbind(matrix("",7,2),apply(as.matrix(laz_t1_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.whz.h1.pt.est=c("",apply(as.matrix(whz_t1_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.whz.h3.pt.est=rbind(matrix("",7,2),apply(as.matrix(whz_t1_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.hcz.h1.pt.est=c("",apply(as.matrix(hcz_t1_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.hcz.h3.pt.est=rbind(matrix("",7,2),apply(as.matrix(hcz_t1_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

  
#----------------- rr table ----------------- 
blank=rep("",7)
table=data.frame(rbind(cbind(laz_t1_prev,rd.laz.h1.pt.est,rd.laz.h3.pt.est),
		cbind(laz_t1_prev,rd.laz.h1.pt.est,rd.laz.h3.pt.est),
      cbind(whz_t1_prev,rd.whz.h1.pt.est,rd.whz.h3.pt.est),
      cbind(hcz_t1_prev,rd.hcz.h1.pt.est,rd.hcz.h3.pt.est)))

lab=rep(c("Control","Passive Control","Water","Sanitation","Handwashing",
                           "Water + Sanitation + Handwashing (WSH)","Nutrition",
                           "Nutrition + WSH"),4)


anthroZ.table=data.frame(cbind(lab,table))

save(anthroZ.table,file="C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Table/table-anthroZ-t1-reformat.RData")

