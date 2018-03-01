##############################################
# WASH Benefits Kenya

# Table with adjusted and unadjusted anthro continuous results

# by Jade
##############################################

rm(list=ls())

load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/laz_mean.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/laz_rd_unadj.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/laz-PR-adj.RData")

load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/waz_mean.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/waz_rd_unadj.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/waz-PR-adj.RData")

load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/whz_mean.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/whz_rd_unadj.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/whz-PR-adj.RData")

load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/hcz_mean.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/hcz_rd_unadj.RData")
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/hcz-PR-adj.RData")

source("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Table/0-table-base-functions.R")

#----------------- control prevalence ----------------- 
laz_t1_prev=sprintf("%0.02f",laz_t1_n_j[,2])
waz_t1_prev=sprintf("%0.02f",waz_t1_n_j[,2])
whz_t1_prev=sprintf("%0.02f",whz_t1_n_j[,2])
hcz_t1_prev=sprintf("%0.02f",hcz_t1_n_j[,2])

#----------------- risk differences ----------------- 
# unadjusted
rd.laz.h1.pt.est.unadj=c("",apply(as.matrix(laz_t1_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.laz.h3.pt.est.unadj=c("",apply(as.matrix(laz_t1_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.waz.h1.pt.est.unadj=c("",apply(as.matrix(waz_t1_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.waz.h3.pt.est.unadj=c("",apply(as.matrix(waz_t1_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.whz.h1.pt.est.unadj=c("",apply(as.matrix(whz_t1_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.whz.h3.pt.est.unadj=c("",apply(as.matrix(whz_t1_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.hcz.h1.pt.est.unadj=c("",apply(as.matrix(hcz_t1_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.hcz.h3.pt.est.unadj=c("",apply(as.matrix(hcz_t1_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

# adjusted
rd.laz.h1.pt.est.adj=c("",apply(as.matrix(laz_t1_h1_rd_adj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.laz.h3.pt.est.adj=c("",apply(as.matrix(laz_t1_h3_rd_adj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.waz.h1.pt.est.adj=c("",apply(as.matrix(waz_t1_h1_rd_adj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.waz.h3.pt.est.adj=c("",apply(as.matrix(waz_t1_h3_rd_adj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.whz.h1.pt.est.adj=c("",apply(as.matrix(whz_t1_h1_rd_adj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.whz.h3.pt.est.adj=c("",apply(as.matrix(whz_t1_h3_rd_adj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.hcz.h1.pt.est.adj=c("",apply(as.matrix(hcz_t1_h1_rd_adj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.hcz.h3.pt.est.adj=c("",apply(as.matrix(hcz_t1_h3_rd_adj_j),1,pt.est.ci.f,decimals=2,scale=1))


#----------------- rr table ----------------- 
table.h1=data.frame(rbind(cbind(laz_t1_prev,rd.laz.h1.pt.est.unadj,rd.laz.h1.pt.est.adj),
                          cbind(waz_t1_prev,rd.waz.h1.pt.est.unadj,rd.waz.h1.pt.est.adj),
                          cbind(whz_t1_prev,rd.whz.h1.pt.est.unadj,rd.whz.h1.pt.est.adj),
                          cbind(hcz_t1_prev,rd.hcz.h1.pt.est.unadj,rd.hcz.h1.pt.est.adj)))

table.h3=data.frame(rbind(cbind(laz_t1_prev[c(8,7,6)],rd.laz.h3.pt.est.unadj,rd.laz.h3.pt.est.adj),
                          cbind(waz_t1_prev[c(8,7,6)],rd.waz.h3.pt.est.unadj,rd.waz.h3.pt.est.adj),
                          cbind(whz_t1_prev[c(8,7,6)],rd.whz.h3.pt.est.unadj,rd.whz.h3.pt.est.adj),
                          cbind(hcz_t1_prev[c(8,7,6)],rd.hcz.h3.pt.est.unadj,rd.hcz.h3.pt.est.adj)))

lab.h1=rep(c("Control","Passive Control","Water","Sanitation","Handwashing",
             "Water + Sanitation + Handwashing (WSH)","Nutrition",
             "Nutrition + WSH"),4)

lab.h3=rep(c("Nutrition + WSH","Nutrition",
             "WSH"),4)

anthroZ.adj.table.t1.h1=data.frame(cbind(lab.h1,table.h1))
anthroZ.adj.table.t1.h3=data.frame(cbind(lab.h3,table.h3))


save(anthroZ.adj.table.t1.h1,anthroZ.adj.table.t1.h3,
     file="C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/table-anthroZ-t1-adj.RData")




