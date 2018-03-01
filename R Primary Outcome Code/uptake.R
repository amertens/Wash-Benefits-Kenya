
#---------------------------------------
# uptake.R
#
# andrew mertens amertens@berkeley.edu)
#
# summarize measures of uptake / compliance
# by study arm and measurement round
# (enrollment, year 1, year 2)
#---------------------------------------

#---------------------------------------
# input files:
#	washb-kenya-uptake.csv
#
# output files:
# kenya-uptake.RData
#
#---------------------------------------


#---------------------------------------
# preamble
#---------------------------------------
rm(list=ls())
try(detach(package:plyr))
library(dplyr)
library(washb)
library(foreign)



#Test function
#Y=d$promoter_vis
#tr=d$tr
#svy=d$studyyear
#s=1
#id=d$clusterid
#family="binomial"
#group="Control"
#print=T

#wrapper function to call washb_mean
mean.est <- function(Y,tr,svy,id,group="Control",s=0,print=FALSE) {
  # Y : outcome variable
  # tr: treatment indicator variable
  # svy  : measurment round variable
  # id: cluster ID variable
  # group : string. treatment factor level to compute mean
  # s     : survey round to compute mean. 0, 1, or 2
  require(washb)
  dat <- data.frame(id=id[tr==group & svy==s],
                        svy=svy[tr==group & svy==s],
                        Y=Y[tr==group & svy==s],
                        tr=tr[tr==group & svy==s])
  dat <- dat[complete.cases(dat),]
  fit <- washb_mean(Y=dat$Y,
                 id=dat$id,
                 print=F
                 )
  if(print==TRUE) print(fit)
  res<-fit[c(2,5,6)]
  names(res) <- c("mean","ci.lb","ci.ub")
  return(res)
}



#---------------------------------------
# load the uptake analysis dataset
#---------------------------------------
setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew")
#d <- read.csv("washb-kenya-uptake.csv")
d <- read.dta("washb-kenya-uptake.dta")

#create 2 empty rows to add for midline and endline passive control, so empty columns get added to the table
d[(nrow(d)+1),]<-rep(NA,ncol(d))
d[(nrow(d)+1),]<-rep(NA,ncol(d))
d[(nrow(d)-1),4]<-"Passive Control"
d[(nrow(d)),4]<-"Passive Control"
d[(nrow(d)-1),"studyyear"]<-1
d[(nrow(d)),"studyyear"]<-2
tail(d)



# re-order the treatment factor for convenience
d$tr <- factor(d$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

#---------------------------------------
# for each uptake indicator, summarize
# the number of obs and the % at each
# measurement round
#---------------------------------------

d.svy <- group_by(d, tr,studyyear)

uptake_n <- d.svy %>% 
                filter(  !is.na(promoter_vis) | 
                         !is.na(freechl) | 
                         !is.na(impr_lat) | 
                         !is.na(ch_feces_safe_disp) | 
                         !is.na(watsoap_avail) |
                         !is.na(lnsn) |
                         !is.na(lnsp)) %>%
                summarise(.,n=n())

      

# promoter visit
promoter_vis <- summarise(d.svy,n=sum(!is.na(promoter_vis)),count=sum(promoter_vis,na.rm=T),mean=mean(promoter_vis,na.rm=T))
print(promoter_vis, n=24)

# store water with detectable chlorine
freechl <- summarise(d.svy,n=sum(!is.na(freechl)),count=sum(freechl,na.rm=T),mean=mean(freechl,na.rm=T))
print(freechl, n=24)

# access to improved latrine
impr_lat <- summarise(d.svy,n=sum(!is.na(impr_lat)),count=sum(impr_lat,na.rm=T),mean=mean(impr_lat,na.rm=T))
print(impr_lat, n=24)

# Child feces safetly disposed
ch_feces_safe_disp <- summarise(d.svy,n=sum(!is.na(ch_feces_safe_disp)),count=sum(ch_feces_safe_disp,na.rm=T),mean=mean(ch_feces_safe_disp,na.rm=T))
print(ch_feces_safe_disp, n=24)

# handwashing location has water and soap
watsoap_avail <- summarise(d.svy,n=sum(!is.na(watsoap_avail)),count=sum(watsoap_avail,na.rm=T),mean=mean(watsoap_avail,na.rm=T))
print(watsoap_avail, n=24)

# Mean sachets of LNS fed in prior week to index child 6-24 mos
lnsp <- summarise(d.svy,n=sum(!is.na(lnsp)),mean=mean(lnsp,na.rm=T))
print(lnsp, n=24)

#---------------------------------------
# combine estimates into a single matrix that matches Jade's
#---------------------------------------
uptake.tabn <- as.data.frame(
  rbind(
    promoter_vis$n,
    freechl$n,
    impr_lat$n,
    ch_feces_safe_disp$n,
    watsoap_avail$n,
    lnsp$n
))
uptake.tabmean <- as.data.frame(
  rbind(
  promoter_vis$mean,
  freechl$mean,
  impr_lat$mean,
  ch_feces_safe_disp$mean,
  watsoap_avail$mean,
  lnsp$mean
))
names(uptake.tabn) <- paste(rep(levels(d$tr),rep(3,8)),c("0 n","1 n","2 n"))
names(uptake.tabmean) <- paste(rep(levels(d$tr),rep(3,8)),c("0 ave","1 ave","2 ave"))



uptake.tab<-cbind(uptake.tabn[,1],uptake.tabmean[,1])
for(i in 2:(ncol(uptake.tabn))){
  uptake.tab<-cbind(uptake.tab,uptake.tabn[,i],uptake.tabmean[,i])
}
uptake.tab<-as.data.frame(uptake.tab)
names(uptake.tab) <- paste(rep(levels(d$tr),rep(6,8)),c("0 n","0 ave","1 n","1 ave","2 n","2 ave"))
uptake.tab$label<-c(
  "Promoter visit",
  "Store water with detectable free chlorine",
  "Access to improved latrine",
  "Child feces safetly disposed",
  "Primary handwashing station has water and soap",
  "LNS sachet consumption % of expected"
)

# reorder label
uptake_table_A <- uptake.tab[,c(ncol(uptake.tab),1:(ncol(uptake.tab)-1))]

# print table

uptake_table_A

#replace NA's
uptake_table_A[is.na(uptake_table_A)]<-0

library(xlsx)
write.xlsx(uptake_table_A, "KenyaUptakeTable.xlsx", sheetName="Sheet1")

#compare to jade's
setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade")
load("uptake_table.Rdata")
try(uptake_table_j)

j=uptake_table_j[,2:ncol(uptake_table_j)]
A=uptake_table_A[,2:ncol(uptake_table_A)]

j-A




setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
save(uptake_table_A, uptake_n, file="uptake_table_A.Rdata")

#---------------------------------------
# Calculate means and 95% CIs by survey round
#---------------------------------------
arms <- levels(d$tr)

# promoter_vis

#promoter_vis0 <- sapply(arms, mean.est,Y=d$promoter_vis,tr=d$tr,svy=d$studyyear,id=d$clusterid,s=0, print=T)
promoter_vis0 <- matrix(NA, nrow=3, ncol=8)
promoter_vis1 <- sapply(arms,mean.est,Y=d$promoter_vis,tr=d$tr,svy=d$studyyear,id=d$clusterid,s=1, print=T)
promoter_vis2 <- sapply(arms,mean.est,Y=d$promoter_vis,tr=d$tr,svy=d$studyyear,id=d$clusterid,s=2, print=T)

# store water with detectable chlorine
# note: since there were no events in any of the non-water arms (actually, 1 in HW)
# we cannot estimate 95% CIs. Will pad the matrix with zeros and NAs
 #freechl0 <- sapply(arms,mean.est,Y=d$freechl,tr=d$tr,svy=d$svy,id=d$clusterid,s=0)
 warms <- c("Water","WSH","Nutrition + WSH")
 freechl1 <- sapply(warms,mean.est,Y=d$freechl,tr=d$tr,svy=d$studyyear,id=d$clusterid,s=1)
 # pad with zeros and missings for other treatment arms
  freechl1 <- cbind(c(0,NA,NA),c(0,NA,NA),freechl1[,1],c(0,NA,NA),c(0,NA,NA),freechl1[,2],c(0,NA,NA),freechl1[,3])
  colnames(freechl1) <- colnames(promoter_vis1)

  
  freechl2 <- matrix(NA, nrow=3, ncol=8)
 #freechl2 <- sapply(warms,mean.est,Y=d$freechl,tr=d$tr,svy=d$svy,id=d$clusterid,s=2)
  # pad with zeros and missings for other treatment arms
  freechl2 <- cbind(c(0,NA,NA),c(0,NA,NA),freechl2[,1],c(0,NA,NA),c(0,NA,NA),freechl2[,2],c(0,NA,NA),freechl2[,3])
  colnames(freechl2) <- colnames(storewat2)



  

  

  

# Access to improved latrine"
latfeces0 <- sapply(arms,mean.est,Y=d$latfeces,tr=d$tr,svy=d$svy,id=d$clusterid,s=0)
latfeces1 <- sapply(arms,mean.est,Y=d$latfeces,tr=d$tr,svy=d$svy,id=d$clusterid,s=1)
latfeces2 <- sapply(arms,mean.est,Y=d$latfeces,tr=d$tr,svy=d$svy,id=d$clusterid,s=2)

#Child feces safetly disposed
humfeces0 <- sapply(arms,tmle.mean.est,Y=d$humfeces,tr=d$tr,svy=d$svy,id=d$clusterid,s=0)
humfeces1 <- sapply(arms,tmle.mean.est,Y=d$humfeces,tr=d$tr,svy=d$svy,id=d$clusterid,s=1)
humfeces2 <- sapply(arms,tmle.mean.est,Y=d$humfeces,tr=d$tr,svy=d$svy,id=d$clusterid,s=2)

# Primary handwashing station has water and soap
hwsw0 <- sapply(arms,tmle.mean.est,Y=d$hwsw,tr=d$tr,svy=d$svy,id=d$clusterid,s=0)
hwsw1 <- sapply(arms,tmle.mean.est,Y=d$hwsw,tr=d$tr,svy=d$svy,id=d$clusterid,s=1)
hwsw2 <- sapply(arms,tmle.mean.est,Y=d$hwsw,tr=d$tr,svy=d$svy,id=d$clusterid,s=2)

# Mean sachets of LNS fed in prior week to index child 6-24 mos (not measured at enrollment, only measured in nutrition arms)
narms <- arms[grep("Nutrition",arms)]
rlnsp1 <- sapply(narms,tmle.mean.est,Y=d$rlnsp,tr=d$tr,svy=d$svy,id=d$clusterid,s=1,family="gaussian")
  # pad with missings for other treatment arms
  rlnsp1 <- cbind(matrix(NA,nrow=3,ncol=5),rlnsp1)
  colnames(rlnsp1) <- colnames(hwsws1)

rlnsp2 <- sapply(narms,tmle.mean.est,Y=d$rlnsp,tr=d$tr,svy=d$svy,id=d$clusterid,s=2,family="gaussian")
  # pad with missings for other treatment arms
  rlnsp2 <- cbind(matrix(NA,nrow=3,ncol=5),rlnsp2)
  colnames(rlnsp2) <- colnames(hwsws2)



#---------------------------------------
# save the objects
#---------------------------------------
rm(d)
save.image(file="C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/kenya-uptake.RData")




