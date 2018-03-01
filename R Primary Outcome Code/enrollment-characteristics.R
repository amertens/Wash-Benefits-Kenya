


#---------------------------------------
# enrollment-characteristics.R
#
# andrew mertens (amertens@berkeley.edu)
#
# summarize enrollment characteristics
# by treatment arm
#---------------------------------------

#---------------------------------------
# input files:
#	washb-kenya-enrol.csv
#
# output files:
#	kenya-enrol-characteristics_A.RData
#
#---------------------------------------


#---------------------------------------
# preamble
#---------------------------------------
rm(list=ls())
library(dplyr)
library(foreign)

#---------------------------------------
# CONSORT: compounds
#---------------------------------------



setwd("C:/Users/andre/Dropbox/Kenya Primary Analysis/Data-selected/clean")
d<-read.dta("washb-kenya-table1.dta")



# re-order the tr factor for convenience
d$tr <- factor(d$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

# number of observations
d.tr <- group_by(d, tr)
ncompounds <- summarise(d.tr,n=n())
ncompounds[,3]<-ncompounds[,2]/sum(ncompounds[,2])
# calculate N obs and means for each variable
colnames(d)
vlist <- c("momage","momedu_prim","patedu","agwork","Nlt18","electricity","cementfloor","ironroof","improvedwater","dminwat","treatwat","odmen","odwom","odch38","odchu3","latown","impr_lat","humfeces","wat_avail" , "soap_avail","HHSmod_sev")

#vlist <- c("momage","momeduy","dadeduy","dadagri","Nhh","elec","cement","landacre","tubewell","storewat","treatwat","odmen","odwom","odch815","odch38","odchu3","latown","latslab","latseal","humfeces","humfecesch","hwlatwat","hwlatsoap","hwkitwat","hwkitsoap")

#Tabulate missingness
for(i in 1:length(vlist)){
  cat(vlist[i],": \n")
  print(table(is.na(d[vlist[i]])))
}

ns <- sapply(d[vlist],function(x) tapply(x,d$tr,function(y) length(y[!is.na(y)])))
mus <- sapply(d[vlist],function(x) tapply(x,d$tr,function(y) mean(y,na.rm=T)))
sd <- sapply(d[vlist],function(x) tapply(x,d$tr,function(y) sd(y,na.rm=T)))

#---------------------------------------
# combine results into a single dataframe
#---------------------------------------
balance.tab.n <- t(ns)
balance.tab.n<-rbind(t(ncompounds[,2]),balance.tab.n)
balance.tab.mu <- t(mus)
balance.tab.mu<-rbind(t(ncompounds[,3]),balance.tab.mu)


table1 <- data.frame(
   variable=rownames(balance.tab.n),
   balance.tab.n[,1],
    balance.tab.mu[,1],
    balance.tab.n[,2],
    balance.tab.mu[,2],
    balance.tab.n[,3],
    balance.tab.mu[,3],
    balance.tab.n[,4],
    balance.tab.mu[,4],
    balance.tab.n[,5],
    balance.tab.mu[,5],
    balance.tab.n[,6],
    balance.tab.mu[,6],
    balance.tab.n[,7],
    balance.tab.mu[,7],
    balance.tab.n[,8],
    balance.tab.mu[,8],
   stringsAsFactors = F
)
names(table1) <- c("variable",paste(rep(colnames(balance.tab.n),rep(2,8)),rep(c(".n",".mu"),8),sep=""))

# print table
table1

table1_A <- table1
rm(table1)

#---------------------------------------
# save objects 
#(drop datasets to save space)
#---------------------------------------
rm(d)

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
save(table1_A,file="kenya-enrol-characteristics_A.RData")




