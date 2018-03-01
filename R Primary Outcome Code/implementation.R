

#---------------------------------------
# implementaiton.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Implementation table
#---------------------------------------

#---------------------------------------
# input files:
#	washb-kenya-implementation.csv
#
# output files:
#	kenya-implementation.RData
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
d<-read.dta("washb-kenya-implementation.dta")



# re-order the tr factor for convenience
d$tr <- factor(d$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

# number of observations
d.tr <- group_by(d, tr)
ncompounds <- summarise(d.tr,n=n())
ncompounds[9,]<-sum(ncompounds[,2])
ncompounds[,3]<-ncompounds[,2]/sum(ncompounds[1:8,2])


# calculate N obs and means for each variable
colnames(d)
vlist <- c("momage","momedu_prim","live_spouse","RESagwork","patedu","agwork","numHHs","numPPL","Nlt18","electricity","cementfloor","ironroof","improvedwater","dminwat","treatwat","odmen","odwom","odch38","odchu3","latown","impr_lat","humfeces","wat_avail" , "soap_avail","HHSmod_sev","child_liq","child_food")


ns <- sapply(d[vlist],function(x) tapply(x,d$tr,function(y) length(y[!is.na(y)])))
mus <- sapply(d[vlist],function(x) tapply(x,d$tr,function(y) mean(y,na.rm=T)))

#Add in combined row
ns<-rbind(ns, colSums(ns))
rownames(ns)[9]<-"total"

mus<-rbind(mus, sapply(d[vlist],function(x) mean(x,na.rm=T)))
rownames(mus)[9]<-"total"

#---------------------------------------
# combine results into a single dataframe
#---------------------------------------
balance.tab.n <- t(ns)
balance.tab.n<-rbind(t(ncompounds[,2]),balance.tab.n)
balance.tab.mu <- t(mus)
balance.tab.mu<-rbind(t(ncompounds[,3]),balance.tab.mu)


impl <- data.frame(
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
    balance.tab.n[,9],
    balance.tab.mu[,9],
   stringsAsFactors = F
)

names(impl) <- c("variable",paste(rep(colnames(balance.tab.n),rep(2,9)),rep(c(".n",".mu"),9),sep=""))

# print table
impl

#dim(impl)
#impl<-data.frame(impl[,1],impl[,3],impl[,5],impl[,7],impl[,9],impl[,11],impl[,13],impl[,15],impl[,17],impl[,19])
#names(impl) <- c("variable",paste(rep(colnames(balance.tab.n),rep(2,9)),rep(c(".n",".mu"),9),sep=""))

#---------------------------------------
# save objects 
#(drop datasets to save space)
#---------------------------------------
rm(d)


#format

#drop n columns
impl = impl[,c(1,3,5,7,9,11,13,15,17,19)]
impl = impl[-1,]
#impl<-t
for(i in c(2:10)){
  for(j in c(1,7:9,14)){
    impl[j,i]=sprintf("%1.0f",as.numeric(impl[j,i]))
  }
  for(j in c(2:6,10:13,15:27)){
    impl[j,i]=sprintf("%1.1f",as.numeric(impl[j,i])*100)
  } 
}

Age (years)

blank=rep("",9)

impl=   rbind(
               c("\\textbf{Maternal}",blank),
               impl[c(1:4),],
               c( "\\textbf{Paternal}",blank),
               impl[c(5:6),],
               c("\\textbf{Household}",blank),
               impl[c(7:12),],
               c("\\textbf{Drinking Water}",blank),
               impl[c(13:15),],
               c("\\textbf{Sanitation}",blank),
               c("Always or usually use primary toilet for defecation",blank),
               impl[c(16:17),],
               c("Daily defecating in the open",blank),
               impl[c(18:19),],
               c("Latrine",blank),
               impl[c(20:22),],
               c("\\textbf{Handwashing location}",blank),
               impl[c(23:24),],
               c("\\textbf{Nutrition}",blank),
               impl[c(25:27),])

rownames(impl)=NULL



impl[2,1]<-"~~~Age (years)"
impl[3,1]<-"~~~Completed at least primary"
impl[4,1]<-"~~~Lives with spouse"
impl[5,1]<-"~~~Works in agriculture"
impl[7,1]<-"~~~Completed at least primary"
impl[8,1]<-"~~~Works in agriculture"
impl[10,1]<-"~~~Number of households per compound"
impl[11,1]<-"~~~Number of people per compound"
impl[12,1]<-"~~~Number of children <18 years in the household"
impl[13,1]<-"~~~Has electricity"
impl[14,1]<-"~~~Has a cement floor"
impl[15,1]<-"~~~Has an iron roof"
impl[17,1]<-"~~~Primary drinking water source is improved"
impl[18,1]<-"~~~One-way walking time to primary water source (minutes)"
impl[19,1]<-"~~~Reported treating currently stored water"
impl[22,1]<-"~~~~~Adult men"
impl[23,1]<-"~~~~~Adult women"
impl[25,1]<-"~~~~~Children 3 to $<$8 years"
impl[26,1]<-"~~~~~Children 0 to $<$3 years"
impl[28,1]<-"~~~~~Own any latrine"
impl[29,1]<-"~~~Access to improved latrine"
impl[30,1]<-"~~~Human feces observed in the compound"
impl[32,1]<-"~~~Has water within 2 meters of handwashing location"
impl[33,1]<-"~~~Has soap within 2 meters of handwashing location"
impl[35,1]<-"~~~Prevalence of moderate to severe household hunger"
impl[36,1]<-"~~~Gave child liquids before six months"
impl[37,1]<-"~~~Gave child solid food before six months"

impl[21,1]<-"~~~Always or usually use primary toilet for defecation"
impl[24,1]<-"~~~Daily defecating in the open"
impl[27,1]<-"~~~Latrine"

impl

#load in HH #s
load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/table1_f.RData")
n.hh.f<-cbind(n.hh.f,"(N=8246)")

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
save(n.hh.f,impl,file="kenya-implementation.Rdata")


