
#---------------------------------------
# Spillover Analysis.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The spillover analysis in Wash Benefits Kenya, calculating distance between
# Control clusters and treatment arms, and comparing outcomes between lowest
# and highest distance control clusters
#
#---------------------------------------

#---------------------------------------
# preamble
#---------------------------------------
###Load in data
rm(list=ls())
try(detach(package:plyr))
library(dplyr)
library(maptools)
library(rgdal)
library(geosphere)
library(washb)
library(foreign)
library(SuperLearner)


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched/tr")
treatment<-read.csv("washb-kenya-tr.csv")

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew")
#track<-read.csv("washb-kenya-tracking.csv",stringsAsFactors = T)
#track<-read.dta("washb-kenya-tracking.dta")
diar<-read.csv("washb-kenya-diar.csv",stringsAsFactors = T)
#diar<-read.dta("washb-kenya-diar.dta")

#anthro<-read.csv("washb-kenya-anthro.csv",stringsAsFactors = T)
anthro<-read.dta("washb-kenya-anthro.dta")

enrol<-read.csv("washb-kenya-enrol.csv",stringsAsFactors = T)
#enrol<-read.dta("washb-kenya-enrol.dta")


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched")
track<-read.csv("msP_household_tracking_20161006.csv",stringsAsFactors = T)

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")





#################
#Diarrhea cleaning
#################

#Merge in variables needed from the enrolment dataset.
enroltomerge<-subset(enrol, select=c('childid','Ncomp','momage','momedu','momheight','dminwat','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','gascook','car','cow','goat','dog','chicken','roof','wall', 'floor','HHS' ))

dim(diar)

diar_enrol<-merge(diar, enroltomerge, by=c("childid"),all.x=T,all.y=F)
colnames(diar_enrol)

dim(diar_enrol)


#subset the diarrhea to children <36 mos at enrollment
#or target children
ad <-
  diar_enrol %>%
  subset(., dcohort==1) %>%
  subset(., !is.na(.$diar7d)) %>%
  subset(., tr=="Control")
dim(ad)




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




#################
#LAZ cleaning
#################
#merge in adjustment covariates
enroltomerge<-subset(enrol, select=c('childid','Ncomp','momage','momedu','momheight','dminwat','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','gascook','car','cow','goat','dog','chicken','roof','wall', 'floor','HHS'))

laz<-merge(anthro, enroltomerge, by=c("childid"),all.x=T,all.y=F)

laz<-subset(laz, childtype=="Study Child"|childtype=="Study Child Twin")

laz$tr <- factor(laz$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
laz$month <- factor(laz$month)
laz$floor <- factor(laz$floor)

laz <- laz[order(laz$block,laz$clusterid,laz$hhid,laz$childid),]

#Drop children missing laz
laz<-subset(laz, !is.na(laz))
# Drop children with extreme LAZ values
laz <- subset(laz,laz_x!=1)

#subset to year 2 only
laz <- subset(laz,studyyear==2)

#Subset to controls only
laz <- subset(laz,tr=="Control")
dim(laz)


#Create fracode staffid experience factor
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


#################
#Compound distance calculations
#################
spill<-
  track%>%
  subset(select=c("hhid","clusterid","GPS_lat","GPS_long"))%>%
  #mutate(X=GPS_long,Y=GPS_lat)%>%
  #rename(X=GPS_long,Y=GPS_lat)%>%
  subset(!is.na(GPS_lat)|!is.na(GPS_long))%>% #drop missing gps coordinates
  subset(GPS_lat>0) #Drop compounds in nairobi (South of the equator, so negative lattitude)

#Merge in treatment information into spillover df
head(spill)
spill<-merge(spill,treatment, by=c("clusterid"),all.x=T,all.y=F)
head(spill)

#order the data
spill<-spill[order(spill$hhid),]


#"+proj=utm +zone=36N ellps=WGS84"

#Convert gps coord to UTM
mean(spill$GPS_long)
mean(spill$GPS_lat)

data<-cbind(spill$GPS_long,spill$GPS_lat)
utm=project(as.matrix(data),"+proj=utm +north +zone=36 ellps=WGS84")

#utm<-project(cbind(spill$GPS_long,spill$GPS_lat),"+proj=utm +north +zone=36 ellps=WGS84")
#northing:
spill$utmN<-utm[,1]
#easting:
spill$utmE<-utm[,2]




# Move the origin to the bottom-left corner
# Leave a 1000 meters margin to avoid any zero related issues
spill$utmN = (spill$utmN - min(spill$utmN) + 1000)
spill$utmE = (spill$utmE - min(spill$utmE) + 1000)



#Calculate the distance matrix
dm_A<-dist(cbind(spill$utmN,spill$utmE), method = "euclidean")



#Convert to km. to compare with Jade
dm_A=as.matrix(dm_A)/1000
rownames(dm_A)<-colnames(dm_A)<-spill$hhid


#Save distance matrix
#save(dm_A, file="dm_Andrew.RData")

dm<-dm_A
max(dm[which(spill$hhid==2003011),])
mean(dm[which(spill$hhid==2003011),])




#Create vectors of positions of different intervention groups
vecW<-vecS<-vecH<-vecN<-vecC<-rep(NA,nrow(spill))
vecW<-ifelse(spill$tr=="Water"|spill$tr=="WSH"|spill$tr=="Nutrition + WSH",1,0)
vecS<-ifelse(spill$tr=="Sanitation"|spill$tr=="WSH"|spill$tr=="Nutrition + WSH",1,0)
vecH<-ifelse(spill$tr=="Handwashing"|spill$tr=="WSH"|spill$tr=="Nutrition + WSH",1,0)
vecN<-ifelse(spill$tr=="Nutrition"|spill$tr=="Nutrition + WSH",1,0)
vecC<-ifelse(spill$tr=="Control",1,0)



#subset rows to control and columns each intervention group
dmT<-dm[which(vecC==1),]
dmW<-dm[which(vecC==1),which(vecW==1)]
dmS<-dm[which(vecC==1),which(vecS==1)]
dmH<-dm[which(vecC==1),which(vecH==1)]
dmN<-dm[which(vecC==1),which(vecN==1)]



#comp2km=function(data){
#  out=data.frame(hhid=as.numeric(as.character(rownames(data))))
#  out$comp2km=apply(data,1,function(x) sum(ifelse(x<2,1,0)))
#  return(out)
#}

#test<-comp2km(dmW)


#total numbers in each row
rowtot<-apply(dmT, 1, function(x) sum(ifelse(x<2,1,0)))


#numerators in each intervention category
watnum<-apply(dmW, 1, function(x) sum(ifelse(x<2,1,0)))
sannum<-apply(dmS, 1, function(x) sum(ifelse(x<2,1,0)))
hygnum<-apply(dmH, 1, function(x) sum(ifelse(x<2,1,0)))
nutnum<-apply(dmN, 1, function(x) sum(ifelse(x<2,1,0)))

watprop<-watnum/rowtot
sanprop<-sannum/rowtot
hygprop<-hygnum/rowtot
nutprop<-nutnum/rowtot

mean(watnum)
mean(sannum)
mean(hygnum)
mean(nutnum)

mean(rowtot)

mean(watprop)
mean(sanprop)
mean(hygprop)
mean(nutprop)



#Subset spill dataset to rows of control only
spillC<-subset(spill, tr=="Control")

#create dataframe of control hhs and intervention proportioncs
spillprop<-data.frame(hhid=spillC$hhid,clusterid=spillC$clusterid,block=spillC$block,tr=spillC$tr,watprop=watprop,sanprop=sanprop,hygprop=hygprop,nutprop=nutprop,rowtot=rowtot, watnum=watnum,sannum=sannum,hygnum=hygnum,nutnum=nutnum)






#Calculate distribution of proportions
par(mfrow=c(2,2))
hist(spillprop$watprop, main="Prop Water Int. <2km")
hist(spillprop$sanprop, main="Prop Sanitation Int. <2km")
hist(spillprop$hygprop, main="Prop Handwashing Int. <2km")
hist(spillprop$nutprop, main="Prop Nutrition Int. <2km")




#Merge in outcome and covariate data
  #Drop columns present in both datasets prior to merge
spillmerge<-subset(spillprop, select= -c(clusterid,block,tr))


#merge
diarspill<-merge(spillmerge,ad,by=c("hhid"),all.x=F,all.y=F)
lazspill<-merge(spillmerge,laz,by=c("hhid"),all.x=F,all.y=F)
dim(diarspill)
dim(lazspill)


#Change to calculate quartiles after merge
DquantW<-quantile(diarspill$watprop, prob = seq(0, 1, length = 11), type = 5)
DquantS<-quantile(diarspill$sanprop, prob = seq(0, 1, length = 11), type = 5)
DquantH<-quantile(diarspill$hygprop, prob = seq(0, 1, length = 11), type = 5)
DquantN<-quantile(diarspill$nutprop, prob = seq(0, 1, length = 11), type = 5)

diarspill$distcatW<-diarspill$distcatS<-diarspill$distcatH<-diarspill$distcatN<-"mid2080"

diarspill[which(diarspill$watprop<=DquantW[3]),]$distcatW<-"low20"
diarspill[which(diarspill$watprop>=DquantW[9]),]$distcatW<-"up80"
diarspill[which(diarspill$sanprop<=DquantS[3]),]$distcatS<-"low20"
diarspill[which(diarspill$sanprop>=DquantS[9]),]$distcatS<-"up80"
diarspill[which(diarspill$hygprop<=DquantH[3]),]$distcatH<-"low20"
diarspill[which(diarspill$hygprop>=DquantH[9]),]$distcatH<-"up80"
diarspill[which(diarspill$nutprop<=DquantN[3]),]$distcatN<-"low20"
diarspill[which(diarspill$nutprop>=DquantN[9]),]$distcatN<-"up80"

AquantW<-quantile(lazspill$watprop, prob = seq(0, 1, length = 11), type = 5)
AquantS<-quantile(lazspill$sanprop, prob = seq(0, 1, length = 11), type = 5)
AquantH<-quantile(lazspill$hygprop, prob = seq(0, 1, length = 11), type = 5)
AquantN<-quantile(lazspill$nutprop, prob = seq(0, 1, length = 11), type = 5)


lazspill$distcatW<-lazspill$distcatS<-lazspill$distcatH<-lazspill$distcatN<-"mid2080"

lazspill[which(lazspill$watprop<=AquantW[3]),]$distcatW<-"low20"
lazspill[which(lazspill$watprop>=AquantW[9]),]$distcatW<-"up80"
lazspill[which(lazspill$sanprop<=AquantS[3]),]$distcatS<-"low20"
lazspill[which(lazspill$sanprop>=AquantS[9]),]$distcatS<-"up80"
lazspill[which(lazspill$hygprop<=AquantH[3]),]$distcatH<-"low20"
lazspill[which(lazspill$hygprop>=AquantH[9]),]$distcatH<-"up80"
lazspill[which(lazspill$nutprop<=AquantN[3]),]$distcatN<-"low20"
lazspill[which(lazspill$nutprop>=AquantN[9]),]$distcatN<-"up80"

#save cutoffs for permute function
Ddistcut<-cbind(DquantW,DquantS,DquantH,DquantN)[c(3,9),]
Adistcut<-cbind(AquantW,AquantS,AquantH,AquantN)[c(3,9),]


#Save proportions and distcats for adjusted analysis
SL_distd<-data.frame(hhid=diarspill$hhid, wp=diarspill$watprop, sp=diarspill$sanprop, hp=diarspill$hygprop, np=diarspill$nutprop
                     , distcatW=diarspill$distcatW, distcatS=diarspill$distcatS, distcatH=diarspill$distcatH, distcatN=diarspill$distcatN)

SL_dista<-data.frame(hhid=lazspill$hhid, wp=lazspill$watprop, sp=lazspill$sanprop, hp=lazspill$hygprop, np=lazspill$nutprop
                     , distcatW=lazspill$distcatW, distcatS=lazspill$distcatS, distcatH=lazspill$distcatH, distcatN=lazspill$distcatN)



#################
#Calculate Difference
#################
#Create empty object to hold spillover test results
perm.haz.dist_A<-perm.diar.dist_A<-matrix(NA,nrow=4,ncol=6)
colnames(perm.haz.dist_A)<-colnames(perm.diar.dist_A)<-c("N20","N80","Mean20","Mean80","Diff","Pvalue")
rownames(perm.diar.dist_A)<-c("Wdiar","Sdiar","Hdiar","Ndiar")
rownames(perm.haz.dist_A)<-c("Wlaz","Slaz","Hlaz","Nlaz")


#Calc n's and means
Dw <- group_by(diarspill, distcatW)
Dw <- summarise(Dw,n=n(), test=mean(diar7d))

Ds <- group_by(diarspill, distcatS)
Ds <- summarise(Ds,n=n(), test=mean(diar7d))

Dh <- group_by(diarspill, distcatH)
Dh <- summarise(Dh,n=n(), test=mean(diar7d))

Dn <- group_by(diarspill, distcatN)
Dn <- summarise(Dn,n=n(), test=mean(diar7d))

perm.diar.dist_A[1,1:2]<-t(Dw[c(1,3),2])
perm.diar.dist_A[1,3:4]<-t(Dw[c(1,3),3])

perm.diar.dist_A[2,1:2]<-t(Ds[c(1,3),2])
perm.diar.dist_A[2,3:4]<-t(Ds[c(1,3),3])

perm.diar.dist_A[3,1:2]<-t(Dh[c(1,3),2])
perm.diar.dist_A[3,3:4]<-t(Dh[c(1,3),3])

perm.diar.dist_A[4,1:2]<-t(Dn[c(1,3),2])
perm.diar.dist_A[4,3:4]<-t(Dn[c(1,3),3])




Aw <- group_by(lazspill, distcatW)
Aw <- summarise(Aw,n=n(), test=mean(laz))

As <- group_by(lazspill, distcatS)
As <- summarise(As,n=n(), test=mean(laz))

Ah <- group_by(lazspill, distcatH)
Ah <- summarise(Ah,n=n(), test=mean(laz))

An <- group_by(lazspill, distcatN)
An <- summarise(An,n=n(), test=mean(laz))

perm.haz.dist_A[1,1:2]<-t(Aw[c(1,3),2])
perm.haz.dist_A[1,3:4]<-t(Aw[c(1,3),3])

perm.haz.dist_A[2,1:2]<-t(As[c(1,3),2])
perm.haz.dist_A[2,3:4]<-t(As[c(1,3),3])

perm.haz.dist_A[3,1:2]<-t(Ah[c(1,3),2])
perm.haz.dist_A[3,3:4]<-t(Ah[c(1,3),3])

perm.haz.dist_A[4,1:2]<-t(An[c(1,3),2])
perm.haz.dist_A[4,3:4]<-t(An[c(1,3),3])



#Calculate difference between <20th and >80th percentile means
perm.diar.dist_A[,5]<-perm.diar.dist_A[,4]-perm.diar.dist_A[,3]
perm.haz.dist_A[,5]<-perm.haz.dist_A[,4]-perm.haz.dist_A[,3]


perm.diar.dist_A
perm.haz.dist_A

###########
#run permutation tests
###########

#create function
permfun<-function(pd,distcut,perm.dist,rep,distcatind){
  permdiff<-rep(NA,rep)
  permgreater<-rep(0,rep)
for(i in 1:rep){
  perm<-(pd$Y)
  #perm$sort<-rnorm(nrow(perm),mean=0,sd=1) #permute outcome
  #perm<-perm[order(perm[,2]),]
  perm<-perm[order(rnorm(length(perm),mean=0,sd=1))]
  perm<-cbind(perm,pd[,distcatind+1])

permdiff[i]<-mean(perm[which(perm[,2]<=distcut[1,distcatind]),1])-mean(perm[which(perm[,2]>=distcut[2,distcatind]),1])
  }

#calculate p-value
  permgreater[which(abs(permdiff)> abs( perm.dist[distcatind,5]))]<-1
  pval <- sum(permgreater)/rep

return(pval)
}

qdiff_ben <- function(y,x,quantiles=c(0.2,0.8)){
  qvals <- quantile(x,probs=quantiles,na.rm=TRUE)
  dx <- factor(rep(NA,length(x)),levels=c("group0","group1"))
  dx[x<=qvals[1]] <- "group0"
  dx[x>=qvals[2]] <- "group1"
  ns <- table(dx)
  mu0 <- mean(y[dx=="group0"],na.rm=TRUE)
  mu1 <- mean(y[dx=="group1"],na.rm=TRUE)
  diff <- mu1-mu0
  return(list(n0=ns[1],n1=ns[2],mu0=mu0,mu1=mu1,diff=diff))
}
permute_qdiff_ben <- function(y,x,quantiles=c(0.2,0.8),nreps=1000) {

  # compute the observed test statistic
  Tobs <- qdiff(y,x,quantiles)$diff

  # get the test statistic's null distribution
  # through permutation
  nobs <- length(y)
  Ho <- rep(NA,nreps)
  for(i in 1:nreps) {
    newx <- x[order(runif(n=nobs))]
    Ho[i] <- qdiff(y,newx,quantiles)$diff
  }

  # compute 2-sided p-value
  pvalue <- sum(abs(Ho) >= abs(Tobs)) / length(Ho)

  # print results
  cat("\n--------------------------------\n")
  cat("Observed test statistic:",Tobs)
  cat("\nPermutation-test two-sided p-value\n(",nreps,"replicates )\nthat this differs from 0 is:",round(pvalue,4))
  cat("\n--------------------------------\n")

  # return the p-value, observed test statistic, and the null distribution
  return(list(pvalue=pvalue,Tobs=Tobs,Ho=Ho))
}
qdiff <- function(y,x,quantiles=c(0.2,0.8)){
  qvals <- quantile(x,probs=quantiles,na.rm=TRUE, type=5)
  dx <- factor(rep(NA,length(x)),levels=c("group0","group1"))
  dx[x<=qvals[1]] <- "group0"
  dx[x>=qvals[2]] <- "group1"
  mu0 <- mean(y[dx=="group0"],na.rm=TRUE)
  mu1 <- mean(y[dx=="group1"],na.rm=TRUE)
  diff <- mu1-mu0
  return(diff)
}
permute_qdiff_old <- function(y,x,Tobs,quantiles=c(0.2,0.8),nreps=1000) {

  # compute the observed test statistic
  #Tobs <- diff

  # get the test statistic's null distribution
  # through permutation
  nobs <- length(y)
  Ho <- rep(NA,nreps)
  for(i in 1:nreps) {
    newy <- y[order(rnorm(n=nobs,mean=0,sd=1))]
    Ho[i] <- qdiff(newy,x,quantiles)

    #permdiff[i]<-mean(perm[which(perm[,2]<=distcut[1,distcatind]),1])-mean(perm[which(perm[,2]>=distcut[2,distcatind]),1])
  }

  # compute 2-sided p-value
  pvalue <- sum(abs(Ho) > abs(Tobs)) / length(Ho)

  # print results
  cat("\n--------------------------------\n")
  cat("Observed test statistic:",Tobs)
  cat("\nPermutation-test two-sided p-value\n(",nreps,"replicates )\nthat this differs from 0 is:",round(pvalue,4))
  cat("\n--------------------------------\n")

  # return the p-value, observed test statistic, and the null distribution
  #return(list(pvalue=pvalue,Tobs=Tobs,Ho=Ho))
  return(list(pvalue=pvalue,Tobs=Tobs,Ho=Ho))
}

permute_qdiff <- function(y,x,Tobs,quantiles=c(0.2,0.8),nreps=1000) {
  permgreater<-rep(0,nreps)
  qvals <- quantile(x,probs=quantiles,na.rm=TRUE, type=5)
  dx <- factor(rep(NA,length(x)),levels=c("group0","group1"))
  dx[x<=qvals[1]] <- "group0"
  dx[x>=qvals[2]] <- "group1"

  # get the test statistic's null distribution
  # through permutation
  nobs <- length(y)
  Ho <- rep(NA,nreps)
  for(i in 1:nreps) {
    newy <- y[order(rnorm(n=nobs,mean=0,sd=1))]
    Ho[i] <- mean(newy[dx=="group1"],na.rm=TRUE)-mean(newy[dx=="group0"],na.rm=TRUE)
  }

  # compute 2-sided p-value
  permgreater[which(abs(Ho)> abs(Tobs))]<-1
  pvalue <- sum(permgreater)/nreps

  # print results
  cat("\n--------------------------------\n")
  cat("Observed test statistic:",Tobs)
  cat("\nPermutation-test two-sided p-value\n(",nreps,"replicates )\nthat this differs from 0 is:",round(pvalue,4))
  cat("\n--------------------------------\n")

  # return the p-value, observed test statistic, and the null distribution
  #return(list(pvalue=pvalue,Tobs=Tobs,Ho=Ho))
  return(list(pvalue=pvalue,Tobs=Tobs,Ho=Ho))
}


#y=pdDiarAdj$Y
#x=pdDiarAdj$hp
#Tobs=perm.diar.distSL[3,5]
#quantiles=c(0.2,0.8)
#nreps=100000
#perm.diar.distSL[3,6]<-permute_qdiff(y=pdDiarAdj$Y,x=pdDiarAdj$hp,Tobs=perm.diar.distSL[3,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue



#diarrhea permute
pdDiar <- data.frame(Y=diarspill$diar7d, wp=diarspill$watprop, sp=diarspill$sanprop, hp=diarspill$hygprop, np=diarspill$nutprop, hhid=diarspill$hhid)
pdDiar<-pdDiar[order(pdDiar$hhid),]

    set.seed(67890)
perm.diar.dist_A[1,6]<-permute_qdiff(y=pdDiar$Y,x=pdDiar$wp,Tobs=perm.diar.dist_A[1,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue
#perm.diar.dist_A[1,6]<-permfun(pdDiar,Ddistcut,perm.diar.dist_A,10000,1)

    set.seed(67890)
perm.diar.dist_A[2,6]<-permute_qdiff(y=pdDiar$Y,x=pdDiar$sp,Tobs=perm.diar.dist_A[2,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue
#perm.diar.dist_A[2,6]<-permfun(pdDiar,Ddistcut,perm.diar.dist_A,10000,2)

    set.seed(67890)
perm.diar.dist_A[3,6]<-permute_qdiff(y=pdDiar$Y,x=pdDiar$hp,Tobs=perm.diar.dist_A[3,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue
#perm.diar.dist_A[3,6]<-permfun(pdDiar,Ddistcut,perm.diar.dist_A,10000,3)

    set.seed(67890)
perm.diar.dist_A[4,6]<-permute_qdiff(y=pdDiar$Y,x=pdDiar$np,Tobs=perm.diar.dist_A[4,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue
#perm.diar.dist_A[4,6]<-permfun(pdDiar,Ddistcut,perm.diar.dist_A,10000,4)


#laz permute
pdLAZ <- data.frame(Y=lazspill$laz, wp=lazspill$watprop, sp=lazspill$sanprop, hp=lazspill$hygprop, np=lazspill$nutprop, hhid=lazspill$hhid)
lazspill<-lazspill[order(lazspill$hhid),]

    set.seed(67890)
perm.haz.dist_A[1,6]<-permute_qdiff(y=pdLAZ$Y,x=pdLAZ$wp,Tobs=perm.haz.dist_A[1,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue
#perm.haz.dist_A[1,6]<-permfun(pdLAZ,Adistcut,perm.haz.dist_A,10000,1)
    set.seed(67890)
perm.haz.dist_A[2,6]<-permute_qdiff(y=pdLAZ$Y,x=pdLAZ$sp,Tobs=perm.haz.dist_A[2,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue
#perm.haz.dist_A[2,6]<-permfun(pdLAZ,Adistcut,perm.haz.dist_A,10000,2)
    set.seed(67890)
perm.haz.dist_A[3,6]<-permute_qdiff(y=pdLAZ$Y,x=pdLAZ$hp,Tobs=perm.haz.dist_A[3,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue
#perm.haz.dist_A[3,6]<-permfun(pdLAZ,Adistcut,perm.haz.dist_A,10000,3)
    set.seed(67890)
perm.haz.dist_A[4,6]<-permute_qdiff(y=pdLAZ$Y,x=pdLAZ$np,Tobs=perm.haz.dist_A[4,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue
#perm.haz.dist_A[4,6]<-permfun(pdLAZ,Adistcut,perm.haz.dist_A,10000,4)


#
#perm.haz.dist_A
#    set.seed(67890)
#test<-permute_qdiff(y=pdLAZ$Y,x=pdLAZ$sp,Tobs=perm.haz.dist_A[2,5],quantiles=c(0.2,0.8),nreps=100000)

#test$Tobs
#test$pvalue

#######################
#Adjusted Permutation Test
#######################

#Adjustment variables to include
#month included within diar and anthro datasets.
diarWvars<-c('sex','aged','month','momage','momedu','momheight','Ncomp','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','roof','floor','cow','goat','dog','chicken','dminwat','HHS','fracode')
 lazWvars<-c('sex','aged','month','momage','momedu','momheight','Ncomp','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','roof','floor','cow','goat','dog','chicken','dminwat','HHS','fracode')



#subset W adjustment set
diarW<- subset(ad, select=diarWvars)
Wanthro<- subset(laz, select=lazWvars)


#check that all the fsactor variables are set
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
diarW$fracode<-as.factor(diarW$fracode)
ad$block<-as.factor(ad$block)

Wanthro$month<-as.factor(Wanthro$month)
Wanthro$HHS<-as.factor(Wanthro$HHS)
Wanthro$electricity<-as.factor(Wanthro$electricity)
Wanthro$radio<-as.factor(Wanthro$radio)
Wanthro$television<-as.factor(Wanthro$television)
Wanthro$mobile<-as.factor(Wanthro$mobile)
Wanthro$clock<-as.factor(Wanthro$clock)
Wanthro$bicycle<-as.factor(Wanthro$bicycle)
Wanthro$motorcycle<-as.factor(Wanthro$motorcycle)
Wanthro$stove<-as.factor(Wanthro$stove)
Wanthro$roof<-as.factor(Wanthro$roof)
Wanthro$floor<-as.factor(Wanthro$floor)
Wanthro$fracode<-as.factor(Wanthro$fracode)
laz$block<-as.factor(laz$block)



#set up
preSLd <- data.frame(childid=ad$childid,hhid=ad$hhid,svy=ad$studyyear,id=ad$clusterid,block=ad$block,tr=ad$tr,Y=ad$diar7d,diarW)
preSLa <- data.frame(childid=laz$childid,hhid=laz$hhid,svy=laz$studyyear,id=laz$clusterid,block=laz$block,tr=laz$tr,Y=laz$laz,Wanthro)
dim(preSLd)
dim(preSLa)


  # pre-screen the covariates for those associated with the outcome (LR test P<0.2)
  colnames(preSLd)
  Wscreend <- washb_prescreen(Y=preSLd$Y,Ws=preSLd[,8:ncol(preSLd)],family="binomial")
  Wselectd <- subset(preSLd,select=Wscreend)

  colnames(preSLa)
  Wscreena <- washb_prescreen(Y=preSLa$Y,Ws=preSLa[,8:ncol(preSLa)],family="gaussian")
  Wselecta <- subset(preSLa,select=Wscreena)


  SLd <- data.frame(childid=ad$childid,hhid=ad$hhid,svy=ad$studyyear,id=ad$clusterid,block=ad$block,
                    tr=ad$tr,Y=ad$diar7d,Wselectd)
  SLa <- data.frame(childid=laz$childid,hhid=laz$hhid,svy=laz$studyyear,id=laz$clusterid,block=laz$block,
                    tr=laz$tr,Y=laz$laz,Wselecta)

  # restrict to complete cases
  SLd <- SLd[complete.cases(SLd),]
  SLa <- SLa[complete.cases(SLa),]

  #Sort data
  SLd <- SLd[order(SLd$block,SLd$id,SLd$hhid,SLd$childid,SLd$svy),]
  SLa <- SLa[order(SLa$block,SLa$id,SLa$hhid,SLa$childid,SLa$svy),]

  Wselectd<-SLd[,c(8:ncol(SLd))]
  Wselectd <- design_matrix(Wselectd)
  Wselecta<-SLa[,c(8:ncol(SLa))]
  Wselecta <- design_matrix(Wselecta)

  dim(SLd)
  dim(SLa)
  dim(Wselectd)
  dim(Wselecta)

  set.seed(67890)
  SLfitd <- SuperLearner(Y=SLd$Y,X=Wselectd,id=SLd$id,
                         family="binomial",
                         SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
  )
  SLfitd
  SLd$pY <- as.vector(predict(SLfitd)$pred)
  SLd$r <- SLd$Y-SLd$pY

  summary(SLd$r)

  set.seed(67890)
  SLfita <- SuperLearner(Y=SLa$Y,X=Wselecta,id=SLa$id,
                         family="gaussian",
                         SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
  )
  SLfita
  SLa$pY <- as.vector(predict(SLfita)$pred)
  SLa$r <- SLa$Y-SLa$pY

  summary(SLa$r)


dim(SL_distd)
dim(SL_dista)
dim(SLd)
dim(SLa)

#Merge in the spillover data:
diarspill<-merge(SLd,spillprop ,by=c("hhid"),all.x=F,all.y=F)
lazspill<-merge(SLa, spillprop, by=c("hhid"),all.x=F,all.y=F)
dim(diarspill)
dim(lazspill)


#drop missing in residual
diarspill<-subset(diarspill)
  summary(diarspill$r)
  summary(lazspill$r)


  mean(diarspill$hygprop)
    mean(lazspill$hygprop)



#Change to calculate quartiles after merge
DquantW<-quantile(diarspill$watprop, prob = seq(0, 1, length = 11), type = 5)
DquantS<-quantile(diarspill$sanprop, prob = seq(0, 1, length = 11), type = 5)
DquantH<-quantile(diarspill$hygprop, prob = seq(0, 1, length = 11), type = 5)
DquantN<-quantile(diarspill$nutprop, prob = seq(0, 1, length = 11), type = 5)

diarspill$distcatW<-diarspill$distcatS<-diarspill$distcatH<-diarspill$distcatN<-"mid2080"

diarspill[which(diarspill$watprop<=DquantW[3]),]$distcatW<-"low20"
diarspill[which(diarspill$watprop>=DquantW[9]),]$distcatW<-"up80"
diarspill[which(diarspill$sanprop<=DquantS[3]),]$distcatS<-"low20"
diarspill[which(diarspill$sanprop>=DquantS[9]),]$distcatS<-"up80"
diarspill[which(diarspill$hygprop<=DquantH[3]),]$distcatH<-"low20"
diarspill[which(diarspill$hygprop>=DquantH[9]),]$distcatH<-"up80"
diarspill[which(diarspill$nutprop<=DquantN[3]),]$distcatN<-"low20"
diarspill[which(diarspill$nutprop>=DquantN[9]),]$distcatN<-"up80"

AquantW<-quantile(lazspill$watprop, prob = seq(0, 1, length = 11), type = 5)
AquantS<-quantile(lazspill$sanprop, prob = seq(0, 1, length = 11), type = 5)
AquantH<-quantile(lazspill$hygprop, prob = seq(0, 1, length = 11), type = 5)
AquantN<-quantile(lazspill$nutprop, prob = seq(0, 1, length = 11), type = 5)


lazspill$distcatW<-lazspill$distcatS<-lazspill$distcatH<-lazspill$distcatN<-"mid2080"

lazspill[which(lazspill$watprop<=AquantW[3]),]$distcatW<-"low20"
lazspill[which(lazspill$watprop>=AquantW[9]),]$distcatW<-"up80"
lazspill[which(lazspill$sanprop<=AquantS[3]),]$distcatS<-"low20"
lazspill[which(lazspill$sanprop>=AquantS[9]),]$distcatS<-"up80"
lazspill[which(lazspill$hygprop<=AquantH[3]),]$distcatH<-"low20"
lazspill[which(lazspill$hygprop>=AquantH[9]),]$distcatH<-"up80"
lazspill[which(lazspill$nutprop<=AquantN[3]),]$distcatN<-"low20"
lazspill[which(lazspill$nutprop>=AquantN[9]),]$distcatN<-"up80"

#save cutoffs for permute function
Ddistcut<-cbind(DquantW,DquantS,DquantH,DquantN)[c(3,9),]
Adistcut<-cbind(AquantW,AquantS,AquantH,AquantN)[c(3,9),]



#calculate Ns, means, and diff of residuals from SL model, rather than outcome
  #Create empty object to hold spillover test results
perm.haz.distSL<-perm.diar.distSL<-matrix(NA,nrow=4,ncol=6)
colnames(perm.haz.distSL)<-colnames(perm.diar.distSL)<-c("N20","N80","Resid.Mean20","Resid.Mean80","Diff","Pvalue")
rownames(perm.diar.distSL)<-c("Wdiar","Sdiar","Hdiar","Ndiar")
rownames(perm.haz.distSL)<-c("Wlaz","Slaz","Hlaz","Nlaz")

#Calc n's and means
Dw <- group_by(diarspill, distcatW)
Dw <- summarise(Dw,n=n(), test=mean(r))

Ds <- group_by(diarspill, distcatS)
Ds <- summarise(Ds,n=n(), test=mean(r))

Dh <- group_by(diarspill, distcatH)
Dh <- summarise(Dh,n=n(), test=mean(r))

Dn <- group_by(diarspill, distcatN)
Dn <- summarise(Dn,n=n(), test=mean(r))

perm.diar.distSL[1,1:2]<-t(Dw[c(1,3),2])
perm.diar.distSL[1,3:4]<-t(Dw[c(1,3),3])

perm.diar.distSL[2,1:2]<-t(Ds[c(1,3),2])
perm.diar.distSL[2,3:4]<-t(Ds[c(1,3),3])

perm.diar.distSL[3,1:2]<-t(Dh[c(1,3),2])
perm.diar.distSL[3,3:4]<-t(Dh[c(1,3),3])

perm.diar.distSL[4,1:2]<-t(Dn[c(1,3),2])
perm.diar.distSL[4,3:4]<-t(Dn[c(1,3),3])




Aw <- group_by(lazspill, distcatW)
Aw <- summarise(Aw,n=n(), test=mean(r))

As <- group_by(lazspill, distcatS)
As <- summarise(As,n=n(), test=mean(r))

Ah <- group_by(lazspill, distcatH)
Ah <- summarise(Ah,n=n(), test=mean(r))

An <- group_by(lazspill, distcatN)
An <- summarise(An,n=n(), test=mean(r))

perm.haz.distSL[1,1:2]<-t(Aw[c(1,3),2])
perm.haz.distSL[1,3:4]<-t(Aw[c(1,3),3])

perm.haz.distSL[2,1:2]<-t(As[c(1,3),2])
perm.haz.distSL[2,3:4]<-t(As[c(1,3),3])

perm.haz.distSL[3,1:2]<-t(Ah[c(1,3),2])
perm.haz.distSL[3,3:4]<-t(Ah[c(1,3),3])

perm.haz.distSL[4,1:2]<-t(An[c(1,3),2])
perm.haz.distSL[4,3:4]<-t(An[c(1,3),3])



#Calculate difference between <20th and >80th percentile means
perm.diar.distSL[,5]<-perm.diar.distSL[,4]-perm.diar.distSL[,3]
perm.haz.distSL[,5]<-perm.haz.distSL[,4]-perm.haz.distSL[,3]


perm.diar.distSL
perm.haz.distSL





#diarrhea permute
pdDiarAdj <- data.frame(Y=diarspill$r, wp=diarspill$watprop, sp=diarspill$sanprop, hp=diarspill$hygprop, np=diarspill$nutprop, hhid=diarspill$hhid)


pdDiarAdj<-pdDiarAdj[order(pdDiarAdj$hhid),]
set.seed(67890)
#perm.diar.distSL[1,6]<-permfun(pdDiarAdj,Ddistcut,perm.diar.distSL,10000,1)
perm.diar.distSL[1,6]<-permute_qdiff(y=pdDiarAdj$Y,x=pdDiarAdj$wp,Tobs=perm.diar.distSL[1,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue

set.seed(67890)
perm.diar.distSL[2,6]<-permute_qdiff(y=pdDiarAdj$Y,x=pdDiarAdj$sp,Tobs=perm.diar.distSL[2,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue
#perm.diar.distSL[2,6]<-permfun(pdDiarAdj,Ddistcut,perm.diar.distSL,10000,2)

pdDiarAdj<-pdDiarAdj[order(pdDiarAdj$hhid),]
set.seed(67890)
#perm.diar.distSL[3,6]<-permfun(pdDiarAdj,Ddistcut,perm.diar.distSL,10000,3)
perm.diar.distSL[3,6]<-permute_qdiff(y=pdDiarAdj$Y,x=pdDiarAdj$hp,Tobs=perm.diar.distSL[3,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue

set.seed(67890)
#perm.diar.distSL[4,6]<-permfun(pdDiarAdj,Ddistcut,perm.diar.distSL,10000,4)
perm.diar.distSL[4,6]<-permute_qdiff(y=pdDiarAdj$Y,x=pdDiarAdj$np,Tobs=perm.diar.distSL[4,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue





#laz permute
pdLazAdj <- data.frame(Y=lazspill$r, wp=lazspill$watprop, sp=lazspill$sanprop, hp=lazspill$hygprop, np=lazspill$nutprop, hhid=lazspill$hhid)


pdLazAdj<-pdLazAdj[order(pdLazAdj$hhid),]
set.seed(67890)
#perm.haz.distSL[1,6]<-permfun(pdLazAdj,Adistcut,perm.haz.distSL,10000,1)
perm.haz.distSL[1,6]<-permute_qdiff(y=pdLazAdj$Y,x=pdLazAdj$wp,Tobs=perm.haz.distSL[1,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue

pdLazAdj<-pdLazAdj[order(pdLazAdj$hhid),]
set.seed(67890)
#perm.haz.distSL[2,6]<-permfun(pdLazAdj,Adistcut,perm.haz.distSL,10000,2)
perm.haz.distSL[2,6]<-permute_qdiff(y=pdLazAdj$Y,x=pdLazAdj$sp,Tobs=perm.haz.distSL[2,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue

pdLazAdj<-pdLazAdj[order(pdLazAdj$hhid),]
set.seed(67890)
#perm.haz.distSL[3,6]<-permfun(pdLazAdj,Adistcut,perm.haz.distSL,10000,3)
perm.haz.distSL[3,6]<-permute_qdiff(y=pdLazAdj$Y,x=pdLazAdj$hp,Tobs=perm.haz.distSL[3,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue

set.seed(67890)
#perm.haz.distSL[4,6]<-permfun(pdLazAdj,Adistcut,perm.haz.distSL,10000,4)
perm.haz.distSL[4,6]<-permute_qdiff(y=pdLazAdj$Y,x=pdLazAdj$np,Tobs=perm.haz.distSL[4,5],quantiles=c(0.2,0.8),nreps=100000)$pvalue






#Save the results
setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
perm.diarr.dist_A<-perm.diar.dist_A #rename to match jade
perm.haz.dist.adj_A<-perm.haz.distSL
perm.diarr.dist.adj_A<-perm.diar.distSL
save(perm.diarr.dist_A,perm.haz.dist_A,perm.diarr.dist.adj_A,perm.haz.dist.adj_A, file="spillover_Andrew.Rdata")










