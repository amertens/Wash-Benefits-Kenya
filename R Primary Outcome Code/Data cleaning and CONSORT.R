
#---------------------------------------
# Consort.R
#
# andrew mertens (amertens@berkeley.edu)
#
# initial dataset setup for diar and anthro outcome analysis in
#Wash Benefits Kenya, plus creation of CONSORT objects
#---------------------------------------


###Load in data
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched/tr")
treatment<-read.csv("washb-kenya-tr.csv")

setwd("C:/Users/andre/Dropbox/Kenya Primary Analysis/Data-selected/clean")
enrol<-read.dta("washb-kenya-enrol.dta")
diar<-read.dta("washb-kenya-diar.dta")
anthro<-read.dta("washb-kenya-anthro.dta")
HHtracking<-read.dta("washb-kenya-tracking.dta")

arms<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N","PassiveControl")




#CONSORT, Balance, Uptake
####Compound Ns for enrollment by arm, year 1, year 2


Nbl <-
  HHtracking %>%
  group_by( tr) %>%
  summarise(Enrollment=n())
Nbl

Nml <-
  HHtracking %>%
  group_by( indata_ml, tr) %>%
  summarise(year1N=n())

Nel <-
  HHtracking %>%
  group_by( indata_el, tr) %>%
  summarise(year2N=n())


compound_tracking<-t(cbind(Nbl[,2], Nml[1:8,3], Nel[1:8,3]))
colnames(compound_tracking)<-arms


compound_tracking<-compound_tracking[,order(c(1,3:8,2))]


compound_tracking


#Target children Ns with outcome data by arm for year 1, year 2
tc_haz<-
anthro %>%
  subset(., !is.na(laz)) %>%
  subset(., laz_x!=1) %>%
  group_by(studyyear, tr) %>%
  filter(childtype=="Study Child"|childtype=="Study Child Twin") %>%
  summarize(n=n())

tc_haz<-as.data.frame(tc_haz)


t1_tc_haz<-as.data.frame(tc_haz[1:8,3])
t2_tc_haz<-as.data.frame(tc_haz[9:16,3])
rownames(t1_tc_haz)<-rownames(t2_tc_haz)<-arms
colnames(t1_tc_haz)<-colnames(t2_tc_haz)<-"N"

tc_diar<-
diar %>%
  subset(., !is.na(diar7d)) %>%
  subset(., dcohort==1) %>%
  group_by(studyyear, tr) %>%
  filter(childtype=="Study Child"|childtype=="Study Child Twin") %>%
  summarize(n=n())


t1_tc_diar<-as.data.frame(tc_diar[1:8,3])
t2_tc_diar<-as.data.frame(tc_diar[9:16,3])
rownames(t1_tc_diar)<-rownames(t2_tc_diar)<-arms
colnames(t1_tc_diar)<-colnames(t2_tc_diar)<-"N"


#Target Child Ns with either outcome:
dim(diar)
dim(anthro)
#comb<-merge(subset(diar, select=c("childid","tr","childtype","studyyear","diar7d","dcohort")), subset(anthro, select=c("childid","childtype","tr","studyyear","laz","laz_x")), by=c("childid","tr","studyyear","childtype"), all.x=T, all.y=T)
diarmerge<-diar[diar$studyyear!=0, c("childid","tr","studyyear","diar7d")]
anthromerge<-anthro[anthro$studyyear!=0, c("childid","tr","studyyear","laz")]

comb<-merge(diarmerge, anthromerge, by=c("childid","tr","studyyear") , all.x=T, all.y=T)
dim(comb)

table(comb$tr,comb$studyyear) 
combsub<-
  comb %>% 
  #subset(studyyear!=0 & (!is.na(diar7d) & dcohort==1 | !is.na(laz) & laz_x!=1)) %>% 
  subset((!is.na(diar7d) | !is.na(laz) )) #%>% 
  #filter(childtype=="Study Child"|childtype=="Study Child Twin")

#y1<-combsub[combsub$studyyear==1,]
#y2<-combsub[combsub$studyyear==2,]
y1<-comb[combsub$studyyear==1,]
y2<-comb[combsub$studyyear==2,]

y1$outcome=ifelse(!is.na(y1$diar7d) | !is.na(y1$laz),1,0)
y2$outcome=ifelse(!is.na(y2$diar7d) | !is.na(y2$laz),1,0)

y1outcome=table(y1$outcome,y1$tr)[2,]
y2outcome=table(y2$outcome,y2$tr)[2,]
y1outcome
y2outcome


dim(combsub)
table(combsub$tr,combsub$studyyear) 



setwd("C:/Users/andre/Dropbox/Kenya Primary Analysis/Data-selected/clean")
d<-read.dta("washb-kenya-diar.dta")
a<-read.dta("washb-kenya-anthro.dta")
dim(d)
dim(a)

md.data=d[d$studyyear==1,c("childid","tr","diar7d")]
ed.data=d[d$studyyear==2,c("childid","tr","diar7d")]
ma.data=a[a$studyyear==1,c("childid","tr","laz")]
ea.data=a[a$studyyear==2,c("childid","tr","laz")]

y1=merge(md.data,ma.data,by=c("childid","tr"), all.x=T, all.y=T)
y2=merge(ed.data,ea.data,by=c("childid","tr"), all.x=T, all.y=T)
dim(y1)
dim(y2)

y1$outcome=ifelse(!is.na(y1$diar7d) | !is.na(y1$laz),1,0)
y2$outcome=ifelse(!is.na(y2$diar7d) | !is.na(y2$laz),1,0)

t1_outcome=table(y1$outcome,y1$tr)[2,]
t2_outcome=table(y2$outcome,y2$tr)[2,]
t1_outcome
t2_outcome


####Target children Ns and # with missing outcome data by arm for year 1, year 2

targetchild<-
  anthro %>%
  filter(childtype=="Study Child"|childtype=="Study Child Twin") %>%
  subset(., select=c("tr", "studyyear", "laz"))

targetchild$miss<-0
targetchild[which(is.na(targetchild$laz)),"miss"]<-1
N.tchild <-
  targetchild %>%
  group_by(miss, studyyear, tr) %>%
  summarise(Nbl=n())

tchild_tracking<-t(cbind(N.tchild[1:8,4], N.tchild[9:16,4], N.tchild[17:24,4], N.tchild[25:32,4]))
colnames(tchild_tracking)<-arms
rownames(tchild_tracking)<-c("N Midline","N Endline","Missing Midline","Missing Endline")
tchild_tracking<-tchild_tracking[,order(c(1,3:8,2))]






####Compounds that dropped out Ns, by arm and reason for dropout

ml.drop<-Nbl[,2]-Nml[9:16,3]
el.drop<-Nbl[,2]-Nel[9:16,3]


compound_lost<-t(cbind(ml.drop, el.drop))
colnames(compound_lost)<-arms
rownames(compound_lost)<-c("Midline","Endline")

compound_lost


#-----------------------------------
#save data
#-----------------------------------
compound_tracking_A<-compound_tracking
#ch36_tracking_A<-ch36_tracking
tchild_tracking_A<-tchild_tracking
#compound_lost_A<-compound_lost

t1_tc_haz_A<-t1_tc_haz
t2_tc_haz_A<-t2_tc_haz
t1_tc_diar_A<-t1_tc_diar
t2_tc_diar_A<-t2_tc_diar

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
save(compound_tracking_A,tchild_tracking_A,t1_tc_haz_A,t2_tc_haz_A,t1_tc_diar_A,t2_tc_diar_A, file="CONSORT_Andrew.RData")

