
#---------------------------------------
# diar-recall-check.R
#
# ben arnold (benarnold@berkeley.edu)
# adapted to Kenya by andrew mertens (amertens@berkeley.edu)
#
# Check for differential recall errors
# in diarrhea by comparing current cases
# (2d recall) with terminated cases
# (those who answered "Yes" to 7d recall
# but "No" to 2d recall) using the C/T ratio.
#
# This is the approach used in Boerma et al 1991
# and recommended by Arnold et al. 2013 (appendix 4)
#
# Arnold et al. 2013. Optimal Recall Period for Caregiver-Reported Illness 
# in Risk Factor and Intervention Studies: A Multicountry Study.
# American Journal of Epidemiology 177 (4)
#
#---------------------------------------

#---------------------------------------
# input files:
#	washb-kenya-diar.csv
#
# output files:
# kenya-diar-recall-check.RData
#
#---------------------------------------


#---------------------------------------
# preamble
#---------------------------------------
rm(list=ls())
library(metafor)
library(washb)
library(dplyr)


#---------------------------------------
# Load the analysis dataset
#---------------------------------------
d <- read.csv("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew/washb-kenya-diar.csv")

#---------------------------------------
# Subset the Data to Follow-up data only
#---------------------------------------
table(d$studyyear)

#---------------------------------------
# Exclude:
# * siblings who were born after enrollment
# * siblings who were >36 mos at enrollment
# * children with missing outcome data
#---------------------------------------


#subset the diarrhea to children <36 mos at enrollment
#or target children
ad <-
  d %>%
  subset(., studyyear>0 & dcohort==1) %>%
  subset(., !is.na(.$diar7d))

dim(ad)
table(ad$diar7d, ad$tr,ad$studyyear)
table(ad$tr,ad$studyyear)

# re-order the tr factor so Control 
ad$tr <- factor(ad$tr,levels=c("Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH","Control"))

# create a variable for terminated diarrhea cases between 7d and 2d recall periods
ad$diart <- ifelse(ad$diar2d==0 & ad$diar7d==1,1,0)
table(ad$diar2d,ad$diart)  #532 terminated cases



#---------------------------------------
# Examine consistency between watery 
# stool and 3+ stool. Do they occur on 
# the same day?
#---------------------------------------
table(ad$threedefa, ad$softa)[2:3,2:3]
table(ad$threedefb, ad$softb)[2:3,2:3]
table(ad$threedefc, ad$softc)[2:3,2:3]
table(ad$threedefd, ad$softd)[2:3,2:3]


round(prop.table(table(ad$threedefa, ad$softa))[2:3,2:3],2)
round(prop.table(table(ad$threedefb, ad$softb))[2:3,2:3],2)
round(prop.table(table(ad$threedefc, ad$softc))[2:3,2:3],2)
round(prop.table(table(ad$threedefd, ad$softd))[2:3,2:3],2)

table(ad$altdiar2d,ad$diar2d)
table(ad$altdiar7d,ad$diar7d)

#---------------------------------------
# Current:Terminated ratio difference function
#---------------------------------------
ctratio <- function(dcurr,dterm,tr) {
  # dcurr: indicator of a current diarrhea case
  # dterm: indicator of a terminated diarrhea case
  # tr   : assigned treatment group
  #curr <- tapply(dcurr,tr,sum)
  #term <- tapply(dterm,tr,sum)
  curr <- table(dcurr, tr)[2,]  
  term <-table(dterm,tr)[2,] 
  ctratio <- curr/term
  return(ctratio)
}

ctratio(ad$diar2d,ad$diart,ad$tr)



#---------------------------------------
# cross-tabs of final observations
# in the analysis, by survey round
#---------------------------------------
table(ad$tr,ad$diar2d, ad$studyyear)
table(ad$tr,ad$diar7d, ad$studyyear)
table(ad$tr,ad$diart, ad$studyyear)


#---------------------------------------
# bootstrap the CT ratio by re-sampling
# randomization blocks with replacement
#---------------------------------------

set.seed(1349175)
nreps <- 1000
bsamp <- matrix(sample(unique(ad$block),size=length(unique(ad$block))*nreps,replace=TRUE),ncol=nreps)
ctratios <- matrix(rep(NA,nreps*8),ncol=8)
for(i in 1:nreps) {
  bd <- merge(ad,data.frame(block=bsamp[,i]),by="block",all.x=FALSE)
  ctratios[i,] <- ctratio(bd$diar2d,bd$diart,bd$tr)
}

# compute differences between each arm and control, then the mean and percentile 95% CIs
muctratio <- apply(ctratios,2,mean)[c(8,1:7)]
ctdiff <- ctratios[,1:7]-ctratios[,8]
ctmeans <- apply(ctdiff,2,mean)
ct95ci  <- apply(ctdiff,2,function(x) quantile(x,probs=c(0.025,0.975)))

res <- cbind(muctratio,c(NA,ctmeans),t(cbind(c(NA,NA),ct95ci)))
rownames(res) <- levels(ad$tr)[c(8,1:7)]
colnames(res) <- c("CTratio","CTratio diff","Lower 95% CI","Upper 95% CI")


# print results
res


# save everything except the datasets themselves
# that way we have all of the block-specific estimates if needed for plotting or additional stats
rm(list=c("d","ad"))
save.image(file="C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew/kenya-diar-recall-check.RData")





	