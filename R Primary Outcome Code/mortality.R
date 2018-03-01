
#---------------------------------------
# Mortality Analysis.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The mortality outcome analysis in Wash Benefits Kenya.
#---------------------------------------

library(foreign)
library(washb)

setwd("C:/Users/andre/Dropbox/Kenya Primary Analysis/Data-selected/clean")
d<-read.csv("washb-kenya-mortality.csv")
head(d)

# re-order the treatment factor for convenience in comparison
d$tr <- factor(d$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))





t(table(d$death,d$tr))


grouped.d <-
  d %>%
  group_by(tr) %>%
  do(as.data.frame(washb_mean(Y=.$death, id=.$clusterid, print=F)))

grouped.d


h1.contrasts <- list(c("Control","Passive Control"), c("Control","Water"), c("Control","Sanitation"), c("Control","Handwashing"), c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))


#Run mh function to calculate PR and RD for H1
d_h1_pr_unadj<-matrix(0, nrow=7, ncol=7)
d_h1_rd_unadj<-matrix(0, nrow=7, ncol=6)

for(i in 1:7){
  d_h1_pr_unadj[i,]<-washb_mh(Y=d$death, tr=d$tr, strat=d$block, contrast = h1.contrasts[[i]],measure="RR")
  d_h1_rd_unadj[i,]<-washb_mh(Y=d$death, tr=d$tr, strat=d$block, contrast = h1.contrasts[[i]],measure="RD")
}
rownames(d_h1_rd_unadj) <- rownames(d_h1_pr_unadj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
colnames(d_h1_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(d_h1_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")


####H2: Unadjusted prevalence ratios and risk differences; combined WSH vs. single arms.  PR, CI, MH P-value
h2.contrasts <- list(c("Water","WSH"), c("Sanitation","WSH"), c("Handwashing","WSH"))

#Run mh function to calculate RD and RR for H2
d_h2_pr_unadj<-matrix(0, nrow=3, ncol=7)
d_h2_rd_unadj<-matrix(0, nrow=3, ncol=6)

for(i in 1:3){
  d_h2_pr_unadj[i,]<-washb_mh(Y=d$death, tr=d$tr, strat=d$block, contrast = h2.contrasts[[i]],measure="RR")
  d_h2_rd_unadj[i,]<-washb_mh(Y=d$death, tr=d$tr, strat=d$block, contrast = h2.contrasts[[i]],measure="RD")
}
rownames(d_h2_rd_unadj) <-rownames(d_h2_pr_unadj) <-c("WSH v Water","WSH v Sanitation","WSH v Handwashing")
colnames(d_h2_rd_unadj)<-c("RD","SE","ci.lb","ci.ub","z","P-value")
colnames(d_h2_pr_unadj)<-c("PR","ci.lb","ci.ub","logPR","se.logPR","z","P-value")

