#---------------------------------------
# washb_functions.R
#
# andrew mertens (amertens@berkeley.edu)
#
# wrapper function for Anthro analysis to
# calculate same objects for all secondary outcomes
#---------------------------------------


##########
#anthrocalc
##########

#wrapper function for Kenya outcome analysis across different anthro measurements, using washb package functions



#----------------
##Create shell function to create required data objects for each child growth measure
#----------------
anthrocalc<-function(outcome,laz, family=NULL, nreps){
  try(detach(package:plyr))
  library(dplyr)
  laz$laz<-laz[,outcome]

  #Calculate mean
  grouped.lazmean <-
    laz %>%
    group_by(studyyear,tr) %>%
    do(as.data.frame(washb_mean(Y=.$laz, id=.$clusterid, print=F)))

  if(family[1]=="gaussian"){
  laz_t1_n<-cbind(grouped.lazmean[1:8,3:5]) %>% set_rownames(c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  laz_t2_n<-cbind(grouped.lazmean[9:16,3:5]) %>% set_rownames(c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  colnames(laz_t2_n)<-colnames(laz_t1_n)<-c("Prev","Mean","SD")
  }

  if(family[1]!="gaussian"){
    grouped.n <-
      laz %>%
      group_by(studyyear,laz,tr)%>%
      summarize(n=n())

    laz_t1_prev<-cbind(grouped.lazmean[1:8,c(4,7,8)])
    laz_t2_prev<-cbind(grouped.lazmean[9:16,c(4,7,8)])
    colnames(laz_t2_prev)<-colnames(laz_t1_prev)<-c("Prev","lb","ub")

  #Temp make empty column of n to get compare.R code to work
  laz_t1_n<-cbind(grouped.lazmean[1:8,3],grouped.n[9:16,4])%>% set_rownames(c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  laz_t2_n<-cbind(grouped.lazmean[9:16,3],grouped.n[25:32,4])%>% set_rownames(c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  colnames(laz_t2_n)<-colnames(laz_t1_n)<-c("N","n")
  }

  h1.contrasts <- list(c("Control","Passive Control"), c("Control","Water"), c("Control","Sanitation"), c("Control","Handwashing"), c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))
  h3.contrasts <- list(c("Nutrition","Nutrition + WSH"), c("WSH","Nutrition + WSH"))

  #subset to year 1
  laz1 <- subset(laz,studyyear==1)

  #unadj setup
  j=6
  colnamesRR<-c("PR","95CI lb","95CI ub","logPR","Std. Error","z value ","P-value")
  colnamesRD<-c("RD","SE","95CI lb","95CI ub","Z","P-value")
  if(family[[1]]=="gaussian"){
    j<-5
    colnamesRD<-c("RD","95CI lb","95CI ub","T-stat","P-value")
  }

    #Run ttest and MH functions to calculate rd for H1
    laz_t1_h1_rd_unadj<-matrix(0, nrow=7, ncol=j)
    laz_t1_h3_rd_unadj<-matrix(0, nrow=2, ncol=j)
    laz_t1_h1_pr_unadj<-matrix(0, nrow=7, ncol=j+1)
    laz_t1_h3_pr_unadj<-matrix(0, nrow=2, ncol=j+1)
    rownames(laz_t1_h1_rd_unadj) <-rownames(laz_t1_h1_pr_unadj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
    rownames(laz_t1_h3_rd_unadj) <-rownames(laz_t1_h3_pr_unadj) <-c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")

    if(family[[1]]!="gaussian"){colnames(laz_t1_h1_pr_unadj) <-colnamesRR}
    colnames(laz_t1_h1_rd_unadj) <-colnamesRD

    for(i in 1:7){
      #temp<-washb_glm(Y=laz1$laz, tr=laz1$tr, pair=laz1$block, W=NULL, contrast = h1.contrasts[[i]], id=laz1$block, family=family, print=T, verbose=F)
      #laz_t1_h1_rd_unadj[i,]<-as.matrix(temp$TR)
      if(family[[1]]=="gaussian"){
                laz_t1_h1_rd_unadj[i,]<-washb_ttest(Y=laz1$laz, tr=laz1$tr, strat=laz1$block, contrast = h1.contrasts[[i]])
      }
      if(family[[1]]!="gaussian"){
        #tempRD<-washb_glm(Y=laz1$laz, tr=laz1$tr, pair=laz1$block, W=NULL, contrast = h1.contrasts[[i]], id=laz1$block, family="gaussian", print=T, verbose=F)
                #laz_t1_h1_rd_unlazj[i,]<-as.matrix(tempRD$TR)
          laz_t1_h1_pr_unadj[i,]<-washb_mh(Y=laz1$laz, tr=laz1$tr, strat=laz1$block, contrast = h1.contrasts[[i]],measure="RR")
          laz_t1_h1_rd_unadj[i,]<-washb_mh(Y=laz1$laz, tr=laz1$tr, strat=laz1$block, contrast = h1.contrasts[[i]],measure="RD")
      }
    }

    #Run ttest function to calculate rd for H3
    for(i in 1:2){
       if(family[[1]]=="gaussian"){
         laz_t1_h3_rd_unadj[i,]<-washb_ttest(Y=laz1$laz, tr=laz1$tr, strat=laz1$block, contrast = h3.contrasts[[i]])
         }
      #temp<-washb_glm(Y=laz1$laz, tr=laz1$tr, pair=laz1$block, W=NULL, contrast = h3.contrasts[[i]], id=laz1$block, family=family, print=T, verbose=F)
      #laz_t1_h3_rd_unadj[i,]<-as.matrix(temp$TR)
      if(family[[1]]!="gaussian"){
        #tempRD<-washb_glm(Y=laz1$laz, tr=laz1$tr, pair=laz1$block, W=NULL, contrast = h3.contrasts[[i]], id=laz1$block, family="gaussian", print=T, verbose=F)
        #laz_t1_h3_rd_unadj[i,]<-as.matrix(tempRD$TR)
          laz_t1_h3_pr_unadj[i,]<-washb_mh(Y=laz1$laz, tr=laz1$tr, strat=laz1$block, contrast = h3.contrasts[[i]],measure="RR")
          laz_t1_h3_rd_unadj[i,]<-washb_mh(Y=laz1$laz, tr=laz1$tr, strat=laz1$block, contrast = h3.contrasts[[i]],measure="RD")
      }
    }





  #Create adjustment variable dataframe
  Wanthro<-subset(laz1, select=Wvars)
    #ensure factors are set
  Wanthro$fracode<-as.factor(Wanthro$fracode)
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
  laz1$block<-as.factor(laz1$block)




  #Run tmle function to calculate rd for H1
  laz_t1_h1_rd_adj<-laz_t1_h1_pr_adj<-matrix(NA, nrow=7, ncol=3)
  laz_t1_h3_rd_adj<-laz_t1_h3_pr_adj<-matrix(NA, nrow=2, ncol=3)

    #Order to match Jade
  laz1 <- laz1[order(laz1$block,laz1$clusterid,laz1$hhid,laz1$childid,laz1$studyyear),]

  for(i in 1:7){
  temp<-washb_tmle(Y=laz1$laz, tr=laz1$tr, W=Wanthro, id=laz1$block,pair=laz1$block,family=family, contrast= h1.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=T)
  laz_t1_h1_rd_adj[i,1]<-temp$estimates$ATE$psi
  laz_t1_h1_rd_adj[i,2:3]<-temp$estimates$ATE$CI
    if(family[[1]]!="gaussian"){
        laz_t1_h1_pr_adj[i,1]<-temp$estimates$RR$psi
        laz_t1_h1_pr_adj[i,2:3]<-temp$estimates$RR$CI
    }
  }
  rownames(laz_t1_h1_rd_adj) <- rownames(laz_t1_h1_pr_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")

  #Run tmle function to calculate rd for H3
    for(i in 1:2){
  temp<-washb_tmle(Y=laz1$laz, tr=laz1$tr, W=Wanthro, id=laz1$block,pair=laz1$block,family=family, contrast= h3.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=F)
  laz_t1_h3_rd_adj[i,1]<-temp$estimates$ATE$psi
  laz_t1_h3_rd_adj[i,2:3]<-temp$estimates$ATE$CI
    if(family[[1]]!="gaussian"){
        laz_t1_h3_pr_adj[i,1]<-temp$estimates$RR$psi
        laz_t1_h3_pr_adj[i,2:3]<-temp$estimates$RR$CI
      }
    }

  rownames(laz_t1_h3_rd_adj) <-rownames(laz_t1_h3_pr_adj) <-c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")
  colnames(laz_t1_h1_rd_adj)<-colnames(laz_t1_h3_rd_adj)<-colnames(laz_t1_h1_pr_adj)<-colnames(laz_t1_h3_pr_adj)<-c("RD","ci.lb","ci.ub")






  #Run permutation function to calculate for H1

  laz_t1_h1_pval_unadj<-t(sapply(h1.contrasts, washb_permute, Y=laz1$laz, tr=laz1$tr, pair=laz1$block, nreps=nreps, seed=67890))
  laz_t1_h3_pval_unadj<-t(sapply(h3.contrasts, washb_permute, Y=laz1$laz, tr=laz1$tr, pair=laz1$block, nreps=nreps, seed=67890))
  laz_t1_h1_pval_unadj<-(as.matrix(unlist(laz_t1_h1_pval_unadj[,1])))
  laz_t1_h3_pval_unadj<-(as.matrix(unlist(laz_t1_h3_pval_unadj[,1])))
  rownames(laz_t1_h1_pval_unadj)<-c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
  rownames(laz_t1_h3_pval_unadj) <- c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")


  #Adjusted Permutation tests
  # restrict to complete cases

  preSLd <- data.frame(id=laz1$clusterid,childid=laz1$childid,hhid=laz1$hhid,block=laz1$block,tr=laz1$tr,svy=laz1$studyyear,Y=laz1$laz,Wanthro)
  dim(preSLd)

  # pre-screen the covariates for those associated with the outcome (LR test P<0.2)
  preSLd <- preSLd[order(preSLd$childid),]


  Wscreen <- washb_prescreen(Y=preSLd$Y,Ws=preSLd[,8:ncol(preSLd)],family=family)
  Wselect <- subset(preSLd,select=Wscreen)

  SLd <- data.frame(id=preSLd$id,hhid=preSLd$hhid,childid=preSLd$childid,block=preSLd$block,tr=preSLd$tr,svy=preSLd$svy,Y=preSLd$Y,Wselect)
  dim(SLd)
  # restrict to complete cases
  SLd <- SLd[complete.cases(SLd),]


  #Order to match Jade
  SLd <- SLd[order(SLd$block,SLd$id,SLd$hhid,SLd$childid,SLd$svy),]

  Wselect<-SLd[,c(8:ncol(SLd))]
  Wselect <- design_matrix(Wselect)
  dim(SLd)
  dim(Wselect)
  head(SLd)
  head(Wselect)
  colnames(Wselect)

  set.seed(67890)
  SLfit2 <- SuperLearner(Y=SLd$Y,X=Wselect,id=SLd$id,
                         family=family,
                         SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
  )
  SLfit2
  SLd$pY <- as.vector(predict(SLfit2)$pred)
  SLd$r <- SLd$Y-SLd$pY


  #Run permutation function
  #create empty matrices for permutation function results
  laz_t1_h1_pval_adj<-matrix(nrow=7,ncol=1)
  laz_t1_h3_pval_adj<-matrix(nrow=2,ncol=1)

  dim(SLd)
  temp<-SLd[SLd$tr=="Control",]
  dim(temp)
  temp2<-SLd[SLd$tr=="Sanitation",]
  dim(temp2)

  #Run permutation function for H1
  for(i in 1:7){
    temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=nreps,contrast=h1.contrasts[[i]], seed=67890)
    laz_t1_h1_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
  }
  #Run permutation function for H2
  for(i in 1:2){
    temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=nreps,contrast=h3.contrasts[[i]], seed=67890)
    laz_t1_h3_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
  }

  rownames(laz_t1_h1_pval_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
  rownames(laz_t1_h3_pval_adj) <- c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")









  #subset to year 2
  laz2 <- subset(laz,studyyear==2)



    #Run ttest and MH functions to calculate rd for H1
    laz_t2_h1_rd_unadj<-matrix(0, nrow=7, ncol=j)
    laz_t2_h3_rd_unadj<-matrix(0, nrow=2, ncol=j)
    laz_t2_h1_pr_unadj<-matrix(0, nrow=7, ncol=j+1)
    laz_t2_h3_pr_unadj<-matrix(0, nrow=2, ncol=j+1)


    for(i in 1:7){
      #temp<-washb_glm(Y=laz2$laz, tr=laz2$tr, pair=laz2$block, W=NULL, contrast = h1.contrasts[[i]], id=laz2$block, family=family, print=T, verbose=F)
      #laz_t2_h1_rd_unadj[i,]<-as.matrix(temp$TR)
      if(family[[1]]=="gaussian"){
                laz_t2_h1_rd_unadj[i,]<-washb_ttest(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h1.contrasts[[i]])
      }
      if(family[[1]]!="gaussian"){
        #tempRD<-washb_glm(Y=laz2$laz, tr=laz2$tr, pair=laz2$block, W=NULL, contrast = h1.contrasts[[i]], id=laz2$block, family="gaussian", print=T, verbose=F)
                #laz_t2_h1_rd_unlazj[i,]<-as.matrix(tempRD$TR)
          laz_t2_h1_pr_unadj[i,]<-washb_mh(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h1.contrasts[[i]],measure="RR")
          laz_t2_h1_rd_unadj[i,]<-washb_mh(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h1.contrasts[[i]],measure="RD")
      }
    }
    rownames(laz_t2_h1_rd_unadj) <-rownames(laz_t2_h1_pr_unadj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")

    #Run glm function to calculate rd for H3
    for(i in 1:2){
       if(family[[1]]=="gaussian"){
         laz_t2_h3_rd_unadj[i,]<-washb_ttest(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h3.contrasts[[i]])
         }
      #temp<-washb_glm(Y=laz2$laz, tr=laz2$tr, pair=laz2$block, W=NULL, contrast = h3.contrasts[[i]], id=laz2$block, family=family, print=T, verbose=F)
      #laz_t2_h3_rd_unadj[i,]<-as.matrix(temp$TR)
      if(family[[1]]!="gaussian"){
        #tempRD<-washb_glm(Y=laz2$laz, tr=laz2$tr, pair=laz2$block, W=NULL, contrast = h3.contrasts[[i]], id=laz2$block, family="gaussian", print=T, verbose=F)
        #laz_t2_h3_rd_unadj[i,]<-as.matrix(tempRD$TR)
          laz_t2_h3_pr_unadj[i,]<-washb_mh(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h3.contrasts[[i]],measure="RR")
          laz_t2_h3_rd_unadj[i,]<-washb_mh(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h3.contrasts[[i]],measure="RD")
      }
    }
    rownames(laz_t2_h3_rd_unadj)<-rownames(laz_t2_h3_pr_unadj)<-c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")
    colnames(laz_t2_h1_rd_unadj)<-colnames(laz_t2_h3_rd_unadj)<-colnamesRD
    if(family[[1]]!="gaussian"){colnames(laz_t2_h1_pr_unadj)<-colnames(laz_t2_h3_pr_unadj)<-colnamesRR}










  #Create adjustment variable dataframe
  Wanthro<-subset(laz2, select=Wvars)
  #ensure factors are set
  Wanthro$fracode<-as.factor(Wanthro$fracode)
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
  laz2$block<-as.factor(laz2$block)



  #Run tmle function to calculate rd for H1
  laz_t2_h1_rd_adj<-laz_t2_h1_pr_adj<-matrix(NA, nrow=7, ncol=3)
  laz_t2_h3_rd_adj<-laz_t2_h3_pr_adj<-matrix(NA, nrow=2, ncol=3)

  #Order to match Jade
  laz2 <- laz2[order(laz2$block,laz2$clusterid,laz2$hhid,laz2$childid,laz2$studyyear),]

  for(i in 1:7){
  temp<-washb_tmle(Y=laz2$laz, tr=laz2$tr, W=Wanthro, id=laz2$block,pair=laz2$block,family=family, contrast= h1.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=F)
  laz_t2_h1_rd_adj[i,1]<-temp$estimates$ATE$psi
  laz_t2_h1_rd_adj[i,2:3]<-temp$estimates$ATE$CI
    if(family[[1]]!="gaussian"){
        laz_t2_h1_pr_adj[i,1]<-temp$estimates$RR$psi
        laz_t2_h1_pr_adj[i,2:3]<-temp$estimates$RR$CI
    }
  }
  rownames(laz_t2_h1_rd_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")

  #Run tmle function to calculate rd for H3
    for(i in 1:2){
  temp<-washb_tmle(Y=laz2$laz, tr=laz2$tr, W=Wanthro, id=laz2$block,pair=laz2$block,family=family, contrast= h3.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=F)
  laz_t2_h3_rd_adj[i,1]<-temp$estimates$ATE$psi
  laz_t2_h3_rd_adj[i,2:3]<-temp$estimates$ATE$CI
    if(family[[1]]!="gaussian"){
        laz_t2_h3_pr_adj[i,1]<-temp$estimates$RR$psi
        laz_t2_h3_pr_adj[i,2:3]<-temp$estimates$RR$CI
      }
    }

    rownames(laz_t1_h3_rd_adj) <-rownames(laz_t1_h3_pr_adj) <-c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")
    colnames(laz_t1_h1_rd_adj)<-colnames(laz_t1_h3_rd_adj)<-colnames(laz_t1_h1_pr_adj)<-colnames(laz_t1_h3_pr_adj)<-c("RD","ci.lb","ci.ub")





  #Run permutation function to calculate for H1
  laz_t2_h1_pval_unadj<-t(sapply(h1.contrasts, washb_permute, Y=laz2$laz, tr=laz2$tr, pair=laz2$block, nreps=nreps, seed=67890))
  laz_t2_h3_pval_unadj<-t(sapply(h3.contrasts, washb_permute, Y=laz2$laz, tr=laz2$tr, pair=laz2$block, nreps=nreps, seed=67890))
  laz_t2_h1_pval_unadj<-as.matrix(unlist(laz_t2_h1_pval_unadj[,1]))
  laz_t2_h3_pval_unadj<-as.matrix(unlist(laz_t2_h3_pval_unadj[,1]))
  rownames(laz_t2_h1_pval_unadj)<-c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
  rownames(laz_t2_h3_pval_unadj) <- c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")




  #Adjusted Permutation tests
  # restrict to complete cases
  preSLd <- data.frame(id=laz2$clusterid,hhid=laz2$hhid,childid=laz2$childid,block=laz2$block,tr=laz2$tr,svy=laz2$studyyear,Y=laz2$laz,Wanthro)

  # pre-screen the covariates for those associated with the outcome (LR test P<0.2)
  # see Wprescreen() and design.matrix() in the base functions
  Wscreen <- washb_prescreen(Y=preSLd$Y,Ws=preSLd[,8:ncol(preSLd)],family=family)
  Wselect <- subset(preSLd,select=Wscreen)



  SLd <- data.frame(id=preSLd$id,hhid=preSLd$hhid,childid=preSLd$childid,block=preSLd$block,tr=preSLd$tr,svy=preSLd$svy,Y=preSLd$Y,Wselect)

  # restrict to complete cases
  SLd <- SLd[complete.cases(SLd),]

   #Order to match Jade
  SLd <- SLd[order(SLd$block,SLd$id,SLd$hhid,SLd$childid,SLd$svy),]

  Wselect<-SLd[,c(8:ncol(SLd))]
  Wselect <- design_matrix(Wselect)


  set.seed(67890)
  SLfit2 <- SuperLearner(Y=SLd$Y,X=Wselect,id=SLd$id,
                         family=family,
                         SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
  )
  SLfit2
  SLd$pY <- as.vector(predict(SLfit2)$pred)
  SLd$r <- SLd$Y-SLd$pY


  #Run permutation function
  #create empty matrices for permutation function results
  laz_t2_h1_pval_adj<-matrix(nrow=7,ncol=1)
  laz_t2_h3_pval_adj<-matrix(nrow=2,ncol=1)

  #Run permutation function for H1
  for(i in 1:7){
    temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=nreps,contrast=h1.contrasts[[i]], seed=67890)
    laz_t2_h1_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
  }
  #Run permutation function for H2
  for(i in 1:2){
    temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=nreps,contrast=h3.contrasts[[i]], seed=67890)
    laz_t2_h3_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
  }
  rownames(laz_t2_h1_pval_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
  rownames(laz_t2_h3_pval_adj) <- c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")



  if(family[[1]]=="gaussian"){
    anthro_results<-list(t1_n=laz_t1_n,
                         t2_n=laz_t2_n,
                         t1_h1_rd_unadj=laz_t1_h1_rd_unadj,
                         t1_h3_rd_unadj=laz_t1_h3_rd_unadj,
                         t2_h1_rd_unadj=laz_t2_h1_rd_unadj,
                         t2_h3_rd_unadj=laz_t2_h3_rd_unadj,
                         t1_h1_rd_adj=laz_t1_h1_rd_adj,
                         t1_h3_rd_adj=laz_t1_h3_rd_adj,
                         t2_h1_rd_adj=laz_t2_h1_rd_adj,
                         t2_h3_rd_adj=laz_t2_h3_rd_adj,
                         t1_h1_pval_unadj=laz_t1_h1_pval_unadj,
                         t1_h3_pval_unadj=laz_t1_h3_pval_unadj,
                         t2_h1_pval_unadj=laz_t2_h1_pval_unadj,
                         t2_h3_pval_unadj=laz_t2_h3_pval_unadj,
                         t1_h1_pval_adj=laz_t1_h1_pval_adj,
                         t1_h3_pval_adj=laz_t1_h3_pval_adj,
                         t2_h1_pval_adj=laz_t2_h1_pval_adj,
                         t2_h3_pval_adj=laz_t2_h3_pval_adj)
  }else{
    anthro_results<-list(t1_n=laz_t1_n,t2_n=laz_t2_n,
                         t1_prev=laz_t1_prev,t2_prev=laz_t2_prev,
                         t1_h1_pr_unadj=laz_t1_h1_pr_unadj,
                         t1_h1_rd_unadj=laz_t1_h1_rd_unadj,
                         t1_h3_pr_unadj=laz_t1_h3_pr_unadj,
                         t1_h3_rd_unadj=laz_t1_h3_rd_unadj,
                         t2_h1_pr_unadj=laz_t2_h1_pr_unadj,
                         t2_h1_rd_unadj=laz_t2_h1_rd_unadj,
                         t2_h3_pr_unadj=laz_t2_h3_pr_unadj,
                         t2_h3_rd_unadj=laz_t2_h3_rd_unadj,

                         t1_h1_pr_adj=laz_t1_h1_pr_adj,
                         t1_h1_rd_adj=laz_t1_h1_rd_adj,
                         t1_h3_pr_adj=laz_t1_h3_pr_adj,
                         t1_h3_rd_adj=laz_t1_h3_rd_adj,
                         t2_h1_pr_adj=laz_t2_h1_pr_adj,
                         t2_h1_rd_adj=laz_t2_h1_rd_adj,
                         t2_h3_pr_adj=laz_t2_h3_pr_adj,
                         t2_h3_rd_adj=laz_t2_h3_rd_adj,

                         t1_h1_pval_unadj=laz_t1_h1_pval_unadj,
                         t1_h3_pval_unadj=laz_t1_h3_pval_unadj,
                         t2_h1_pval_unadj=laz_t2_h1_pval_unadj,
                         t2_h3_pval_unadj=laz_t2_h3_pval_unadj,
                         t1_h1_pval_adj=laz_t1_h1_pval_adj,
                         t1_h3_pval_adj=laz_t1_h3_pval_adj,
                         t2_h1_pval_adj=laz_t2_h1_pval_adj,
                         t2_h3_pval_adj=laz_t2_h3_pval_adj)
                        }
  return(anthro_results)
}















##########
#anthrosens
##########

#wrapper function for sensitivity analysis of year 2 anthro measurements, 
#stratified by child age 

#----------------
##Create shell function to create required data objects for each child growth measure
#----------------
anthrosens<-function(outcome,laz, family=NULL, nreps){
  try(detach(package:plyr))
  library(dplyr)
  laz$laz<-laz[,outcome]

  #Calculate mean
  grouped.lazmean <-
    laz %>%
    group_by(studyyear,tr) %>%
    do(as.data.frame(washb_mean(Y=.$laz, id=.$clusterid, print=F)))

  if(family[1]=="gaussian"){
  laz_t2_n<-cbind(grouped.lazmean[1:8,3:5]) %>% set_rownames(c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  colnames(laz_t2_n)<-c("Prev","Mean","SD")
  }

  if(family[1]!="gaussian"){
    grouped.n <-
      laz %>%
      group_by(studyyear,laz,tr)%>%
      summarize(n=n())

    laz_t2_prev<-cbind(grouped.lazmean[1:8,c(4,7,8)])
    colnames(laz_t2_prev)<-c("Prev","lb","ub")

  #Temp make empty column of n to get compare.R code to work
  laz_t2_n<-cbind(grouped.lazmean[1:8,3],grouped.n[9:16,4])%>% set_rownames(c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
  colnames(laz_t2_n)<-c("N","n")
  }

  h1.contrasts <- list(c("Control","Passive Control"), c("Control","Water"), c("Control","Sanitation"), c("Control","Handwashing"), c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))
  h3.contrasts <- list(c("Nutrition","Nutrition + WSH"), c("WSH","Nutrition + WSH"))

  #subset to year 2
  laz2 <- subset(laz,studyyear==2)

  #unadj setup
  j=6
  colnamesRR<-c("PR","95CI lb","95CI ub","logPR","Std. Error","z value ","P-value")
  colnamesRD<-c("RD","SE","95CI lb","95CI ub","Z","P-value")
  if(family[[1]]=="gaussian"){
    j<-5
    colnamesRD<-c("RD","95CI lb","95CI ub","T-stat","P-value")
  }

    #Run ttest and MH functions to calculate rd for H1
    laz_t2_h1_rd_unadj<-matrix(0, nrow=7, ncol=j)
    laz_t2_h3_rd_unadj<-matrix(0, nrow=2, ncol=j)
    laz_t2_h1_pr_unadj<-matrix(0, nrow=7, ncol=j+1)
    laz_t2_h3_pr_unadj<-matrix(0, nrow=2, ncol=j+1)
    rownames(laz_t2_h1_rd_unadj) <-rownames(laz_t2_h1_pr_unadj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
    rownames(laz_t2_h3_rd_unadj) <-rownames(laz_t2_h3_pr_unadj) <-c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")

    if(family[[1]]!="gaussian"){colnames(laz_t2_h1_pr_unadj) <-colnamesRR}
    colnames(laz_t2_h1_rd_unadj) <-colnamesRD

    for(i in 1:7){
      if(family[[1]]=="gaussian"){
                laz_t2_h1_rd_unadj[i,]<-washb_ttest(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h1.contrasts[[i]])
      }
      if(family[[1]]!="gaussian"){
          laz_t2_h1_pr_unadj[i,]<-washb_mh(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h1.contrasts[[i]],measure="RR")
          laz_t2_h1_rd_unadj[i,]<-washb_mh(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h1.contrasts[[i]],measure="RD")
      }
    }

    #Run ttest function to calculate rd for H3
    for(i in 1:2){
       if(family[[1]]=="gaussian"){
         laz_t2_h3_rd_unadj[i,]<-washb_ttest(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h3.contrasts[[i]])
         }
      if(family[[1]]!="gaussian"){
          laz_t2_h3_pr_unadj[i,]<-washb_mh(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h3.contrasts[[i]],measure="RR")
          laz_t2_h3_rd_unadj[i,]<-washb_mh(Y=laz2$laz, tr=laz2$tr, strat=laz2$block, contrast = h3.contrasts[[i]],measure="RD")
      }
    }





  #Create adjustment variable dataframe
  Wanthro<-subset(laz2, select=Wvars)
    #ensure factors are set
  Wanthro$fracode<-as.factor(Wanthro$fracode)
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
  laz2$block<-as.factor(laz2$block)




  #Run tmle function to calculate rd for H1
  laz_t2_h1_rd_adj<-laz_t2_h1_pr_adj<-matrix(NA, nrow=7, ncol=3)
  laz_t2_h3_rd_adj<-laz_t2_h3_pr_adj<-matrix(NA, nrow=2, ncol=3)

    #Order to match Jade
  laz2 <- laz2[order(laz2$block,laz2$clusterid,laz2$hhid,laz2$childid,laz2$studyyear),]

  for(i in 1:7){
  temp<-washb_tmle(Y=laz2$laz, tr=laz2$tr, W=Wanthro, id=laz2$block,pair=laz2$block,family=family, contrast= h1.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=T)
  laz_t2_h1_rd_adj[i,1]<-temp$estimates$ATE$psi
  laz_t2_h1_rd_adj[i,2:3]<-temp$estimates$ATE$CI
    if(family[[1]]!="gaussian"){
        laz_t2_h1_pr_adj[i,1]<-temp$estimates$RR$psi
        laz_t2_h1_pr_adj[i,2:3]<-temp$estimates$RR$CI
    }
  }
  rownames(laz_t2_h1_rd_adj) <- rownames(laz_t2_h1_pr_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")

  #Run tmle function to calculate rd for H3
    for(i in 1:2){
  temp<-washb_tmle(Y=laz2$laz, tr=laz2$tr, W=Wanthro, id=laz2$block,pair=laz2$block,family=family, contrast= h3.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=F)
  laz_t2_h3_rd_adj[i,1]<-temp$estimates$ATE$psi
  laz_t2_h3_rd_adj[i,2:3]<-temp$estimates$ATE$CI
    if(family[[1]]!="gaussian"){
        laz_t2_h3_pr_adj[i,1]<-temp$estimates$RR$psi
        laz_t2_h3_pr_adj[i,2:3]<-temp$estimates$RR$CI
      }
    }

  rownames(laz_t2_h3_rd_adj) <-rownames(laz_t2_h3_pr_adj) <-c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")
  colnames(laz_t2_h1_rd_adj)<-colnames(laz_t2_h3_rd_adj)<-colnames(laz_t2_h1_pr_adj)<-colnames(laz_t2_h3_pr_adj)<-c("RD","ci.lb","ci.ub")






  #Run permutation function to calculate for H1

  laz_t2_h1_pval_unadj<-t(sapply(h1.contrasts, washb_permute, Y=laz2$laz, tr=laz2$tr, pair=laz2$block, nreps=nreps, seed=67890))
  laz_t2_h3_pval_unadj<-t(sapply(h3.contrasts, washb_permute, Y=laz2$laz, tr=laz2$tr, pair=laz2$block, nreps=nreps, seed=67890))
  laz_t2_h1_pval_unadj<-(as.matrix(unlist(laz_t2_h1_pval_unadj[,1])))
  laz_t2_h3_pval_unadj<-(as.matrix(unlist(laz_t2_h3_pval_unadj[,1])))
  rownames(laz_t2_h1_pval_unadj)<-c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
  rownames(laz_t2_h3_pval_unadj) <- c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")


  #Adjusted Permutation tests
  # restrict to complete cases

  preSLd <- data.frame(id=laz2$clusterid,childid=laz2$childid,hhid=laz2$hhid,block=laz2$block,tr=laz2$tr,svy=laz2$studyyear,Y=laz2$laz,Wanthro)
  dim(preSLd)

  # pre-screen the covariates for those associated with the outcome (LR test P<0.2)
  preSLd <- preSLd[order(preSLd$childid),]


  Wscreen <- washb_prescreen(Y=preSLd$Y,Ws=preSLd[,8:ncol(preSLd)],family=family)
  Wselect <- subset(preSLd,select=Wscreen)

  SLd <- data.frame(id=preSLd$id,hhid=preSLd$hhid,childid=preSLd$childid,block=preSLd$block,tr=preSLd$tr,svy=preSLd$svy,Y=preSLd$Y,Wselect)
  dim(SLd)
  # restrict to complete cases
  SLd <- SLd[complete.cases(SLd),]


  #Order to match Jade
  SLd <- SLd[order(SLd$block,SLd$id,SLd$hhid,SLd$childid,SLd$svy),]

  Wselect<-SLd[,c(8:ncol(SLd))]
  Wselect <- design_matrix(Wselect)
  dim(SLd)
  dim(Wselect)
  head(SLd)
  head(Wselect)
  colnames(Wselect)

  set.seed(67890)
  SLfit2 <- SuperLearner(Y=SLd$Y,X=Wselect,id=SLd$id,
                         family=family,
                         SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
  )
  SLfit2
  SLd$pY <- as.vector(predict(SLfit2)$pred)
  SLd$r <- SLd$Y-SLd$pY


  #Run permutation function
  #create empty matrices for permutation function results
  laz_t2_h1_pval_adj<-matrix(nrow=7,ncol=1)
  laz_t2_h3_pval_adj<-matrix(nrow=2,ncol=1)

  dim(SLd)
  temp<-SLd[SLd$tr=="Control",]
  dim(temp)
  temp2<-SLd[SLd$tr=="Sanitation",]
  dim(temp2)

  #Run permutation function for H1
  for(i in 1:7){
    temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=nreps,contrast=h1.contrasts[[i]], seed=67890)
    laz_t2_h1_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
  }
  #Run permutation function for H2
  for(i in 1:2){
    temp<-washb_permute(Y=SLd$r,tr=SLd$tr,pair=SLd$block, nreps=nreps,contrast=h3.contrasts[[i]], seed=67890)
    laz_t2_h3_pval_adj[i,]<-as.matrix(unlist(temp$p.value))
  }

  rownames(laz_t2_h1_pval_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")
  rownames(laz_t2_h3_pval_adj) <- c("Nutrition + WSH v Nutrition","Nutrition + WSH v WSH")











  if(family[[1]]=="gaussian"){
    anthro_results<-list(
                         t2_n=laz_t2_n,
                         t2_h1_rd_unadj=laz_t2_h1_rd_unadj,
                         t2_h3_rd_unadj=laz_t2_h3_rd_unadj,
                         t2_h1_rd_adj=laz_t2_h1_rd_adj,
                         t2_h3_rd_adj=laz_t2_h3_rd_adj,
                         t2_h1_pval_unadj=laz_t2_h1_pval_unadj,
                         t2_h3_pval_unadj=laz_t2_h3_pval_unadj,
                         t2_h1_pval_adj=laz_t2_h1_pval_adj,
                         t2_h3_pval_adj=laz_t2_h3_pval_adj)
  }else{
    anthro_results<-list(
                         t2_h1_pr_unadj=laz_t2_h1_pr_unadj,
                         t2_h1_rd_unadj=laz_t2_h1_rd_unadj,
                         t2_h3_pr_unadj=laz_t2_h3_pr_unadj,
                         t2_h3_rd_unadj=laz_t2_h3_rd_unadj,
                         t2_h1_pr_adj=laz_t2_h1_pr_adj,
                         t2_h1_rd_adj=laz_t2_h1_rd_adj,
                         t2_h3_pr_adj=laz_t2_h3_pr_adj,
                         t2_h3_rd_adj=laz_t2_h3_rd_adj,

                         t2_h1_pval_unadj=laz_t2_h1_pval_unadj,
                         t2_h3_pval_unadj=laz_t2_h3_pval_unadj,
                         t2_h1_pval_adj=laz_t2_h1_pval_adj,
                         t2_h3_pval_adj=laz_t2_h3_pval_adj)
                        }
  return(anthro_results)
}











