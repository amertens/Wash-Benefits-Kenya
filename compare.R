##############################################
# WASH Benefits Kenya
# Primary outcome analysis
# Compare Andrew and Jade's results

# by Jade
##############################################

rm(list=ls())
try(setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/results"))
try(setwd("~/Dropbox/wbk-primary-analysis/results/"))

#----------------------------------------
# function to take the difference between
# two objects with the same structure
# with different suffixes
# obj = vector of objects
# suffix1 = first suffix
# suffix2 = second suffix
#----------------------------------------
compare=function(obj,suffix1,suffix2){
  for(i in 1:length(obj)){
    j <- get(paste(obj[i],suffix1,sep=""))
    a <- get(paste(obj[i],suffix2,sep=""))
    cat("\n------------------------------\n",
        "Comparison of: ",obj[i],
        "\n------------------------------\n")
    cat("\n------------------------------\n",
        "Jade's Estimates",
        "\n------------------------------\n")
    print(j)
    cat("\n\n------------------------------\n",
        "Andrew's Estimates",
        "\n------------------------------\n")
    print(a)
    cat("\n\n------------------------------\n",
        "Difference (Jade - Andrew)",
        "\n------------------------------\n")
    if(all(abs(j-a)<1e-8)) cat("\n PERFECT REPLICATION !!!\n\n")
    else{
      if(all(abs(j-a)<1e-3 & abs(j-a)>1e-8)) cat("\n NEARLY PERFECT REPLICATION !!!\n\n")
      print(round(j-a,5))
    }
  }
}

# list of object names

#######################################
# PRIMARY OUTCOMES
#######################################
#-------------------------------------
# CONSORT
#-------------------------------------
#obj=c("compound_tracking","tchild_tracking")
#obj=c("compound_lost","ch36_tracking")

#-------------------------------------
# Unadjusted
#-------------------------------------
obj=c("diar_t0_n","diar_t1_n","diar_t2_n", "diar_t0_prev","diar_t1_prev","diar_t2_prev")

obj=c("diar_h1_pr_unadj","diar_h1_rd_unadj","diar_h2_pr_unadj","diar_h2_rd_unadj")
#obj=c("diar_h1_pval_unadj","diar_h2_pval_unadj")

 obj=c("laz_t1_n","laz_t2_n")
#
 obj=c("laz_t1_h1_rd_unadj","laz_t1_h3_rd_unadj","laz_t2_h1_rd_unadj","laz_t2_h3_rd_unadj")
#

obj=c("laz_t1_h1_pval_unadj","laz_t2_h1_pval_unadj","laz_t2_h1_pval_unadj","laz_t2_h3_pval_unadj")

#-------------------------------------
# Adjusted
#-------------------------------------
obj=c("diar_h1_pr_adj","diar_h2_pr_adj", "diar_h1_rd_adj","diar_h2_rd_adj")
#obj=c("diar_h1_pval_adj","diar_h2_pval_adj")

obj=c("laz_t1_h1_diff_adj","laz_t1_h3_diff_adj","laz_t2_h1_diff_adj","laz_t2_h3_diff_adj")
#obj=c("laz_t1_h1_pval_adj","laz_t1_h3_pval_adj",
#      "laz_t2_h1_pval_adj","laz_t2_h3_pval_adj")

#######################################
# SECONDARY OUTCOMES
#######################################
#-------------------------------------
# Unadjusted
#-------------------------------------
 #obj=c("waz_t1_n","waz_t2_n")
# obj=c("waz_t1_h1_diff_unadj", "waz_t1_h3_diff_unadj",
#  "waz_t2_h1_diff_unadj", "waz_t2_h3_diff_unadj")
# obj=c("waz_t1_h1_diff_adj", "waz_t1_h3_diff_adj",
#   "waz_t2_h1_diff_adj", "waz_t2_h3_diff_adj")
 # obj=c("waz_t1_h1_pval_unadj", "waz_t1_h3_pval_unadj",
 #   "waz_t2_h1_pval_unadj", "waz_t2_h3_pval_unadj")


#obj=c("whz_t1_n","waz_t2_n")
# obj=c("whz_t1_h1_diff_unadj", "whz_t1_h3_diff_unadj",
#   "whz_t2_h1_diff_unadj", "whz_t2_h3_diff_unadj")
# obj=c("whz_t1_h1_diff_adj", "whz_t1_h3_diff_adj",
#    "whz_t2_h1_diff_adj", "whz_t2_h3_diff_adj")
# obj=c("whz_t1_h1_pval_unadj", "whz_t1_h3_pval_unadj",
#   "whz_t2_h1_pval_unadj", "whz_t2_h3_pval_unadj")

#obj=c("hcz_t1_n","hcz_t2_n")
# obj=c("hcz_t1_h1_diff_unadj", "hcz_t1_h3_diff_unadj",
#   "hcz_t2_h1_diff_unadj", "hcz_t2_h3_diff_unadj")
# obj=c("hcz_t1_h1_diff_adj", "hcz_t1_h3_diff_adj",
#    "hcz_t2_h1_diff_adj", "hcz_t2_h3_diff_adj")
# obj=c("hcz_t1_h1_pval_unadj", "hcz_t1_h3_pval_unadj",
#  "hcz_t2_h1_pval_unadj", "hcz_t2_h3_pval_unadj")

# obj=c("stunt_t1_n", "stunt_t2_n", "stunt_t1_prev", "stunt_t2_prev")
# obj=c("stunt_t1_h1_pr_unadj", "stunt_t1_h3_pr_unadj",
#   "stunt_t1_h1_rd_unadj", "stunt_t1_h3_rd_unadj",
#   "stunt_t2_h1_pr_unadj", "stunt_t2_h3_pr_unadj",
#   "stunt_t2_h1_rd_unadj", "stunt_t2_h3_rd_unadj")
# obj=c("stunt_t1_h1_pval_unadj","stunt_t1_h3_pval_unadj",
#      "stunt_t2_h1_pval_unadj","stunt_t2_h3_pval_unadj")

# obj=c("sstunt_t1_n", "sstunt_t2_n", "sstunt_t1_prev", "sstunt_t2_prev")
# obj=c("sstunt_t1_h1_pr_unadj", "sstunt_t1_h3_pr_unadj",
#   "sstunt_t1_h1_rd_unadj", "sstunt_t1_h3_rd_unadj",
#   "sstunt_t2_h1_pr_unadj", "sstunt_t2_h3_pr_unadj",
#   "sstunt_t2_h1_rd_unadj", "sstunt_t2_h3_rd_unadj")
# obj=c("stuntt_t1_h1_pval_unadj","stuntt_t1_h3_pval_unadj",
#      "stuntt_t2_h1_pval_unadj","stuntt_t2_h3_pval_unadj")

# obj=c("wast_t1_n", "wast_t2_n", "wast_t1_prev", "wast_t2_prev")
# obj=c("wast_t1_h1_rd_unadj", "wast_t1_h3_rd_unadj",
#   "wast_t2_h1_rd_unadj", "wast_t2_h3_rd_unadj",
#   "wast_t1_h1_pr_unadj", "wast_t1_h3_pr_unadj",
#   "wast_t2_h1_rd_unadj", "wast_t2_h3_rd_unadj")
# obj=c("wast_t1_h1_pval_unadj","wast_t1_h3_pval_unadj",
#      "wast_t2_h1_pval_unadj","wast_t2_h3_pval_unadj")

# obj=c("underwt_t1_n", "underwt_t2_n", "underwt_t1_prev", "underwt_t2_prev")
# obj=c("underwt_t1_h1_rd_unadj", "underwt_t1_h3_rd_unadj",
#       "underwt_t2_h1_rd_unadj", "underwt_t2_h3_rd_unadj",
#       "underwt_t1_h1_pr_unadj", "underwt_t1_h3_pr_unadj",
#       "underwt_t2_h1_rd_unadj", "underwt_t2_h3_rd_unadj")
# obj=c("underwt_t1_h1_pval_unadj","underwt_t1_h3_pval_unadj",
#      "underwt_t2_h1_pval_unadj","underwt_t2_h3_pval_unadj")

#-------------------------------------
# Adjusted
#-------------------------------------
# obj=c("waz_t1_h1_pval_adj", "waz_t1_h3_pval_adj",
#   "waz_t2_h1_pval_adj", "waz_t2_h3_pval_adj")

# obj=c("whz_t1_h1_pval_adj", "whz_t1_h3_pval_adj",
#   "whz_t2_h1_pval_adj", "whz_t2_h3_pval_adj")

# obj=c("hcz_t1_h1_pval_adj", "hcz_t1_h3_pval_adj",
#  "hcz_t2_h1_pval_adj", "hcz_t2_h3_pval_adj")

# obj=c("stunt_t1_h1_pr_adj", "stunt_t1_h3_pr_adj",
#   "stunt_t1_h1_rd_adj", "stunt_t1_h3_rd_adj",
#   "stunt_t2_h1_pr_adj", "stunt_t2_h3_pr_adj",
#   "stunt_t2_h1_rd_adj", "stunt_t2_h3_rd_adj")
# obj=c("stunt_t1_h1_pval_adj","stunt_t1_h3_pval_adj",
#      "stunt_t2_h1_pval_adj","stunt_t2_h3_pval_adj")

# obj=c("sstunt_t1_h1_pr_adj", "sstunt_t1_h3_pr_adj",
#   "sstunt_t1_h1_rd_adj", "sstunt_t1_h3_rd_adj",
#   "sstunt_t2_h1_pr_adj", "sstunt_t2_h3_pr_adj",
#   "sstunt_t2_h1_rd_adj", "sstunt_t2_h3_rd_adj")
# obj=c("stuntt_t1_h1_pval_adj","stuntt_t1_h3_pval_adj",
#      "stuntt_t2_h1_pval_adj","stuntt_t2_h3_pval_adj")

# obj=c("wast_t1_h1_rd_adj", "wast_t1_h3_rd_adj",
#       "wast_t2_h1_rd_adj", "wast_t2_h3_rd_adj",
#   "wast_t2_h1_pr_adj", "wast_t2_h3_pr_adj",
#   "wast_t2_h1_rd_adj", "wast_t2_h3_rd_adj")

# obj=c("underwt_t1_h1_rd_adj", "underwt_t1_h3_rd_adj",
#       "underwt_t2_h1_rd_adj", "underwt_t2_h3_rd_adj",
#       "underwt_t1_h1_pr_adj", "underwt_t1_h3_pr_adj",
#       "underwt_t2_h1_rd_adj", "underwt_t2_h3_rd_adj")



# obj="table1"

# load objects
# dir=getwd()
# for(i in 1:length(obj)){
#   load(paste(dir,"/jade/",obj[i],".RData",sep=""))
#   load(paste(dir,"/ben/",obj[[i]],".RData",sep=""))
# }

#obj=c("uptake_table")


# load everything in each directory
dir=getwd()
jade.res <- list.files(path=paste(dir,"/jade/",sep=""))
for(i in 1:length(jade.res)) {
  load(paste(dir,"/jade/",jade.res[i],sep=""))
}
and.res <- list.files(path=paste(dir,"/andrew/",sep=""))
for(i in 1:length(and.res)) {
  load(paste(dir,"/andrew/",and.res[i],sep=""))
}


# compare objects
compare(obj,"_j","_A")


