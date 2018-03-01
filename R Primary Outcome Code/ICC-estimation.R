
#---------------------------------------
# ICC analysis
#
# andrew mertens (amertens@berkeley.edu)
#
# Calculate inter-cluster correfficient 
# for diarrhea and laz outcomes
#---------------------------------------



###Load in data
rm(list=ls())
library(foreign)
library(tidyverse)
library(washb)
library(psych)
library(irr)


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched/tr")
treatment<-read.csv("washb-kenya-tr.csv")

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew")
enrol<-read.dta("washb-kenya-enrol.dta")
#diar<-read.dta("washb-kenya-diar.dta")
#HHtracking<-read.dta("washb-kenya-tracking.dta")

#enrol<-read.csv("washb-kenya-enrol.csv",stringsAsFactors = T)
diar<-read.csv("washb-kenya-diar.csv",stringsAsFactors = T)
anthro<-read.dta("washb-kenya-anthro.dta")
anthro <- anthro %>% filter(laz_x!=1 & !is.na(laz))

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")

colnames(diar)


# icc(ratings = diar, model="oneway",type = "consistency", unit = "single")
# ICC(sf)

#subset the diarrhea to children <36 mos at enrollment
#or target children
diar <-
  diar %>%
  subset(., dcohort==1) %>%
  subset(., !is.na(.$diar7d))

diar$clusterid <- factor(diar$clusterid)
#diar.mod = lm(diar7d ~ clusterid, data = diar)
diar.mod = glm(diar7d ~ clusterid, data = diar, family="binomial")

diar_anova<-anova(diar.mod)
msc <- diar_anova[1,4]
mse <- diar_anova[2,4]
clust_df <- length(unique(diar$clusterid)) - 1

msc/(msc + mse)
(msc - mse)/(msc + clust_df*mse) 
ICCest(x="clusterid", y="diar7d", data = diar, alpha = 0.05, CI.type = c("THD", "Smith")) 
ICCest(x="clusterid", y="diar7d", data = diar[diar$studyyear==2,], alpha = 0.05, CI.type = c("THD", "Smith")) 



#Drop children missing laz
anthro<-subset(anthro, !is.na(anthro))
dim(anthro)

# Drop children with extreme LAZ values
anthro <- subset(anthro,laz_x!=1)

#Subset to year 2
anthro <- subset(anthro,studyyear==2)


anthro$clusterid <- factor(anthro$clusterid)
anthro.mod = lm(laz ~ clusterid, data = anthro)
anthro_anova<-anova(anthro.mod)
msc <- anthro_anova$`Mean Sq`[1]
mse <- anthro_anova$`Mean Sq`[2]

msc/(msc + mse)

#ICC package
library(ICC)
ICCest(x="clusterid", y="laz", data = anthro, alpha = 0.05, CI.type = c("THD", "Smith")) 

