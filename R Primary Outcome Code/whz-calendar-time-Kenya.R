#---------------------------------
# diar-calendar-time.R
#
# summarize diarrhea prevalence
# by calendar time
#---------------------------------

#---------------------------------------
# preamble
#---------------------------------------
rm(list=ls())
library(washb)
library(lubridate)
library(scales)
library(dplyr)

#---------------------------------------
# Load the analysis dataset
#---------------------------------------
setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched/tr")
treatment<-read.csv("washb-kenya-tr.csv")


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew")
d<-read.csv("washb-kenya-anthro.csv",stringsAsFactors = T)
HHtracking<-read.csv("washb-kenya-tracking.csv",stringsAsFactors = T)

HHtracking<-subset(HHtracking, select=c("hhid","ms_ml_ad_date","ms_el_ad_date"))

#---------------------------------------
# Merge survey dates with diarrhea information
#---------------------------------------
ad<-merge(d, HHtracking, by=c("hhid"),all.x=T,all.y=F)


#---------------------------------------
# Subset the Data to Follow-up data only
#---------------------------------------
table(ad$studyyear)
ad <- subset(ad,studyyear>0)

#---------------------------------------
# Exclude:
# * siblings who were born after enrollment
# * siblings who were >36 mos at enrollment
# * children with missing outcome data
#---------------------------------------
#subset the diarrhea to children <36 mos at enrollment
#or target children
table(is.na(d$whz))
ad <-
  ad %>%
  filter(!is.na(whz)& whz_x!=1) 
table(is.na(ad$whz) )


#assign survey date based on survey year:
ad$ms_ml_am_date<-as.character(ad$ms_ml_am_date)
ad$ms_el_am_date<-as.character(ad$ms_el_am_date)
ad$svydate<-ad$ms_ml_am_date
#ad[ad$studyyear==1,"svydate"]<-ad[ad$studyyear==1,"ms_ml_ad_date"] 
ad[ad$studyyear==2,"svydate"]<-ad[ad$studyyear==2,"ms_el_am_date"]


ad$svydate<-as.Date(ad$svydate,format="%d%b%Y")
table(is.na(ad$svydate))



# re-order the tr factor for convenience
ad$tr <- factor(ad$tr,levels=c("Water","Sanitation","Handwashing","Nutrition","WSH","Nutrition + WSH","Control", "Passive Control"))


#---------------------------------------
# create a calendar time aggregator
# by month
#---------------------------------------
#ad$caldate <- as.Date(as.character(ad$svydate),format="%d%b%Y")
ad$caldate <- ad$svydate
ad$caldate <- as.Date(ad$svydate,format="%d%b%Y")

head(ad$caldate)
ad$my <- floor_date(ad$caldate,"month")



#---------------------------------------
# control monthly means w/ robust 95% SEs
#---------------------------------------
mys <- unique(ad$my)
diar_c <- sapply(mys,
                 function(x) {
                   # washb_mean(ad$whz[(ad$tr=="Control"| ad$tr=="Passive Control")  & ad$my==x],
                   #            id=ad$block[(ad$tr=="Control"|ad$tr=="Passive Control") & ad$my==x],
                   #            print=F
               washb_mean(ad$whz[ad$tr!="Nutrition" & ad$tr!="Nutrition + WSH" & ad$my==x],
                              id=ad$block[ad$tr!="Nutrition" & ad$tr!="Nutrition + WSH"  & ad$my==x],
                              print=F
                              )
                 }
                 )
diar_c <- data.frame(t(diar_c))
diar_c$month <- as.Date(mys)
colnames(diar_c) <- c("n","mean","sd","se","lb","ub","month")
diar_c <- diar_c[order(diar_c$month),c("month","n","mean","sd","se","lb","ub")]


  diar_c<-diar_c[2:nrow(diar_c),]  #Drop first outlier point
  xtics <- unique(diar_c$month) #Save every month 
  diar_c<-diar_c[diar_c$n>=100,]  #Drop dates with <100 points

# summarize dist of obs per month
summary(diar_c$n)

#---------------------------------------
# intervention (all arms) monthly means w/ robust 95% SEs
#---------------------------------------
mys <- unique(ad$my)
diar_i <- sapply(mys,
                 function(x) {
                   # washb_mean(ad$whz[ad$tr!="Control" & ad$tr!="Passive Control" & ad$my==x],
                   #            id=ad$block[ad$tr!="Control" & ad$tr!="Passive Control" & ad$my==x],
                   #            print=F
                                     washb_mean(ad$whz[(ad$tr=="Nutrition" | ad$tr=="Nutrition + WSH") & ad$my==x],
                              id=ad$block[(ad$tr=="Nutrition" | ad$tr=="Nutrition + WSH")  & ad$my==x],
                              print=F
                   )
                 }
)
diar_i <- data.frame(t(diar_i))
diar_i$month <- as.Date(mys)
colnames(diar_i) <- c("n","mean","sd","se","lb","ub","month")
diar_i <- diar_i[order(diar_i$month),c("month","n","mean","sd","se","lb","ub")]

  diar_i<-diar_i[2:nrow(diar_i),]  #Drop first outlier point
  diar_i<-diar_i[diar_i$n>=100,]  #Drop dates with <100 points

# summarize dist of obs per month
summary(diar_i$n)


#---------------------------------------
# make plot in black and white (no CIs)
#---------------------------------------
pdf("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Figures/DiarByMonth_Kenya-bw-noci.pdf",width=6,height=3)
# general plotting parameters
op <- par(mar=c(4,4,2,1)+0.1)
ytics <- c(-0.2,0,0.2,0.4, 0.6, 0.8)
xtics<-c(xtics[1:9],as.Date("2015-07-01"),xtics[10:21])
xtics2 <- as.Date(c("2014-11-01","2015-06-01","2016-02-01"))
xtics3 <- as.Date(c("2015-03-01","2016-02-01"))
#cblue <- "#3366AA"
#corange <- "#EEA722"
cblue <- "black"
corange <- "black"
cols <- c(corange,cblue)
# empty plot
plot(diar_c$month[diar_c$month<="2015-07-01"],
     diar_c$mean[diar_c$month<="2015-07-01"],
     type="n",
     ylim=range(ytics),ylab="Weight-for-height Zscore",yaxt="n",
     xlim=range(xtics),xlab="",xaxt="n",
     bty="n",las=1)
axis(1,at=xtics,las=2,labels=format(xtics,"%m"),cex.axis=0.6)
#axis(1,at=xt,las=2,labels=xlabs,cex.axis=0.6)
axis(2,at=ytics,las=1,labels=sprintf("%1.1f",ytics))
mtext(format(xtics2,"%Y"),at=xtics2,side=1,line=1.5,cex=0.75)
mtext(c("Year 1 follow-up","Year 2 follow-up"),at=xtics3,side=1,line=2.5,cex=0.75)
legend("topright",c("Control and WSH arms","Nutrition intervention arms"),ncol=1,lty=c(1,2),col=cols,bty="n",cex=0.75)

weight<-NULL
# control years 1 and 2 lines + points
lines(diar_c$month[diar_c$month<="2015-07-01"],diar_c$mean[diar_c$month<="2015-07-01"],lwd=weight ,lty=1,col=cols[1])
#points(diar_c$month[diar_c$month<="2015-07-01"],diar_c$mean[diar_c$month<="2015-07-01"],col=cols[1],pch=19,cex=0.5)

lines(diar_c$month[diar_c$month>"2015-07-01"],diar_c$mean[diar_c$month>"2015-07-01"],lwd=weight,lty=1,col=cols[1])
#points(diar_c$month[diar_c$month>"2015-07-01"],diar_c$mean[diar_c$month>"2015-07-01"],col=cols[1],pch=19,cex=0.5)

# intervention years 1 and 2 lines + points
lines(diar_i$month[diar_i$month<="2015-07-01"],diar_i$mean[diar_i$month<="2015-07-01"],lwd=weight,lty=2,col=cols[2])
#points(diar_i$month[diar_i$month<="2015-07-01"],diar_i$mean[diar_i$month<="2015-07-01"],col=cols[2],pch=19,cex=0.5)

lines(diar_i$month[diar_i$month>"2015-07-01"],diar_i$mean[diar_i$month>"2015-07-01"],lwd=weight,lty=2,col=cols[2])
#points(diar_i$month[diar_i$month>"2015-07-01"],diar_i$mean[diar_i$month>"2015-07-01"],col=cols[2],pch=19,cex=0.5)


par(op)
dev.off()



#---------------------------------------
# make plot w/ color (no CIs)
#---------------------------------------
pdf("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Figures/WHZByMonth_Kenya-noci.pdf",width=6,height=3)
# general plotting parameters
op <- par(mar=c(4,4,2,1)+0.1)
ytics <- c(-0.2,0,0.2,0.4, 0.6, 0.8)
xtics<-c(xtics[1:9],as.Date("2015-07-01"),xtics[10:21])
xtics2 <- as.Date(c("2014-11-01","2015-06-01","2016-02-01"))
xtics3 <- as.Date(c("2015-03-01","2016-02-01"))
cblue <- "#3366AA"
corange <- "#EEA722"
cols <- c(corange,cblue)
# empty plot
plot(diar_c$month[diar_c$month<="2015-07-01"],
     diar_c$mean[diar_c$month<="2015-07-01"],
     type="n",
     ylim=range(ytics),ylab="Weight-for-height Zscore",yaxt="n",
     xlim=range(xtics),xlab="",xaxt="n",
     bty="n",las=1)
axis(1,at=xtics,las=2,labels=format(xtics,"%m"),cex.axis=0.6)
#axis(1,at=xt,las=2,labels=xlabs,cex.axis=0.6)
axis(2,at=ytics,las=1,labels=sprintf("%1.1f",ytics))
mtext(format(xtics2,"%Y"),at=xtics2,side=1,line=1.5,cex=0.75)
mtext(c("Year 1 follow-up","Year 2 follow-up"),at=xtics3,side=1,line=2.5,cex=0.75)
legend("topright",c("Control and WSH arms","Nutrition intervention arms"),ncol=1,lty=c(1,2),col=cols,bty="n",cex=0.75)

# control years 1 and 2 lines + points
lines(diar_c$month[diar_c$month<="2015-07-01"],diar_c$mean[diar_c$month<="2015-07-01"],lty=1,col=cols[1])
points(diar_c$month[diar_c$month<="2015-07-01"],diar_c$mean[diar_c$month<="2015-07-01"],col=cols[1],pch=19,cex=0.5)

lines(diar_c$month[diar_c$month>"2015-07-01"],diar_c$mean[diar_c$month>"2015-07-01"],lty=1,col=cols[1])
points(diar_c$month[diar_c$month>"2015-07-01"],diar_c$mean[diar_c$month>"2015-07-01"],col=cols[1],pch=19,cex=0.5)

# intervention years 1 and 2 lines + points
lines(diar_i$month[diar_i$month<="2015-07-01"],diar_i$mean[diar_i$month<="2015-07-01"],lty=2,col=cols[2])
points(diar_i$month[diar_i$month<="2015-07-01"],diar_i$mean[diar_i$month<="2015-07-01"],col=cols[2],pch=19,cex=0.5)

lines(diar_i$month[diar_i$month>"2015-07-01"],diar_i$mean[diar_i$month>"2015-07-01"],lty=2,col=cols[2])
points(diar_i$month[diar_i$month>"2015-07-01"],diar_i$mean[diar_i$month>"2015-07-01"],col=cols[2],pch=19,cex=0.5)


par(op)
dev.off()


#---------------------------------------
# make plot w/ color + shaded 95% CIs
#---------------------------------------
pdf("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Figures/WHZByMonth_Kenya-ci.pdf",width=6,height=3)
# general plotting parameters
op <- par(mar=c(4,4,2,1)+0.1)
ytics <- c(-0.2,0,0.2,0.4, 0.6, 0.8)
xtics2 <- as.Date(c("2014-11-01","2015-06-01","2016-02-01"))
xtics3 <- as.Date(c("2015-03-01","2016-02-01"))
cblue <- "#3366AA"
corange <- "#EEA722"
cols <- c(corange,cblue)
# empty plot
plot(diar_c$month[diar_c$month<="2015-07-01"],
     diar_c$mean[diar_c$month<="2015-07-01"],
     type="n",
     ylim=range(ytics),ylab="Weight-for-height Zscore",yaxt="n",
     xlim=range(xtics),xlab="",xaxt="n",
     bty="n",las=1)
  axis(1,at=xtics,las=2,labels=format(xtics,"%m"),cex.axis=0.6)
  axis(2,at=ytics,las=1,labels=sprintf("%1.1f",ytics))
  mtext(format(xtics2,"%Y"),at=xtics2,side=1,line=1.5,cex=0.75)
  mtext(c("Year 1 follow-up","Year 2 follow-up"),at=xtics3,side=1,line=2.5,cex=0.75)
  legend("topright",c("Control and WSH arms","Nutrition intervention arms"),ncol=1,lty=c(1,2),col=cols,bty="n",cex=0.75)
  
# control shaded 95% CIs
  polygon(x=c(diar_c$month[diar_c$month<="2015-07-01"],
              rev(diar_c$month[diar_c$month<="2015-07-01"])),
          y=c(diar_c$lb[diar_c$month<="2015-07-01"],
              rev(diar_c$ub[diar_c$month<="2015-07-01"])),
          col=alpha(cols[1],alpha=0.2),border=NA)
  polygon(x=c(diar_c$month[diar_c$month>"2015-07-01"],
              rev(diar_c$month[diar_c$month>"2015-07-01"])),
          y=c(diar_c$lb[diar_c$month>"2015-07-01"],
              rev(diar_c$ub[diar_c$month>"2015-07-01"])),
          col=alpha(cols[1],alpha=0.2),border=NA)

# intervention shaded 95% CIs
  polygon(x=c(diar_i$month[diar_i$month<="2015-07-01"],
              rev(diar_i$month[diar_i$month<="2015-07-01"])),
          y=c(diar_i$lb[diar_i$month<="2015-07-01"],
              rev(diar_i$ub[diar_i$month<="2015-07-01"])),
          col=alpha(cols[2],alpha=0.2),border=NA)
  polygon(x=c(diar_i$month[diar_i$month>"2015-07-01"],
              rev(diar_i$month[diar_i$month>"2015-07-01"])),
          y=c(diar_i$lb[diar_i$month>"2015-07-01"],
              rev(diar_i$ub[diar_i$month>"2015-07-01"])),
          col=alpha(cols[2],alpha=0.2),border=NA)

# control years 1 and 2 lines + points
lines(diar_c$month[diar_c$month<="2015-07-01"],diar_c$mean[diar_c$month<="2015-07-01"],lty=1,col=cols[1])
points(diar_c$month[diar_c$month<="2015-07-01"],diar_c$mean[diar_c$month<="2015-07-01"],col=cols[1],pch=19,cex=0.5)

lines(diar_c$month[diar_c$month>"2015-07-01"],diar_c$mean[diar_c$month>"2015-07-01"],lty=1,col=cols[1])
points(diar_c$month[diar_c$month>"2015-07-01"],diar_c$mean[diar_c$month>"2015-07-01"],col=cols[1],pch=19,cex=0.5)

# intervention years 1 and 2 lines + points
lines(diar_i$month[diar_i$month<="2015-07-01"],diar_i$mean[diar_i$month<="2015-07-01"],lty=2,col=cols[2])
points(diar_i$month[diar_i$month<="2015-07-01"],diar_i$mean[diar_i$month<="2015-07-01"],col=cols[2],pch=19,cex=0.5)

lines(diar_i$month[diar_i$month>"2015-07-01"],diar_i$mean[diar_i$month>"2015-07-01"],lty=2,col=cols[2])
points(diar_i$month[diar_i$month>"2015-07-01"],diar_i$mean[diar_i$month>"2015-07-01"],col=cols[2],pch=19,cex=0.5)

par(op)
dev.off()













#---------------------------------------
# plot seperated by type of intervention
#---------------------------------------



#---------------------------------------
# intervention W
#---------------------------------------
mys <- unique(ad$my)
treatment<-"Water"
diar_W <- sapply(mys,
                 function(x) {
                   washb_mean(ad$whz[ad$tr==treatment  & ad$my==x],
                              id=ad$block[ad$tr==treatment & ad$my==x],
                              print=F
                   )
                 }
)
diar_W <- data.frame(t(diar_W))
diar_W$month <- as.Date(mys)
colnames(diar_W) <- c("n","mean","sd","se","lb","ub","month")
diar_W <- diar_W[order(diar_W$month),c("month","n","mean","sd","se","lb","ub")]

  diar_W<-diar_W[2:nrow(diar_W),]  #Drop first outlier point
  diar_W<-diar_W[diar_W$n>=10,]  #Drop dates with <10 points

# summarize dist of obs per month
summary(diar_W$n)


#---------------------------------------
# intervention S
#---------------------------------------
treatment<-"Sanitation"
diar_S <- sapply(mys,
                 function(x) {
                   washb_mean(ad$whz[ad$tr==treatment  & ad$my==x],
                              id=ad$block[ad$tr==treatment & ad$my==x],
                              print=F
                   )
                 }
)
diar_S <- data.frame(t(diar_S))
diar_S$month <- as.Date(mys)
colnames(diar_S) <- c("n","mean","sd","se","lb","ub","month")
diar_S <- diar_S[order(diar_S$month),c("month","n","mean","sd","se","lb","ub")]

  diar_S<-diar_S[2:nrow(diar_S),]  #Drop first outlier point
  diar_S<-diar_S[diar_S$n>=10,]  #Drop dates with <10 points

# summarize dist of obs per month
summary(diar_S$n)


#---------------------------------------
# intervention H
#---------------------------------------
treatment<-"Handwashing"
diar_H <- sapply(mys,
                 function(x) {
                   washb_mean(ad$whz[ad$tr==treatment  & ad$my==x],
                              id=ad$block[ad$tr==treatment & ad$my==x],
                              print=F
                   )
                 }
)
diar_H <- data.frame(t(diar_H))
diar_H$month <- as.Date(mys)
colnames(diar_H) <- c("n","mean","sd","se","lb","ub","month")
diar_H <- diar_H[order(diar_H$month),c("month","n","mean","sd","se","lb","ub")]

  diar_H<-diar_H[2:nrow(diar_H),]  #Drop first outlier point
  diar_H<-diar_H[diar_H$n>=10,]  #Drop dates with <10 points

# summarize dist of obs per month
summary(diar_H$n)


#---------------------------------------
# intervention N
#---------------------------------------
treatment<-"Nutrition"
diar_N <- sapply(mys,
                 function(x) {
                   washb_mean(ad$whz[ad$tr==treatment  & ad$my==x],
                              id=ad$block[ad$tr==treatment & ad$my==x],
                              print=F
                   )
                 }
)
diar_N <- data.frame(t(diar_N))
diar_N$month <- as.Date(mys)
colnames(diar_N) <- c("n","mean","sd","se","lb","ub","month")
diar_N <- diar_N[order(diar_N$month),c("month","n","mean","sd","se","lb","ub")]

  diar_N<-diar_N[2:nrow(diar_N),]  #Drop first outlier point
  diar_N<-diar_N[diar_N$n>=10,]  #Drop dates with <10 points

# summarize dist of obs per month
summary(diar_N$n)


#---------------------------------------
# intervention WSH
#---------------------------------------
treatment<-"WSH"
diar_WSH <- sapply(mys,
                 function(x) {
                   washb_mean(ad$whz[ad$tr==treatment  & ad$my==x],
                              id=ad$block[ad$tr==treatment & ad$my==x],
                              print=F
                   )
                 }
)
diar_WSH <- data.frame(t(diar_WSH))
diar_WSH$month <- as.Date(mys)
colnames(diar_WSH) <- c("n","mean","sd","se","lb","ub","month")
diar_WSH <- diar_WSH[order(diar_WSH$month),c("month","n","mean","sd","se","lb","ub")]

  diar_WSH<-diar_WSH[2:nrow(diar_WSH),]  #Drop first outlier point
  diar_WSH<-diar_WSH[diar_WSH$n>=10,]  #Drop dates with <10 points

# summarize dist of obs per month
summary(diar_WSH$n)


#---------------------------------------
# intervention WSH+N
#---------------------------------------
treatment<-"Nutrition + WSH"
diar_WSHN <- sapply(mys,
                 function(x) {
                   washb_mean(ad$whz[ad$tr==treatment  & ad$my==x],
                              id=ad$block[ad$tr==treatment & ad$my==x],
                              print=F
                   )
                 }
)
diar_WSHN <- data.frame(t(diar_WSHN))
diar_WSHN$month <- as.Date(mys)
colnames(diar_WSHN) <- c("n","mean","sd","se","lb","ub","month")
diar_WSHN <- diar_WSHN[order(diar_WSHN$month),c("month","n","mean","sd","se","lb","ub")]

  diar_WSHN<-diar_WSHN[2:nrow(diar_WSHN),]  #Drop first outlier point
  diar_WSHN<-diar_WSHN[diar_WSHN$n>=10,]  #Drop dates with <10 points

# summarize dist of obs per month
summary(diar_WSHN$n)



#---------------------------------------
# intervention WSH+N
#---------------------------------------
cblack <- "#000004FF"
cblue <- "#3366AA"
cteal <- "#11AA99"
cgreen <- "#66AA55"
cchartr <- "#CCCC55"
cmagent <- "#992288"
cred <- "#EE3333"
corange <- "#EEA722"
cyellow <- "#FFEE33"
cgrey <- "#777777"
cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange,N=cred,"WSH+N"=cmagent)



#---------------------------------------
# intervention WSH+N
#---------------------------------------

#Temp
cblue <- "#3366AA"
corange <- "#EEA722"
cols <- c(corange,cblue)

diar_i<-diar_WSHN

pdf("C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/WHZByMonthByIntervention.pdf",width=6,height=3)
# general plotting parameters
op <- par(mar=c(4,4,2,1)+0.1)
ytics <- c(-0.2,0,0.2,0.4, 0.6, 0.8)
xtics2 <- as.Date(c("2014-11-01","2015-06-01","2016-02-01"))
xtics3 <- as.Date(c("2015-03-01","2016-02-01"))


# empty plot
plot(diar_c$month[diar_c$month<="2015-07-01"],
     diar_c$mean[diar_c$month<="2015-07-01"],
     type="n",
     ylim=range(ytics),ylab="Weight-for-height Zscore",yaxt="n",
     xlim=range(xtics),xlab="",xaxt="n",
     bty="n",las=1)
  axis(1,at=xtics,las=2,labels=format(xtics,"%m"),cex.axis=0.6)
  axis(2,at=ytics,las=1,labels=sprintf("%1.1f",ytics))
  mtext(format(xtics2,"%Y"),at=xtics2,side=1,line=1.5,cex=0.75)
  mtext(c("Year 1 follow-up","Year 2 follow-up"),at=xtics3,side=1,line=2.5,cex=0.75)
  legend("topright",c("Control","Intervention (all arms)"),ncol=1,lty=c(1,2),col=cols,bty="n",cex=0.75)
  
# control shaded 95% CIs
  polygon(x=c(diar_c$month[diar_c$month<="2015-07-01"],
              rev(diar_c$month[diar_c$month<="2015-07-01"])),
          y=c(diar_c$lb[diar_c$month<="2015-07-01"],
              rev(diar_c$ub[diar_c$month<="2015-07-01"])),
          col=alpha(cols[1],alpha=0.2),border=NA)
  polygon(x=c(diar_c$month[diar_c$month>"2015-07-01"],
              rev(diar_c$month[diar_c$month>"2015-07-01"])),
          y=c(diar_c$lb[diar_c$month>"2015-07-01"],
              rev(diar_c$ub[diar_c$month>"2015-07-01"])),
          col=alpha(cols[1],alpha=0.2),border=NA)

# intervention shaded 95% CIs
  polygon(x=c(diar_i$month[diar_i$month<="2015-07-01"],
              rev(diar_i$month[diar_i$month<="2015-07-01"])),
          y=c(diar_i$lb[diar_i$month<="2015-07-01"],
              rev(diar_i$ub[diar_i$month<="2015-07-01"])),
          col=alpha(cols[2],alpha=0.2),border=NA)
  polygon(x=c(diar_i$month[diar_i$month>"2015-07-01"],
              rev(diar_i$month[diar_i$month>"2015-07-01"])),
          y=c(diar_i$lb[diar_i$month>"2015-07-01"],
              rev(diar_i$ub[diar_i$month>"2015-07-01"])),
          col=alpha(cols[2],alpha=0.2),border=NA)

# control years 1 and 2 lines + points
lines(diar_c$month[diar_c$month<="2015-07-01"],diar_c$mean[diar_c$month<="2015-07-01"],lty=1,col=cols[1])
points(diar_c$month[diar_c$month<="2015-07-01"],diar_c$mean[diar_c$month<="2015-07-01"],col=cols[1],pch=19,cex=0.5)

lines(diar_c$month[diar_c$month>"2015-07-01"],diar_c$mean[diar_c$month>"2015-07-01"],lty=1,col=cols[1])
points(diar_c$month[diar_c$month>"2015-07-01"],diar_c$mean[diar_c$month>"2015-07-01"],col=cols[1],pch=19,cex=0.5)

# intervention years 1 and 2 lines + points
lines(diar_i$month[diar_i$month<="2015-07-01"],diar_i$mean[diar_i$month<="2015-07-01"],lty=2,col=cols[2])
points(diar_i$month[diar_i$month<="2015-07-01"],diar_i$mean[diar_i$month<="2015-07-01"],col=cols[2],pch=19,cex=0.5)

lines(diar_i$month[diar_i$month>"2015-07-01"],diar_i$mean[diar_i$month>"2015-07-01"],lty=2,col=cols[2])
points(diar_i$month[diar_i$month>"2015-07-01"],diar_i$mean[diar_i$month>"2015-07-01"],col=cols[2],pch=19,cex=0.5)

par(op)
dev.off()
