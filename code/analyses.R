#Produce analyses described in Walter et al. (in prep) Title TBD

rm(list=ls())

#if necessary, install hotspomoments R package containing analysis functions
if(!require(hotspomoments)){
  library(devtools)
  install_github("jonathan-walter/hotspomoments", force=TRUE)
}
library(hotspomoments)
library(tidyverse)
library(fields)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load datasets ------------------------------------------------------------------------------------

#soil CO2: <https://figshare.com/articles/dataset/Soil_CO2_pore_space_concentrations/20320089/1>
canaan.raw<-read.csv("../data/Canaan_gas_well_data_2010_2012.csv") 
#seagrass metabolism: <ADD URL>
cayman.raw<-read.csv("../data/Cayman_seagrass_metabolism.csv")
#lake DO saturation
source("getOrtizSwanLakeData.R")


#--------------------------------------------------------------------------------------------------
#main case study analyses -------------------------------------------------------------------------


# soil CO2 (Canaan valley, West Virginia, USA) ----------------------------------------------------

#5 cm wells
canaan5<-canaan.raw[!is.na(canaan.raw$X5cm),]

hist(canaan5$X5cm)
hshmtest_canaan5<-hshmtest(canaan5$X5cm, stat="skewness") #very skewy.
hshmid_canaan5cm_ref.normal<-hshmid(canaan5$X5cm, side="upper", criteria = "ref.normal", thresh=0.95)
hshmid_canaan5cm_reduce.skew<-hshmid(canaan5$X5cm, criteria="reduce.skew", 
                                     thresh = quantile(hshmtest_canaan5$surr, 0.95))

sum(hshmid_canaan5cm_ref.normal)
sum(hshmid_canaan5cm_reduce.skew)

hist(canaan5$X5cm)
points(canaan5$X5cm[hshmid_canaan5cm_ref.normal], 
       rep(0,sum(hshmid_canaan5cm_ref.normal)), pch=19, col="red")
points(canaan5$X5cm[hshmid_canaan5cm_reduce.skew], 
       rep(5,sum(hshmid_canaan5cm_reduce.skew)), pch=1, col="red")

canaan5.hshm <- canaan5[hshmid_canaan5cm_ref.normal,]

table(canaan5.hshm$DATE)
table(canaan5.hshm$ELEV, canaan5.hshm$VEG)
table(paste0(canaan5.hshm$ELEV, canaan5.hshm$PLOT))


#20 cm wells
canaan20<-canaan.raw[!is.na(canaan.raw$X20cm),]

hist(canaan20$X20cm)
hshmtest_canaan20<-hshmtest(canaan20$X20cm, stat="skewness") #very skewy.
hshmid_canaan20cm_ref.normal<-hshmid(canaan20$X20cm, side="upper", criteria = "ref.normal", thresh=0.95)
#hshmid_canaan20cm_reduce.skew<-hshmid(canaan20$X20cm, criteria="reduce.skew", 
#                                     thresh = quantile(hshmtest_canaan20$surr, 0.95))

sum(hshmid_canaan20cm_ref.normal)
#sum(hshmid_canaan20cm_reduce.skew)

hist(canaan20$X20cm)
points(canaan20$X20cm[hshmid_canaan20cm_ref.normal], 
       rep(0,sum(hshmid_canaan20cm_ref.normal)), pch=19, col="red")
# points(canaan20$X20cm[hshmid_canaan20cm_reduce.skew], 
#        rep(5,sum(hshmid_canaan20cm_reduce.skew)), pch=1, col="red")

canaan20.hshm <- canaan20[hshmid_canaan20cm_ref.normal,]

table(canaan20.hshm$DATE)
table(canaan20.hshm$ELEV, canaan20.hshm$VEG)
table(paste0(canaan20.hshm$ELEV, canaan20.hshm$PLOT))



## Cayman islands seagrass NEP --------------------------------------------------------------------
cayman <- cayman.raw
colnames(cayman)[5:8] <- c("biomass", "GPP", "Re", "NEP")
cayman <- cayman[!is.na(cayman$NEP), ]

# Exclude the naturally grazed and ungrazed plots; use only the 10 experimental plots
cayman <- cayman[!grepl("graze", cayman$Treatment),]


# NEP
hist(cayman$NEP)
hshmtest(cayman$NEP, stat="skewness")  # not significantly skewed, but somewhat right-skewed
hshmtest(cayman$NEP, stat="kurtosis")  # no excess kurtosis, somewhat platykurtic

hshmid_caymanNEP <- hshmid(cayman$NEP, criteria="ref.normal", side="both", thresh=0.95)

hist(cayman$NEP)
points(cayman$NEP[hshmid_caymanNEP], rep(0, sum(hshmid_caymanNEP)), pch=19, col="red")

cayman.hshm <- cayman[hshmid_caymanNEP, ]
print(cayman.hshm)

# Add HSHM IDs to data set for viewing
cayman_hshm = bind_cols(cayman, as_tibble(hshmid_caymanNEP))
colnames(cayman_hshm)[9] <- c("hshm_NEP")


# plotting
tt <- unique(cayman$Sample.Week)
cayman.ref <- unique(cayman$Plot[cayman$Treatment=="reference"])
cayman.trt <- unique(cayman$Plot[cayman$Treatment=="clipped"])

ref.mat <- matrix(NA, nrow=length(cayman.ref), ncol=length(tt))
trt.mat <- matrix(NA, nrow=length(cayman.ref), ncol=length(tt))


for(ii in 1:length(cayman.ref)){
  for(jj in 1:length(tt)){
    if(any(cayman$Treatment=="reference" & cayman$Plot==cayman.ref[ii] & cayman$Sample.Week==tt[jj])){
      ref.mat[ii,jj] <- cayman$NEP[cayman$Treatment=="reference" & cayman$Plot==cayman.ref[ii] & cayman$Sample.Week==tt[jj]]
    }
    if(any(cayman$Treatment=="clipped" & cayman$Plot==cayman.trt[ii] & cayman$Sample.Week==tt[jj])){
      trt.mat[ii,jj] <- cayman$NEP[cayman$Treatment=="clipped" & cayman$Plot==cayman.trt[ii] & cayman$Sample.Week==tt[jj]]
    }
  }
}

cay.mat <- rbind(ref.mat, trt.mat)

pal<-colorRampPalette(c("red","grey85","blue"))


xx <- seq(-200,350,by=1)


pdf("../outputs/Fig2_caymanNEP.pdf", width=6.5, height=4)

par(mar=c(3.5,3.5,1.1,1.1), mgp=c(2.25,0.6,0), mfrow=c(1,2), oma=c(0,0,0,1.5))

hist(c(cay.mat), freq=FALSE, main="", xlim=c(-200,350),
     xlab=expression(NEP~(mmol~C~m^-2~d^-1)))
lines(xx, dnorm(xx, mean(cay.mat, na.rm=T), sd(cay.mat, na.rm=T)), lwd=1.5, lty=2)
points(x=sort(c(cay.mat))[1:2], y=c(0,0), col="red", pch=16)
points(x=sort(c(cay.mat), decreasing=TRUE)[1:2], y=c(0,0), col="blue", pch=16)
mtext("a)", at=par("usr")[1]-0.05*diff(par("usr")[1:2]))

par(mar=c(3.5,3.1,1.1,1.1), mgp=c(2.2,0.6,0))

image.plot(t(cay.mat), col=pal(100), zlim=c(-max(cay.mat, na.rm=T), max(cay.mat, na.rm=T)),
           xaxt="n", yaxt="n", xlab="Sampling week", legend.shrink=0.8, legend.cex=0.8)
axis(2, at=seq(0,1,length.out=10), labels=c(2,4,6,8,10,1,3,5,7,9))#labels=c(NA,NA,"Reference",NA,NA,NA,NA,"Clipped",NA,NA))
axis(1, at=seq(0,1,length.out=10), labels=1:10)
abline(h=0.5, lwd=2)
points(x=c(8/9,8/9,5/9,6/9), y=c(3/9,6/9,4/9,4/9), pch="*", cex=2)
mtext("Reference", side=2, at=0.25, line=2.25)
mtext("Clipped", side=2, at=0.75, line=2.25)
mtext("Plot Number", side=2, line=3.5)
mtext("b)", at=par("usr")[1]-0.05*diff(par("usr")[1:2]))

dev.off()

# Swan Lake (Iowa) Dissolved Oxygen ---------------------------------------------------------------

swan<-swan.raw[!is.na(swan.raw$DissolvedOxygen_Saturation),]

hist(swan$DissolvedOxygen_Saturation)
hshmtest(swan$DissolvedOxygen_Saturation, stat="skewness")
hshmid_swanDO<-hshmid(swan$DissolvedOxygen_Saturation, criteria = "ref.normal", side="upper", thresh=0.95)

hist(swan$DissolvedOxygen_Saturation)
points(swan$DissolvedOxygen_Saturation[hshmid_swanDO], rep(0,sum(hshmid_swanDO)), pch=19, col="red")

swan[hshmid_swanDO,]


# -------------------------------------------------------------------------------------------------
# Sensitivity to losing observations -- based on Swan Lake DOsat ----------------------------------

## Begin dropping by site

max.drop<-floor(length(unique(swan$Site))*2/3)
max.reps<-100
drop.by<-as.character(unique(swan$Site))

nn<-NULL
skew<-NULL
pval<-NULL
hshmTP <- NULL
hshmFP <- NULL
hshmobs<-list()

orig.inds <- 1:nrow(swan)
orig.hshm <- orig.inds[hshmid_swanDO]

for(n.drop in 1:max.drop){
  
  if(choose(length(drop.by), n.drop) > max.reps){
    nreps = max.reps
  }
  else{nreps = choose(length(drop.by), n.drop)}
  
  for(rep in 1:nreps){
    
    drop<-sample(drop.by, n.drop, replace=FALSE)
    orig.inds.ii <- orig.inds[!swan$Site %in% drop]
    
    tmpdat<-swan[!swan$Site %in% drop,]
    tmpHSHMtest<-hshmtest(tmpdat$DissolvedOxygen_Saturation, stat="skewness")
    tmpHSHMid<-hshmid(tmpdat$DissolvedOxygen_Saturation, criteria = "ref.normal",  side="upper", thresh=0.95)
    
    nn<-c(nn, n.drop)
    skew<-c(skew, tmpHSHMtest$emp)
    pval<-c(pval, tmpHSHMtest$quantile)
    hshmTP <- c(hshmTP, sum(orig.inds.ii[tmpHSHMid] %in% (orig.hshm[orig.hshm %in% orig.inds.ii]))/
      sum(orig.hshm %in% orig.inds.ii))
    hshmFP <- c(hshmFP, sum(!orig.inds.ii[tmpHSHMid] %in% (orig.hshm[orig.hshm %in% orig.inds.ii]))/
                  sum(orig.hshm %in% orig.inds.ii))
    hshmobs<-c(hshmobs, tmpdat[tmpHSHMid,])
  }
  
}
hist(skew) #skewnesss changes +/- 0.1
quantile(skew, c(0.025, 0.975))
hist(pval) #pval doesn't change
hist(hshmTP)
quantile(hshmTP, c(0.025, 0.975))
hist(hshmFP)
quantile(hshmFP, c(0.025, 0.975))

plot(nn, hshmTP)
plot(nn, hshmFP)

sensStats.space <- data.frame(nn=nn, skew=skew, pval=pval, hshmTP=hshmTP, hshmFP=hshmFP)

plot(unique(nn), aggregate(hshmTP~nn, FUN="mean")$hshmTP, type="l", ylim=c(0.9,1), lwd=2,
     xlab="Sites dropped", ylab="HSHM ID agreement")
lines(unique(nn), aggregate(hshmTP~nn, FUN=function(x){quantile(x,0.95)})$hshmTP, lty=2)
lines(unique(nn), aggregate(hshmTP~nn, FUN=function(x){quantile(x,0.05)})$hshmTP, lty=2)

plot(unique(nn), aggregate(hshmFP~nn, FUN="mean")$hshmFP, type="l", ylim=c(0,0.15), lwd=2,
     xlab="Sites dropped", ylab="HSHM ID disagreement")
lines(unique(nn), aggregate(hshmFP~nn, FUN=function(x){quantile(x,0.95)})$hshmFP, lty=2)
lines(unique(nn), aggregate(hshmFP~nn, FUN=function(x){quantile(x,0.05)})$hshmFP, lty=2)

#now dropping by date

max.drop<-floor(length(unique(swan$DOY))*2/3)
max.reps<-100
drop.by<-as.character(unique(swan$DOY))

nn<-NULL
skew<-NULL
pval<-NULL
hshmTP <- NULL
hshmFP <- NULL
hshmobs<-list()

orig.inds <- 1:nrow(swan)
orig.hshm <- orig.inds[hshmid_swanDO]

for(n.drop in 1:max.drop){
  
  if(choose(length(drop.by), n.drop) > max.reps){
    nreps = max.reps
  }
  else{nreps = choose(length(drop.by), n.drop)}
  
  for(rep in 1:nreps){
    drop<-sample(drop.by, n.drop, replace=FALSE)
    orig.inds.ii <- orig.inds[!swan$Site %in% drop]
    
    tmpdat<-swan[!swan$DOY %in% drop,]
    tmpHSHMtest<-hshmtest(tmpdat$DissolvedOxygen_Saturation, stat="skewness")
    tmpHSHMid<-hshmid(tmpdat$DissolvedOxygen_Saturation, criteria = "ref.normal", side="upper", thresh=0.95)
    
    nn<-c(nn, n.drop)
    skew<-c(skew, tmpHSHMtest$emp)
    pval<-c(pval, tmpHSHMtest$quantile)
    hshmTP <- c(hshmTP, sum(orig.inds.ii[tmpHSHMid] %in% (orig.hshm[orig.hshm %in% orig.inds.ii]))/
                  sum(orig.hshm %in% orig.inds.ii))
    hshmFP <- c(hshmFP, sum(!orig.inds.ii[tmpHSHMid] %in% (orig.hshm[orig.hshm %in% orig.inds.ii]))/
                  sum(orig.hshm %in% orig.inds.ii))
    hshmobs<-c(hshmobs, tmpdat[tmpHSHMid,])
  }
  
}

hist(skew) ##biggggg differences in skew
quantile(skew, c(0.025, 0.975))
hist(pval) ##biggggg changes in pvalue possible
mean(pval>0.95) ##but these changes are rare! > 96% of the time we say there are positive HSHM
hist(hshmTP)
quantile(hshmTP, c(0.025,0.5,0.975))
hist(hshmFP)
quantile(hshmFP, c(0.025,0.5,0.975))

plot(nn, hshmTP)


sensStats.time <- data.frame(nn=nn, skew=skew, pval=pval, hshmTP=hshmTP, hshmFP=hshmFP)

plot(unique(nn), aggregate(hshmTP~nn, FUN="mean")$hshmTP, type="l", ylim=c(0,1), lwd=2,
     xlab="Sampling dates dropped", ylab="HSHM ID agreement")
lines(unique(nn), aggregate(hshmTP~nn, FUN=function(x){quantile(x,0.95)})$hshmTP, lty=2)
lines(unique(nn), aggregate(hshmTP~nn, FUN=function(x){quantile(x,0.05)})$hshmTP, lty=2)

plot(unique(nn), aggregate(hshmFP~nn, FUN="mean")$hshmFP, type="l", ylim=c(0,1.5), lwd=2,
     xlab="Sampling dates dropped", ylab="HSHM ID disagreement")
lines(unique(nn), aggregate(hshmFP~nn, FUN=function(x){quantile(x,0.95)})$hshmFP, lty=2)
lines(unique(nn), aggregate(hshmFP~nn, FUN=function(x){quantile(x,0.05)})$hshmFP, lty=2)


pdf("../outputs/fig5_sensitivity.pdf", width=6.5, height=8)

par(mfcol=c(4,2), mar=c(3.5,3.5,1.1,1.1), mgp=c(2,0.8,0), oma=c(0,0,1.1,0))

hist(sensStats.space$skew, main="", xlab="Skewness")
qq <- par("usr")
text(qq[1]+0.05*diff(qq[1:2]), qq[4]-0.05*diff(qq[3:4]), "a)")
hist(sensStats.space$pval, main="", xlab="Quantile", breaks=seq(0,1,0.05))
qq <- par("usr")
text(qq[1]+0.05*diff(qq[1:2]), qq[4]-0.05*diff(qq[3:4]), "b)")
plot(unique(sensStats.space$nn), aggregate(hshmTP~nn, data=sensStats.space, FUN="mean")$hshmTP, type="l", ylim=c(0.9,1), lwd=2,
     xlab="Locations dropped", ylab="HSHM ID agreement")
lines(unique(sensStats.space$nn), aggregate(hshmTP~nn, data=sensStats.space, FUN=function(x){quantile(x,0.95)})$hshmTP, lty=2)
lines(unique(sensStats.space$nn), aggregate(hshmTP~nn, data=sensStats.space, FUN=function(x){quantile(x,0.05)})$hshmTP, lty=2)
qq <- par("usr")
text(qq[1]+0.05*diff(qq[1:2]), qq[4]-0.2*diff(qq[3:4]), "c)")

plot(unique(sensStats.space$nn), aggregate(hshmFP~nn, data=sensStats.space, FUN="mean")$hshmFP, type="l", ylim=c(0,0.15), lwd=2,
     xlab="Locations dropped", ylab="HSHM ID disagreement")
lines(unique(sensStats.space$nn), aggregate(hshmFP~nn, data=sensStats.space, FUN=function(x){quantile(x,0.95)})$hshmFP, lty=2)
lines(unique(sensStats.space$nn), aggregate(hshmFP~nn, data=sensStats.space, FUN=function(x){quantile(x,0.05)})$hshmFP, lty=2)
qq <- par("usr")
text(qq[1]+0.05*diff(qq[1:2]), qq[4]-0.1*diff(qq[3:4]), "d)")

hist(sensStats.time$skew, main="", xlab="Skewness")
qq <- par("usr")
text(qq[1]+0.05*diff(qq[1:2]), qq[4]-0.05*diff(qq[3:4]), "e)")
hist(sensStats.time$pval, main="", xlab="Quantile", breaks=seq(0,1,0.05))
qq <- par("usr")
text(qq[1]+0.05*diff(qq[1:2]), qq[4]-0.05*diff(qq[3:4]), "f)")
plot(unique(sensStats.time$nn), 
     aggregate(hshmTP~nn, data=sensStats.time, FUN="mean")$hshmTP, type="l", ylim=c(0,1), lwd=2,
     xlab="Dates dropped", ylab="HSHM ID agreement")
lines(unique(sensStats.time$nn), aggregate(hshmTP~nn, data=sensStats.time, FUN=function(x){quantile(x,0.95)})$hshmTP, lty=2)
lines(unique(sensStats.time$nn), aggregate(hshmTP~nn, data=sensStats.time, FUN=function(x){quantile(x,0.05)})$hshmTP, lty=2)
qq <- par("usr")
text(qq[1]+0.05*diff(qq[1:2]), qq[4]-0.2*diff(qq[3:4]), "g)")

plot(unique(sensStats.time$nn), 
     aggregate(hshmFP~nn, data=sensStats.time, FUN="mean")$hshmFP, type="l", ylim=c(0,1.5), lwd=2,
     xlab="Dates dropped", ylab="HSHM ID disagreement")
lines(unique(sensStats.time$nn), aggregate(hshmFP~nn, data=sensStats.time, FUN=function(x){quantile(x,0.95)})$hshmFP, lty=2)
lines(unique(sensStats.time$nn), aggregate(hshmFP~nn, data=sensStats.time, FUN=function(x){quantile(x,0.05)})$hshmFP, lty=2)
qq <- par("usr")
text(qq[1]+0.05*diff(qq[1:2]), qq[4]-0.1*diff(qq[3:4]), "h)")

mtext("Dropping sites", outer=TRUE, at=0.25, cex=2/3)
mtext("Dropping sampling dates", outer=TRUE, at=0.75, cex=2/3)

dev.off()

