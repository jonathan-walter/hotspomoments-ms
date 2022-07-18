#Produce analyses described in Walter et al. (in prep) Title TBD

rm(list=ls())

#if necessary, install hotspomoments R package containing analysis functions
if(!require(hotspomoments)){
  library(devtools)
  install_github("jonathan-walter/hotspomoments", force=TRUE)
}
library(hotspomoments)
library(tidyverse)

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
canaan<-canaan.raw[!is.na(canaan.raw$X5cm),]

hist(canaan$X5cm)
hshmtest_canaan<-hshmtest(canaan$X5cm, stat="skewness") #very skewy.
hshmid_canaan5cm_ref.normal<-hshmid(canaan$X5cm, side="upper", criteria = "ref.normal", thresh=0.95)
hshmid_canaan5cm_reduce.skew<-hshmid(canaan$X5cm, criteria="reduce.skew", 
                                     thresh = quantile(hshmtest_canaan$surr, 0.95))

sum(hshmid_canaan5cm_ref.normal)
sum(hshmid_canaan5cm_reduce.skew)

hist(canaan.raw$X5cm)
points(canaan$X5cm[hshmid_canaan5cm_ref.normal], 
       rep(0,sum(hshmid_canaan5cm_ref.normal)), pch=19, col="red")
points(canaan$X5cm[hshmid_canaan5cm_reduce.skew], 
       rep(5,sum(hshmid_canaan5cm_reduce.skew)), pch=1, col="red")

canaan.hshm <- canaan[hshmid_canaan5cm_ref.normal,]

table(canaan.hshm$DATE)
table(canaan.hshm$ELEV, canaan.hshm$VEG)


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
hshmobs<-list()

for(n.drop in 1:max.drop){
  
  if(choose(length(drop.by), n.drop) > max.reps){
    nreps = max.reps
  }
  else{nreps = choose(length(drop.by), n.drop)}
  
  for(rep in 1:nreps){
    drop<-sample(drop.by, n.drop, replace=FALSE)
    
    tmpdat<-swan[!swan$Site %in% drop,]
    tmpHSHMtest<-hshmtest(tmpdat$DissolvedOxygen_Saturation, stat="skewness")
    tmpHSHMid<-hshmid(tmpdat$DissolvedOxygen_Saturation, criteria = "ref.normal",  side="upper", thresh=0.95)
    
    nn<-c(nn, n.drop)
    skew<-c(skew, tmpHSHMtest$emp.skew)
    pval<-c(pval, tmpHSHMtest$quantile)
    hshmobs<-c(hshmobs, tmpdat[tmpHSHMid,])
  }
  
}
hist(skew) #skewnesss changes +/- 0.1
hist(pval) #pval doesn't change


#now dropping by date

max.drop<-floor(length(unique(swan$DOY))*2/3)
max.reps<-100
drop.by<-as.character(unique(swan$DOY))

nn<-NULL
skew<-NULL
pval<-NULL
hshmobs<-list()

for(n.drop in 1:max.drop){
  
  if(choose(length(drop.by), n.drop) > max.reps){
    nreps = max.reps
  }
  else{nreps = choose(length(drop.by), n.drop)}
  
  for(rep in 1:nreps){
    drop<-sample(drop.by, n.drop, replace=FALSE)
    
    tmpdat<-swan[!swan$DOY %in% drop,]
    tmpHSHMtest<-hshmtest(tmpdat$DissolvedOxygen_Saturation, stat="skewness")
    tmpHSHMid<-hshmid(tmpdat$DissolvedOxygen_Saturation, criteria = "ref.normal", side="both", thresh=0.95)
    
    nn<-c(nn, n.drop)
    skew<-c(skew, tmpHSHMtest$emp.skew)
    pval<-c(pval, tmpHSHMtest$quantile)
    hshmobs<-c(hshmobs, tmpdat[tmpHSHMid,])
  }
  
}

hist(skew) ##biggggg differences in skew
hist(pval) ##biggggg changes in pvalue possible
mean(pval>0.95) ##but these changes are rare! > 96% of the time we say there are positive HSHM