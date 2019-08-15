##DESCRIPTION
#runs full thinned dataset 

#DATE: 8/13/2019


#HISTORY: Model_RF_call.function.shrub.allThin.5prev.R



##LIBRARIES
library(randomForest)
library(PresenceAbsence)
library(dplyr)

rm(list=ls())

##SET DIRECTORIES
in.dir <- "~/Project_Uconn/One/OneAnalysis/input/"
out.dir <- "~/Project_Uconn/One/OneAnalysis/output/"
r.dir <- "~/Project_Uconn/One/OneAnalysis/r/"


##TEST DATA
load(paste0(in.dir,"covs.Askins.200m.prop.v2.RData"))

#TRAINING DATA 
load(paste0(in.dir,"AskinsDates.thinOnly.eBird.expertise.RData"))

#5 PREVALENCE
colnames(covariates.prop.200.test)#which columns birds?
birds <- covariates.prop.200.test[,53:ncol(covariates.prop.200.test)]
birds <- ifelse(birds>0,1,0)
birds5 <-birds[,which((colSums(birds)/nrow(birds))>.049)]
birds.5prev <- colnames(birds5)
birds.5prev <- birds.5prev[!grepl(pattern="NA",birds.5prev)]
birds.5prev <- birds.5prev[complete.cases(birds.5prev)]
birds.5prev[birds.5prev %in% 'Eastern.Wood.pewee'] <- "Eastern.Wood.Pewee"


#COVARIATES; Same for all training data subsets
load(paste0(in.dir,"covs.eBirdSummerMorning.n14076.200m.prop.RData"))#now this is n12956 (11/6/2017) because in 'landcover.proportions.R' I removed rows where sums were 0, i.e., no covariates/over ocean


#COVARIATE NAMES
cov.names.askins <- c("est","rowname","DURATION.MINUTES","EFFORT.DISTANCE.KM","TIME.OBSERVATIONS.STARTED.dec.hr","dev_vlow","dev_low","dev_med", "dev_high","decid","evergr","mixed","grass", "shrub", "past.hay","crops", "Lake","Pond","EmergentWL","ForestShrubWL","patch","perforated","edge","core","elev","eness","nness","slope")                  

source(paste0(r.dir,"Models_RF_function.allThin.R"))

##RUN MODELS
askins <- run.models(covariates=covariates.prop.200,cov.names=cov.names.askins,bird.names=birds.5prev,training.list=dat.u.thin,test.dat=covariates.prop.200.test)

#save it
save.name <- "askins.200mTrain.200mTest.allThin"
save(askins,file=paste0(out.dir,save.name,".RData",sep=""))

