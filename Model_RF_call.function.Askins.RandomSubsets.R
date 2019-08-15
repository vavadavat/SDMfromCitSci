##DESCRIPTION
#runs subsets based on random selection 10 times

#DATE: 8/13/2019

#HISTORY: Model_RF_call.function.shrub.RandomSubsets.5prev.R

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
colnames(covariates.prop.200.test)#which columns with birds
birds <- covariates.prop.200.test[,53:ncol(covariates.prop.200.test)]
birds <- ifelse(birds>0,1,0)
birds5 <-birds[,which((colSums(birds)/nrow(birds))>.049)]
birds.5prev <- colnames(birds5)
birds.5prev <- birds.5prev[!grepl(pattern="NA",birds.5prev)]
birds.5prev <- birds.5prev[complete.cases(birds.5prev)]
birds.5prev[birds.5prev %in% 'Eastern.Wood.pewee'] <- "Eastern.Wood.Pewee"

##SAMPLE SIZE OF SUBSETS
load(paste0(out.dir,"dat.sample.size.eBird.AskinsDates.RData"))

#COVARIATES; Same for all training data subsets
load(paste0(in.dir,"covs.eBirdSummerMorning.n14076.200m.prop.RData"))#now this is n12956 (11/6/2017) because in 'landcover.proportions.R' I removed rows where sums were 0, i.e., no covariates/over ocean

#COVARIATE NAMES
cov.names.askins <- c("est","rowname","DURATION.MINUTES","EFFORT.DISTANCE.KM","TIME.OBSERVATIONS.STARTED.dec.hr","dev_vlow","dev_low","dev_med", "dev_high","decid","evergr","mixed","grass", "shrub", "past.hay","crops", "Lake","Pond","EmergentWL","ForestShrubWL","patch","perforated","edge","core","elev","eness","nness","slope")                  

source(paste0(r.dir,"Models_RF_function.Subsets.R"))

##LOOP OVER 10 LISTS OF RANDOM SUBSETS
askins.list <- list()
for(p in 1:10){
  ##CREATE LIST OF RANDOMLY SAMPLED ROWS FROM THINNED DATASET TO MATCH SAMPLE SIZES OF CULLED SUBSETS
  group.subset.thin <- list()
  for (l in 1:nrow(dat.sample.size.eBird.askins.dates)){
    group.subset.thin[[l]] <- dat.u.thin[sample(nrow(dat.u.thin), dat.sample.size.eBird.askins.dates[l,1]),]
  }
  #RUN FUNCTION
  shrub.askins <- run.models(covariates=covariates.prop.200,cov.names=cov.names.askins,bird.names=birds.5prev,training.list=group.subset.thin,test.dat=covariates.prop.200.test)
  tmp <- lapply(shrub.askins,`[`,7,)#just AUC row
  names(tmp) <- dat.sample.size.eBird.askins.dates[,2]
  askins.auc <- do.call(rbind, lapply(lapply(tmp, unlist), "[", #this should work as replacement for list.rbind
                                           unique(unlist(c(sapply(tmp,names))))))
  colnames(askins.auc) <- unique(unlist(c(sapply(tmp,names))))
  
  askins.list[[p]] <- askins.auc#each randomization because element in list
 }

#save it
save.name <- "Askins.200mTrain.200mTest.RandomSubsets"
save(askins.list,file=paste0(out.dir,save.name,".RData",sep=""))


