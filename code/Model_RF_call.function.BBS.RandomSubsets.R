##DESCRIPTION
#runs subsets based on random selection 10 times

#DATE: 8/13/2019

#HISTORY: Model_RF_call.function.woodland.RandomSubsets.5prev.R

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
load(paste0(in.dir,"BBS.dat.all.200m.RData"))
BBS.dat.all <- BBS.dat.all.200m;rm(BBS.dat.all.200m)
BBS.dat.all <- rename(BBS.dat.all,eness=e.ness,nness=n.ness)

#5 PREVALENCE
colnames(BBS.dat.all)
birds <- BBS.dat.all[,38:187]
birds5 <-birds[,which((colSums(birds)/nrow(birds))>.049)]
birds.5prev <- colnames(birds5)

#TRAINING DATA 
load(paste0(in.dir,"BBSDates.thinOnly.eBird.expertise.RData"))

##SAMPLE SIZE OF SUBSETS
load(paste0(out.dir,"dat.sample.size.eBird.BBSDates.RData"))

#COVARIATES; Same for all training data subsets
load(paste0(in.dir,"covs.eBirdSummerMorning.n14076.200m.prop.RData"))#now this is n12956 (11/6/2017) because in 'landcover.proportions.R' I removed rows where sums were 0, i.e., no covariates/over ocean
covariates.prop.200$log.house.km2 <- log(covariates.prop.200$housing +1)
covariates.prop.200$Income.mean <- covariates.prop.200$income

#COVARIATE NAMES
cov.names.woodland <- c("est","TIME.OBSERVATIONS.STARTED.dec.hr","DURATION.MINUTES","EFFORT.DISTANCE.KM","log.house.km2","Income.mean","Lake","Pond","EmergentWL","ForestShrubWL","River","dev_vlow","dev_low","dev_med","dev_high","decid","evergr","mixed","shrub","grass","past.hay","crops","patch","perforated","edge","core","elev","slope","eness","nness")

#MODEL FUNCTION
source(paste0(r.dir,"Models_RF_function.Subsets.R"))


##LOOP OVER 10 LISTS OF RANDOM SUBSETS
woodland.bbs.list <- list()
for(p in 1:10){
  ##CREATE LIST OF RANDOMLY SAMPLED ROWS FROM THINNED DATASET TO MATCH SAMPLE SIZES OF CULLED SUBSETS
  group.subset.thin <- list()
  for (l in 1:nrow(dat.sample.size.eBird.bbs.dates)){
    group.subset.thin[[l]] <- dat.u.thin[sample(nrow(dat.u.thin), dat.sample.size.eBird.bbs.dates[l,1]),]
  }
  #RUN FUNCTION
  woodland.bbs <- run.models(covariates=covariates.prop.200,cov.names =cov.names.woodland,bird.names=birds.5prev,training.list=group.subset.thin,test.dat=BBS.dat.all)
  tmp <- lapply(woodland.bbs,`[`,7,)#just AUC row
  names(tmp) <- dat.sample.size.eBird.bbs.dates[,2]
  woodland.bbs.auc <- do.call(rbind, lapply(lapply(tmp, unlist), "[", #this should work as replacement for list.rbind
                                           unique(unlist(c(sapply(tmp,names))))))
  colnames(woodland.bbs.auc) <- unique(unlist(c(sapply(tmp,names))))
  
  woodland.bbs.list[[p]] <- woodland.bbs.auc#each randomization because element in list
}

#save it
save.name <- "woodland.bbs.200mTrain.200mTest.RandomSubsets.fixed"
save(woodland.bbs.list,file=paste0(out.dir,save.name,".RData",sep=""))


