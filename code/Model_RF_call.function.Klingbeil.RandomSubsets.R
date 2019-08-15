##DESCRIPTION
#runs full thinned dataset with expertise included as covariate

#DATE: 8/13/2019

#HISTORY: Model_RF_call.function.forest.RandomSubsets.5prev.R

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
load(paste0(in.dir,"bird.names.klingbeil.5percentPrevalence.RData"))
bird.names.5prev <- bird.names.klingbeil; rm(bird.names.klingbeil)
load(paste0(in.dir,"klingbeil.dat.all.200m.RData"))
klingbeil.dat.all <- klingbeil.dat.all.200m;rm(klingbeil.dat.all.200m)
klingbeil.dat.all <- rename(klingbeil.dat.all,eness=e.ness,nness=n.ness)

#TRAINING DATA 
load(paste0(in.dir,"KlingbeilDates.thinOnly.eBird.expertise.RData"))

#COVARIATES; Same for all training data subsets
load(paste0(in.dir,"covs.eBirdSummerMorning.n14076.200m.prop.RData"))#now this is n12956 (11/6/2017) because in 'landcover.proportions.R' I removed rows where sums were 0, i.e., no covariates/over ocean

##SAMPLE SIZE OF SUBSETS
load(paste0(out.dir,"dat.sample.size.eBird.BKDates.RData"))

#COVARIATE NAMES
cov.names.forest <- c("est","rowname","TIME.OBSERVATIONS.STARTED.dec.hr","DURATION.MINUTES","EFFORT.DISTANCE.KM","dev_vlow","dev_low","dev_med","dev_high","decid","evergr","mixed","elev","slope","eness","nness","patch","perforated","edge","core","Lake","Pond","EmergentWL","ForestShrubWL","River")


source(paste0(r.dir,"Models_RF_function.Subsets.R"))


##LOOP OVER 10 LISTS OF RANDOM SUBSETS
forest.BK.list <- list()
for(p in 1:10){
  ##CREATE LIST OF RANDOMLY SAMPLED ROWS FROM THINNED DATASET TO MATCH SAMPLE SIZES OF CULLED SUBSETS
  group.subset.thin <- list()
  for (l in 1:nrow(dat.sample.size.eBird.BK.dates)){
    group.subset.thin[[l]] <- dat.u.thin[sample(nrow(dat.u.thin), dat.sample.size.eBird.BK.dates[l,1]),]
  }
  #RUN FUNCTION
  klingbeil <- run.models(covariates=covariates.prop.200,cov.names =cov.names.forest,bird.names=bird.names.5prev,training.list=group.subset.thin,test.dat=klingbeil.dat.all)
  tmp <- lapply(klingbeil,`[`,7,)#just AUC row
  names(tmp) <- dat.sample.size.eBird.klingbeil.dates[,2]
  klingbeil.auc <-list.rbind(tmp)
  klingbeil.list[[p]] <- klingbeil.auc#each randomization because element in list
  
}

#save it
save.name <- "klingbeil.200mTrain.200mTest.RandomSubsets"

save(klingbeil.list,file=paste0(in.dir,save.name,".RData",sep=""))
