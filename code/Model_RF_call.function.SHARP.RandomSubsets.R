##DESCRIPTION
#runs randomly subsetted thinned dataset to match samples sizes of 'culled' data subsets

#HISTORY: Model_RF_call.function.tidal.Random.5prev.R

#DATE 8/13/2019
 

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
load(paste0(in.dir,"dat.2014.sharp.200m.RData"))

##TRAINING DATA
load(paste0(in.dir,"sharpDates.thinOnly.eBird.expertise.RData"))

#BIRD NAMES
colnames(dat.2014)
birds <- dat.2014[,42:176]
birds <- ifelse(birds>0,1,0)
dat.2014[,42:176] <- birds
birds5 <-birds[,which((colSums(birds)/nrow(birds))>.049)]
birds.5prev <- colnames(birds5)
birds.5prev <- birds.5prev[complete.cases(birds.5prev)]
birds.5prev <- birds.5prev[!(birds.5prev%in%'Nelson.s.Sparrow')]#remove because migrant
birds.5prev <- birds.5prev[!(birds.5prev%in%'Lesser.Yellowlegs')]
birds.5prev <- birds.5prev[!(birds.5prev%in%'Sharp.tailed.Sparrow')]#remove because undifferentiated Nelson/Saltmarsh

##SAMPLE SIZE OF SUBSETS
load(paste0(out.dir,"dat.sample.size.eBird.SHARPDates.RData"))

##COVARIATES
load(paste0(in.dir,"covs.eBirdSummerMorning.n14076.200m.prop.RData"))#now this is n12956 (11/6/2017) because in 'landcover.proportions.R' I removed rows where sums were 0, i.e., no covariates/over ocean
covariates.prop.200$forest <- rowSums(covariates.prop.200[,c("decid","evergr","mixed")])
covariates.prop.200$low.veg <- rowSums(covariates.prop.200[,c("shrub","grass","past.hay")])

##COVARIATE NAMES
cov.names.sharp <- c("est","rowname","TIME.OBSERVATIONS.STARTED.dec.hr","DURATION.MINUTES","EFFORT.DISTANCE.KM","dev_vlow","dev_low","dev_med","dev_high","forest","low.veg","crops","HighMarsh","LowMarsh","Mudflat","Phragmites","Pool_Panne","EstuarineMarineWetland","EstuarineMarineDeep")

##CALL FUNCTION
source(paste0(r.dir,"Models_RF_function.Subsets.R"))

##LOOP OVER 10 LISTS OF RANDOM SUBSETS
sharp.list <- list()
for(p in 1:10){
##CREATE LIST OF RANDOMLY SAMPLED ROWS FROM THINNED DATASET TO MATCH SAMPLE SIZES OF CULLED SUBSETS
group.subset.thin <- list()
for (l in 1:nrow(dat.sample.size.eBird.sharp.dates)){
  group.subset.thin[[l]] <- dat.u.thin[sample(nrow(dat.u.thin), dat.sample.size.eBird.sharp.dates[l,1]),]
}
#RUN FUNCTION
sharp <- run.models(covariates=covariates.prop.200,cov.names=cov.names.sharp,bird.names=birds.5prev,training.list=group.subset.thin,test.dat=dat.2014)
tmp <- lapply(sharp,`[`,7,)#just AUC row
names(tmp) <- dat.sample.size.eBird.sharp.dates[,2]
sharp.auc <- do.call(rbind, lapply(lapply(tmp, unlist), "[", #this should work as replacement for list.rbind
                              unique(unlist(c(sapply(tmp,names))))))
colnames(sharp.auc) <- unique(unlist(c(sapply(tmp,names))))
sharp.list[[p]] <- sharp.auc#each randomization because element in list
}

##SAVE RESULT
save.name<- "sharp.200mTrain.200mTest.RandomSubsets"
save(sharp.list,file=paste0(out.dir,save.name,".RData",sep=""))



