##DESCRIPTION
#runs full thinned dataset 

#DATE: 8/13/2019

#HISTORY: Model_RF_call.function.woodland.allThin.5prev.R

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
BBS.dat.all <- rename(BBS.dat.all,eness=e.ness)
BBS.dat.all <- rename(BBS.dat.all,nness=n.ness)

#5 PREVALENCE
colnames(BBS.dat.all)
birds <- BBS.dat.all[,38:187]
birds5 <-birds[,which((colSums(birds)/nrow(birds))>.049)]
birds.5prev <- colnames(birds5)

#TRAINING DATA 
load(paste0(in.dir,"BBSDates.thinOnly.eBird.expertise.RData"))

#COVARIATES; Same for all training data subsets
load(paste0(in.dir,"covs.eBirdSummerMorning.n14076.200m.prop.RData"))#now this is n12956 (11/6/2017) because in 'landcover.proportions.R' I removed rows where sums were 0, i.e., no covariates/over ocean
covariates.prop.200$log.house.km2 <- log(covariates.prop.200$housing +1)
covariates.prop.200$Income.mean <- covariates.prop.200$income

#COVARIATE NAMES; est= observer expertise
cov.names.bbs <- c("est","TIME.OBSERVATIONS.STARTED.dec.hr","DURATION.MINUTES","EFFORT.DISTANCE.KM","log.house.km2","Income.mean","Lake","Pond","EmergentWL","ForestShrubWL","River","dev_vlow","dev_low","dev_med","dev_high","decid","evergr","mixed","shrub","grass","past.hay","crops","patch","perforated","edge","core","elev","slope","eness","nness")

#MODEL FUNCTION
source(paste0(r.dir,"Models_RF_function.allThin.R"))

#RUN MODELS
bbs.bbs <- run.models(covariates=covariates.prop.200,cov.names =cov.names.bbs,bird.names=birds.5prev,training.list=dat.u.thin,test.dat=BBS.dat.all)

#SAVE
save.name <- "bbs.200mTrain.200mTest.allThin"
save(bbs,file=paste0(out.dir,save.name,".RData",sep=""))


