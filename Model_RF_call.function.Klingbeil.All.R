##DESCRIPTION
#runs full thinned dataset

#DATE: 8/13/2019

#HISTORY: Model_RF_call.function.forest.allThin.R

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

#COVARIATE NAMES
cov.names.klingbeil <- c("est","rowname","TIME.OBSERVATIONS.STARTED.dec.hr","DURATION.MINUTES","EFFORT.DISTANCE.KM","dev_vlow","dev_low","dev_med","dev_high","decid","evergr","mixed","elev","slope","eness","nness","patch","perforated","edge","core","Lake","Pond","EmergentWL","ForestShrubWL","River")


source(paste0(r.dir,"Models_RF_function.All.R"))

##EXAMPLE
klingbeil <- run.models(covariates=covariates.prop.200,cov.names =cov.names.klingbeil,bird.names=bird.names.5prev,training.list=dat.u.thin,test.dat=klingbeil.dat.all)

#save it
save.name <- "klingbeil.200mTrain.200mTest.allThin"

save(klingbeil,file=paste(paste0(out.dir,save.name,".RData",sep="")))
