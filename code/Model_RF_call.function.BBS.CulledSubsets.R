##DESCRIPTION
#Trains models with 16 culled eBird training sets and predicts to BBS data

#DATE: 8/13/2019

#HISTORY: Model_RF_call.function.woodland.5prev.R 

##LIBRARIES
library(randomForest)
library(PresenceAbsence)
library(dplyr)

rm(list=ls())

##SET DIRECTORIES
in.dir <- "~/Project_Uconn/One/OneAnalysis/input/"
out.dir <- "~/Project_Uconn/One/OneAnalysis/output/"
r.dir <- "~/Project_Uconn/One/OneAnalysis/r/"

#TEST DATA
load(paste0(in.dir,"bird.names.woodland.5percentPrevalence.RData"))#bird names with min. 5% prevalence
load(paste0(in.dir,"BBS.dat.all.200m.RData"))
BBS.dat.all <- BBS.dat.all.200m;rm(BBS.dat.all.200m)
BBS.dat.all <- rename(BBS.dat.all,eness=e.ness,nness=n.ness)


#5 PREVALENCE
colnames(BBS.dat.all)
birds <- BBS.dat.all[,38:187]
birds5 <-birds[,which((colSums(birds)/nrow(birds))>.049)]
birds.5prev <- colnames(birds5)

#TRAINING DATA eBird
load(paste0(in.dir,"group.subset.thin.Distance.Expertise.bbs.RData"))


#COVARIATES; Same for all training data subsets
load(paste0(in.dir,"covs.eBirdSummerMorning.n14076.200m.prop.RData"))#now this is n12956 (11/6/2017) because in 'landcover.proportions.R' I removed rows where sums were 0, i.e., no covariates/over ocean
covariates.prop.200$log.house.km2 <- log(covariates.prop.200$housing +1)
covariates.prop.200$Income.mean <- covariates.prop.200$income


#COVARIATE NAMES; est = observer expertise
cov.names.woodland <- c("est","TIME.OBSERVATIONS.STARTED.dec.hr","DURATION.MINUTES","EFFORT.DISTANCE.KM","log.house.km2","Income.mean","Lake","Pond","EmergentWL","ForestShrubWL","River","dev_vlow","dev_low","dev_med","dev_high","decid","evergr","mixed","shrub","grass","past.hay","crops","patch","perforated","edge","core","elev","slope","eness","nness")


source(paste0(r.dir,"Models_RF_function.Subsets.R"))


#RUN MODELS
bbs <- run.models(covariates=covariates.prop.200,cov.names =cov.names.woodland,bird.names=birds.5prev,training.list=group.subset.thin,test.dat=BBS.dat.all)

save.name <- "bbs.200mTrain.200mTest"
names(bbs) <- names(group.subset.thin)

save(bbs,file=paste0(out.dir,save.name,".RData",sep=""))

#save sample size of culled subsets to use in 'random subsets' runs.
dat.sample.size.tmp <- lapply(bbs,function(x) max(x[1,]))
dat.sample.size <- t(as.data.frame(dat.sample.size.tmp))
dat.sample.size <- as.data.frame(dat.sample.size)
dat.sample.size$name <- rownames(dat.sample.size)
colnames(dat.sample.size)[1]<-"dat.sample.size"
rownames(dat.sample.size) <- NULL
dat.sample.size.eBird.bbs.dates <- dat.sample.size
save(dat.sample.size.eBird.bbs.dates,file=paste0(out.dir,"dat.sample.size.eBird.BBSDates.RData"))
