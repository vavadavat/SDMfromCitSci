##DESCRIPTION
#Trains models with 16 culled eBird training sets and predicts to 
#SHARP tidal marsh data

#HISTORY Model_RF_call.function.tidal.5prev.R

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
load(paste0(in.dir,"group.subset.thin.Distance.Expertise.sharp.RData"))

#BIRD NAMES
birds <- dat.2014[,42:176]
birds <- ifelse(birds>0,1,0)
dat.2014[,42:176] <- birds
birds5 <-birds[,which((colSums(birds)/nrow(birds))>.049)]
birds.5prev <- colnames(birds5)
birds.5prev <- birds.5prev[complete.cases(birds.5prev)]
birds.5prev <- birds.5prev[!(birds.5prev%in%'Nelson.s.Sparrow')]#remove because migrant
birds.5prev <- birds.5prev[!(birds.5prev%in%'Lesser.Yellowlegs')]#remove because migrant
birds.5prev <- birds.5prev[!(birds.5prev%in%'Sharp.tailed.Sparrow')]#remove because undifferentiated Nelson/Saltmarsh

##COVARIATES
load(paste0(in.dir,"covs.eBirdSummerMorning.n14076.200m.prop.RData"))#now this is n12956 (11/6/2017) because in 'landcover.proportions.R' I removed rows where sums were 0, i.e., no covariates/over ocean
covariates.prop.200$forest <- rowSums(covariates.prop.200[,c("decid","evergr","mixed")])
covariates.prop.200$low.veg <- rowSums(covariates.prop.200[,c("shrub","grass","past.hay")])

##COVARIATE NAMES
cov.names.tidal <- c("est","rowname","TIME.OBSERVATIONS.STARTED.dec.hr","DURATION.MINUTES","EFFORT.DISTANCE.KM","dev_vlow","dev_low","dev_med","dev_high","forest","low.veg","crops","HighMarsh","LowMarsh","Mudflat","Phragmites","Pool_Panne","EstuarineMarineWetland","EstuarineMarineDeep")

##CALL FUNCTION
source(paste0(r.dir,"Models_RF_function.v3.R"))

##EXAMPLE
tidal.sharp <- run.models(covariates=covariates.prop.200,cov.names=cov.names.tidal,bird.names=birds.5prev,training.list=group.subset.thin,test.dat=dat.2014)

##SAVE RESULT
save.name<- "tidal.sharp.200mTrain.200mTest"
names(tidal.sharp)<-names(group.subset.thin)
save(tidal.sharp,file=paste0(out.dir,save.name,".RData",sep=""))

dat.sample.size.tmp <- lapply(tidal.sharp,function(x) max(x[1,]))

dat.sample.size <- t(as.data.frame(dat.sample.size.tmp))
dat.sample.size <- as.data.frame(dat.sample.size)
dat.sample.size$name <- rownames(dat.sample.size)
colnames(dat.sample.size)[1]<-"dat.sample.size"
rownames(dat.sample.size) <- NULL
dat.sample.size.eBird.sharp.dates <- dat.sample.size
save(dat.sample.size.eBird.sharp.dates,file=paste0(out.dir,"dat.sample.size.eBird.SHARPDates.RData"))
