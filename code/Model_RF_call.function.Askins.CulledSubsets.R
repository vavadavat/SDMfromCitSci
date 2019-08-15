##DESCRIPTION
#Trains models with various eBird subsetted training sets 

#DATE: 8/13/2019


load("C:/Users/valer/Documents/Project_UConn/One/OneAnalysis/output/AUC.df.4groups_Exp_Dist.RData")#this one also has only COYE set, as it should be. 

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


#5 PREVALENCE
colnames(covariates.prop.200.test)
birds <- covariates.prop.200.test[,53:ncol(covariates.prop.200.test)]
birds <- ifelse(birds>0,1,0)
birds5 <-birds[,which((colSums(birds)/nrow(birds))>.049)]
birds.5prev <- colnames(birds5)
birds.5prev <- birds.5prev[!grepl(pattern="NA",birds.5prev)]
birds.5prev <- birds.5prev[complete.cases(birds.5prev)]
birds.5prev[birds.5prev %in% 'Eastern.Wood.pewee'] <- "Eastern.Wood.Pewee"

#TRAINING DATA 
load(paste0(in.dir,"group.subset.thin.Distance.Expertise.askins.RData"))

#COVARIATES; Same for all training data subsets
load(paste0(in.dir,"covs.eBirdSummerMorning.n14076.200m.prop.RData"))#now this is n12956 (11/6/2017) because in 'landcover.proportions.R' I removed rows where sums were 0, i.e., no covariates/over ocean

#COVARIATE NAMES
cov.names.askins <- c("est","rowname","DURATION.MINUTES","EFFORT.DISTANCE.KM","TIME.OBSERVATIONS.STARTED.dec.hr","dev_vlow","dev_low","dev_med", "dev_high","decid","evergr","mixed","grass", "shrub", "past.hay","crops", "Lake","Pond","EmergentWL","ForestShrubWL","patch","perforated","edge","core","elev","eness","nness","slope")                  

source(paste0(r.dir,"Models_RF_function.Subsets.R"))

##RUN MODELS
askins <- run.models(covariates=covariates.prop.200,cov.names=cov.names.askins,bird.names=birds.5prev,training.list=group.subset.thin,test.dat=covariates.prop.200.test)

#save it
save.name <- "askins.200mTrain.200mTest"
names(askins) <- names(group.subset.thin)
save(askins,file=paste(paste0(out.dir,save.name,".RData",sep="")))#/SensitivityAnalyses

dat.sample.size.tmp <- lapply(shrub.askins,function(x) max(x[1,]))

dat.sample.size <- t(as.data.frame(dat.sample.size.tmp))
dat.sample.size <- as.data.frame(dat.sample.size)
dat.sample.size$name <- rownames(dat.sample.size)
colnames(dat.sample.size)[1]<-"dat.sample.size"
rownames(dat.sample.size) <- NULL
dat.sample.size.eBird.askins.dates <- dat.sample.size
save(dat.sample.size.eBird.askins.dates,file=paste0(out.dir,"dat.sample.size.eBird.AskinsDates.RData"))
