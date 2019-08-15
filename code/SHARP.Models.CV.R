#DESCRIPTION: 10-fold Cross-validation tests , now all species with 5 prev.

#DATE: 6/4/2018

#History: Tidal.Models.CV.R


library(randomForest)
library(PresenceAbsence)
library(rfUtilities)
library(dplyr)

rm(list=ls())

##SET DIRECTORIES
in.dir <- "~/Project_Uconn/One/OneAnalysis/input/"
out.dir <- "~/Project_Uconn/One/OneAnalysis/output/"
r.dir <- "~/Project_Uconn/One/OneAnalysis/r/"

#BIRD NAMES
load(paste0(in.dir,"bird.names.sharp.5prev.47spp.RData"))
bird.names<-birds.5prev;rm(birds.5prev)

#DATA
load(paste0(in.dir,"dat.2014.tidal.marsh.200m.RData"))
birds <- dat.2014[,42:176]
birds <- ifelse(birds>0,1,0)
dat.2014[,42:176] <- birds

#cv.group 
load(paste0(in.dir,"table.ids.forCV.sharp.RData"))
colnames(table.ids)[2] <- "IDgroup"
dat.2014 <- merge(dat.2014,table.ids[,c("IDgroup","cv.group")],by="IDgroup")

#Y DAT AND X DAT
y.dat.all <- dat.2014[,colnames(dat.2014) %in% bird.names]
y.dat.all[is.na(y.dat.all)] <- 0

x.dat <- dat.2014[,c("cv.group","dev_vlow","dev_low","dev_med","dev_high","forest","low.veg","crops","HighMarsh","LowMarsh","Mudflat","Phragmites","Pool_Panne","EstuarineMarineWetland","EstuarineMarineDeep")]#,"housing";this isn't in the testing data for some reason

#STORAGE LISTS
eval.train.list <- list()
eval.test.list <- list()

for (j in 1:length(bird.names)){
  y.dat <- y.dat.all[,colnames(y.dat.all)%in%bird.names[j]]
  y.dat<- as.factor(y.dat)
  x.y.dat <- cbind(y.dat,x.dat)

  #create lists to store results
  testData <- list()
  trainData <- list()
  
  #perform 10 fold cross validation
  eval.train.list[[j]] <- 1
  eval.train <- NULL
  eval.test.list[[j]] <- 1
  eval.test <- NULL
for(i in 1:10){
  testData[[i]] <- x.y.dat[x.y.dat$cv.group %in% i,]
  trainData[[i]] <- x.y.dat[!x.y.dat$cv.group %in% i,]
  set.seed(88)
  
  RFmod <-randomForest(y.dat ~ .,data=trainData[[i]][,-c(2)], ntree=3000,importance=F, na.action=na.omit,proximity=F)#
  
  #predictions to train data
  prob.train.data <- predict(RFmod,trainData[[i]],type="prob")[,2]
  train.df <- cbind(seq(1:nrow(trainData[[i]])), (ifelse(trainData[[i]]$y.dat==1,1,0)),prob.train.data)
  kappa.thresh <- optimal.thresholds(DATA=train.df,opt.methods="MaxKappa")[,2]
  
  btrain <-as.data.frame(presence.absence.accuracy(train.df,threshold=kappa.thresh))
  btrain.2 <-round((btrain[,2:7]),2)#leave out standard devs, but could decide to include
  colnames(btrain.2) <-c("threshold","PCC","Sensitivity","Specificity","Kappa","AUC")
  bttrain <- round(t(btrain.2),2)
  colnames(bttrain)<-'Value'
  prev.pred.train <- round(sum(ifelse(prob.train.data>kappa.thresh,1,0))/length(prob.train.data),2)
  prev.actual.train <- round(sum(train.df[,2])/length(train.df[,2]),2)
  eval.traindata <-rbind(bttrain,prev.pred=prev.pred.train,prev.actual=prev.actual.train)
  eval.train <- cbind(eval.train,eval.traindata)
  
  
  #predictions to test data
  prob.test.data <- predict(RFmod,testData[[i]],type="prob")[,2]
  test.df <- cbind(seq(1:nrow(testData[[i]])), (ifelse(testData[[i]]$y.dat==1,1,0)),prob.test.data)
  #evaluation metrics
  b <-as.data.frame(presence.absence.accuracy(test.df,threshold=kappa.thresh))
  b.2 <-round((b[,2:7]),2)#leave out standard devs, but could decide to include
  colnames(b.2) <-c("threshold","PCC","Sensitivity","Specificity","Kappa","AUC")
  bt <- round(t(b.2),2)
  colnames(bt)<-'Value'
  prev.pred <- round(sum(ifelse(prob.test.data>kappa.thresh,1,0))/length(prob.test.data),2)
  prev.actual <- round(sum(test.df[,2])/length(test.df[,2]),2)
  eval.testdata <-rbind(bt,prev.pred=prev.pred,prev.actual=prev.actual)
  eval.test <- cbind(eval.test,eval.testdata)
   
}
  eval.test.list[[j]] <- eval.test
  eval.train.list[[j]] <- eval.train
}

#SAVES
names(eval.test.list) <- bird.names
names(eval.train.list) <- bird.names

eval.test.means <- lapply(eval.test.list, function(x) rowMeans(x,na.rm=TRUE))
eval.test.means <- do.call("rbind", eval.test.means)
eval.train.means <- lapply(eval.train.list, function(x) rowMeans(x,na.rm=TRUE))
eval.train.means <- do.call("rbind", eval.train.means)

eval.test.means

eval.test.means.Tidal200m <- eval.test.means
eval.train.means.Tidal200m <- eval.train.means

save(eval.test.means.Tidal200m ,file=paste0(out.dir,"eval.test.means.Tidal200m.RData"))
