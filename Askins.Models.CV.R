#DESCRIPTION: 10-fold Cross-validation tests. Runs now for all 5 prevalence species.

#DATE: 6/4/2018

#TO DO: 

#History: Shrub.Askins.Models.CV


library(randomForest)
library(PresenceAbsence)
library(dplyr)

rm(list=ls())

##SET DIRECTORIES
in.dir <- "~/Project_Uconn/One/OneAnalysis/input/"
out.dir <- "~/Project_Uconn/One/OneAnalysis/output/"
r.dir <- "~/Project_Uconn/One/OneAnalysis/r/"

#BIRDS
load(paste0(in.dir,"bird.names.Askins.5prev.41spp.RData"))
bird.names <- birds.5prev;rm(birds.5prev)

#DATA
load(paste0(in.dir,"covs.Askins.200m.merge.RData"))
colnames(dat)[names(dat) %in% 'Eastern.Wood.pewee'] <- "Eastern.Wood.Pewee"

x.dat <- dat[,c("C_V__Block","rowname",  "dev_vlow","dev_low","dev_med", "dev_high","decid","evergr","mixed","grass", "shrub", "past.hay","crops", "Lake","Pond","EmergentWL","ForestShrubWL","patch","perforated","edge","core","elev","eness","nness","slope")]                  

eval.train.list <- list()
eval.test.list <- list()

for (j in 1:length(bird.names)){
 y.dat <- dat[,colnames(dat)%in%bird.names[j]]
  # #prevalence <- sum(y.dat)/length(y.dat)
y.dat <- ifelse(y.dat>0,1,0)
  y.dat<- as.factor(y.dat)
  x.y.dat <- cbind(y.dat,x.dat)

  #Perform 10 fold cross validation
  testData <- list()
  trainData <- list()
  #create lists to store results
  
  eval.train.list[[j]] <- 1
  eval.train <- NULL
  eval.test.list[[j]] <- 1
  eval.test <- NULL
for(i in 1:10){
  testData[[i]] <- x.y.dat[x.y.dat$C_V__Block %in% i,]
  trainData[[i]] <- x.y.dat[!x.y.dat$C_V__Block %in% i,]
  set.seed(88)
  RFmod <-randomForest(y.dat ~ .,data=trainData[[i]][,-c(2:3)], ntree=3000,importance=F, na.action=na.omit,proximity=F)#
  
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
}

#SAVES
names(eval.test.list) <- bird.names
eval.test.means <- lapply(eval.test.list, function(x) rowMeans(x,na.rm=TRUE))
eval.test.means <- do.call("rbind", eval.test.means)
eval.test.means <- as.data.frame(eval.test.means)
eval.test.means.Askins200m <- eval.test.means
save(eval.test.means.Askins200m ,file=paste0(out.dir,"eval.test.means.Askins200m.RData"))
