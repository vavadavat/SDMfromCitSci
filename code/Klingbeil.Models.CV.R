#DESCRIPTION: 10-fold Cross-validation tests 

#DATE: 8/14/2019


library(randomForest)
library(PresenceAbsence)
library(dplyr)

rm(list=ls())

##SET DIRECTORIES
in.dir <- "~/Project_Uconn/One/OneAnalysis/input/"
out.dir <- "~/Project_Uconn/One/OneAnalysis/output/"
r.dir <- "~/Project_Uconn/One/OneAnalysis/r/"

#BIRDS
load(paste0(in.dir,"bird.names.klingbeil.5percentPrevalence.RData"))
bird.names <- bird.names.klingbeil;rm(bird.names.klingbeil)

#DATA
load(paste0(in.dir,"klingbeil.dat.all.200m.RData"))
klingbeil.dat.all <- klingbeil.dat.all.200m;rm(klingbeil.dat.all.200m)
klingbeil.dat.all <- rename(klingbeil.dat.all,eness=e.ness,nness=n.ness)
dat <- klingbeil.dat.all;rm(klingbeil.dat.all)

#COVARIATE NAMES
cov.names.forest <- c("SITE","dev_vlow","dev_low","dev_med","dev_high","decid","evergr","mixed","elev","slope","eness","nness","patch","perforated","edge","core","woody.wl","herb.wl")
x.dat <- dat[,c(cov.names.forest)]

#UNIQUE SITES to group pt cts by site for cv splits
site.uni <- unique(dat$SITE)
#randomly shuffle:
site.uni.sort <- as.data.frame(sample(site.uni,replace=F))
colnames(site.uni.sort) <- "SITE"
#10 groups:
splits <- seq(1,10,1)
#each group is repeated twice:
site.uni.sort$group <- rep(splits,2)

#STORAGE LISTS
eval.train.list <- list()
eval.test.list <- list()

for (j in 1:length(bird.names)){
 y.dat <- dat[,colnames(dat)%in%bird.names[j]]
 y.dat <- ifelse(y.dat>0,1,0)
 y.dat<- as.factor(y.dat)
 x.y.dat <- cbind(y.dat,x.dat)

 #create lists to store results
  testData <- list()
  trainData <- list()

  #Perform 10 fold cross validation
  eval.train.list[[j]] <- 1
  eval.train <- NULL
  eval.test.list[[j]] <- 1
  eval.test <- NULL
 
for(i in 1:10){
  testData[[i]] <- x.y.dat[x.y.dat$SITE %in% site.uni.sort[which(site.uni.sort$group %in% i),1],]
  trainData[[i]] <- x.y.dat[!x.y.dat$SITE %in% site.uni.sort[which(site.uni.sort$group %in% i),1],]
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
  b.2 <-round((b[,2:7]),2)
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
eval.test.means.BK200m <- eval.test.means
save(eval.test.means.BK200m ,file=paste0(out.dir,"eval.test.means.BK200m.RData"))
