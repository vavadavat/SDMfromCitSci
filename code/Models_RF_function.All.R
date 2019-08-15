###
#date
#2/19/2018
####
#history Models_RF_function.v2.R
###


run.models <- function(cov.names,bird.names,training.list,test.dat,covariates,save.name){
  
    #merge eBird training data with covariates, the merge depends on different columns depending on whether this was a mask or not
    dat.all <-merge(training.list,covariates,by=c("LATITUDE","LONGITUDE"),all.x=F,all.y=F)
    n.obs <- nrow(dat.all)
    #reformat bird names to be consistent
    names <- colnames(dat.all)[4:479]
    names <- gsub(pattern=" ",replacement=".",x=names)
    names <- gsub(pattern="-",replacement=".",x=names)
    names <- gsub(pattern="'",replacement=".",x=names)
    colnames(dat.all)[4:479] <- names
    x.dat <- dat.all[,cov.names]
    
    ##temporary saves
    eval.testdata.dat <- NULL
    
    for (i in 1:length(bird.names)){
      
      y.dat <- dat.all[,colnames(dat.all)%in% bird.names[i]]
      y.dat <- ifelse(y.dat>0,1,0)
      y.dat.f <- as.factor(y.dat)
      x.y.dat <- cbind(y.dat.f,x.dat)
      
     # set.seed(88)
      tryCatch({
      rf.mod <- randomForest(y.dat.f ~ .,data=x.y.dat[,!colnames(x.y.dat) %in% c("rowname")],importance=F,proximity=F,ntree=3000)
      rf.mod
      
      prob.train.dat <- predict(rf.mod,type="prob")[,2]
      DF=cbind(seq(1:nrow(x.y.dat)),y.dat,prob.train.dat)
      kappa.thresh <- optimal.thresholds(DATA=DF,opt.methods="MaxKappa")[,2]
      maxsensspec.thresh <- optimal.thresholds(DATA=DF,opt.methods="MaxSens+Spec")[,2]
      obs.prev.thresh <- optimal.thresholds(DATA=DF,opt.methods="PredPrev=Obs")[,2]
      sens.eq.spec.thresh <- optimal.thresholds(DATA=DF,opt.methods="Sens=Spec")[,2]
      
      test.dat$TIME.OBSERVATIONS.STARTED.dec.hr <- 7.0
      test.dat$DURATION.MINUTES <- mean(x.dat$DURATION.MINUTES)
      test.dat$EFFORT.DISTANCE.KM <- mean(x.dat$EFFORT.DISTANCE.KM)
      test.dat$est <- quantile(x.dat$est,0.75)#take observer at 75th percentile
      prob.test.dat <- predict(rf.mod,newdata=test.dat,type="prob")[,2]
      y.dat.test <- as.vector(test.dat[,bird.names[i]])
      
      #evaluation metrics
      DF.test=cbind(seq(1:length(prob.test.dat)),y.dat.test,prob.test.dat)
      b <-as.data.frame(presence.absence.accuracy(DF.test,threshold=kappa.thresh,st.dev=F))
      b.2 <-round(b[,2:ncol(b)],2)#leave out standard devs, but could decide to include
      colnames(b.2) <-c("threshold","PCC","Sensitivity","Specificity","Kappa","AUC")
      bt <- t(b.2)
      colnames(bt)<-'Value'
      prev.pred.kappa <- round(sum(ifelse(prob.test.dat>kappa.thresh,1,0))/length(prob.test.dat),2)
      prev.pred.maxsensspec <- round(sum(ifelse(prob.test.dat>maxsensspec.thresh,1,0))/length(prob.test.dat),2)
      prev.pred.obs.prev <- round(sum(ifelse(prob.test.dat>obs.prev.thresh,1,0))/length(prob.test.dat),2)
      prev.pred.sens.eq.spec <- round(sum(ifelse(prob.test.dat>sens.eq.spec.thresh,1,0))/length(prob.test.dat),2)
      
      prev.actual <- round(sum(y.dat.test)/length(y.dat.test),2)
      eval.testdata <-rbind(n.obs,bt,prev.actual=prev.actual,prev.pred.kappa=prev.pred.kappa,prev.pred.maxsensspec=prev.pred.maxsensspec,prev.pred.obs.prev=prev.pred.obs.prev,prev.pred.sens.eq.spec=prev.pred.sens.eq.spec)
      colnames(eval.testdata) <-bird.names[i]
      eval.testdata.dat <- cbind(eval.testdata.dat,eval.testdata)
      print(i)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
  return(eval.testdata.dat)
}


