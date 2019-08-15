#DESCRIPTION Linear mixed effect models to explain variation in delta
#AUC (benchmark AUC - eBird AUC) using culling method 
#(observer expertise, distance traveled) and species traits

#DATE 8/13/2019


library(lme4)
library(dplyr)

rm(list=ls())

#SET DIRECTORIES
in.dir <- "~/Project_Uconn/One/OneAnalysis/input/"
out.dir <- "~/Project_Uconn/One/OneAnalysis/output/"
r.dir <- "~/Project_Uconn/One/OneAnalysis/r/"

#DATA
load(paste0(out.dir,"AUC.eBird.culled.traits.RData"))
dat
#FUNCTION
source(paste0(r.dir,"AIC.Lmm.Fcn.R"))

############
####SHARP
############

##bestmodels
tm.dat <- dat[which(dat$group %in% "sharp"),]
tm.dat <- droplevels(tm.dat) 
AIC.LMM.Fcn(tm.dat)

#best model, within delta 6 AIC
disBYu.tm <- lmer(diff.auc.0.5.logit ~distance*ubiquity +(1|species.f),data=tm.dat)
summary(disBYu.tm)

############
####Klingbeil
############
f.dat <- dat[which(dat$group %in% "klingbeil"),]
f.dat <- droplevels(f.dat)  
AIC.LMM.Fcn(f.dat)

#best model, within delta 6 AIC
disBYprev.f <- lmer(diff.auc.0.5.logit ~prevalence*distance +(1|species.f),data=f.dat)
summary(disBYprev.f)
dis.f <- lmer(diff.auc.0.5.logit ~distance +(1|species.f),data=f.dat)
summary(dis.f)
disBYu.f <- lmer(diff.auc.0.5.logit ~ubiquity*distance +(1|species.f),data=f.dat)
summary(disBYu.f)

############
####Askins
############

s.dat <- dat[which(dat$group %in% "askins"),]
s.dat <- droplevels(s.dat)  

AIC.LMM.Fcn(s.dat)

#best models, within delta 6 AIC
dis.s <- lmer(diff.auc.0.5.logit ~distance +(1|species.f),data=s.dat)
summary(dis.s)
disBYid.s <- lmer(diff.auc.0.5.logit ~identifiability*distance +(1|species.f),data=s.dat)
summary(disBYid.s)
disBYu.s <- lmer(diff.auc.0.5.logit ~ubiquity*distance +(1|species.f),data=s.dat)
summary(disBYu.s)

############
#####BBS
############
w.dat <- dat[which(dat$group %in% "bbs"),]
w.dat <- droplevels(w.dat) 

AIC.LMM.Fcn(w.dat)

#best models, within delta 6 AIC
disBYprev.w <- lmer(diff.auc.0.5.logit ~distance*prevalence +(1|species.f),data=w.dat)
summary(disBYprev.w)
