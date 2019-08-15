#DESCRIPTION Runs lmer on a priori models, summarize AIC

#DATE 8/13/2019


AIC.LMM.Fcn <- function(dat){
  ###CULLING.CRITERIA
  null <- lmer(diff.auc.0.5.logit ~(1|species.f),data=dat)
  disBYexp <- lmer(diff.auc.0.5.logit ~distance*expertise +(1|species.f),data=dat)
  exp <- lmer(diff.auc.0.5.logit ~expertise +(1|species.f),data=dat)
  disExp <- lmer(diff.auc.0.5.logit ~expertise +distance +(1|species.f),data=dat)
  dis <- lmer(diff.auc.0.5.logit ~distance +(1|species.f),data=dat)
  
  ###TRAITS
  prev <- lmer(diff.auc.0.5.logit ~prevalence.z  +(1|species.f),data=dat)
  id <- lmer(diff.auc.0.5.logit ~identifiability.z+(1|species.f),data=dat)
  ubiq <- lmer(diff.auc.0.5.logit ~ubiquity.z  +(1|species.f),data=dat)
  abun <- lmer(diff.auc.0.5.logit ~abundance.z+(1|species.f),data=dat)
  
  ###INTERACTION: TRAIT*CULLING.CRITERIA
  expBYid <- lmer(diff.auc.0.5.logit ~expertise*identifiability.z +(1|species.f),data=dat) 
  expBYu <- lmer(diff.auc.0.5.logit ~ubiquity.z*expertise +(1|species.f),data=dat)
  expBYa <- lmer(diff.auc.0.5.logit ~abundance.z*expertise +(1|species.f),data=dat)
  expBYprev <- lmer(diff.auc.0.5.logit ~expertise*prevalence.z +(1|species.f),data=dat)
  
  disBYid <- lmer(diff.auc.0.5.logit ~identifiability.z*distance +(1|species.f),data=dat)
  disBYu <- lmer(diff.auc.0.5.logit ~ubiquity.z*distance +(1|species.f),data=dat)
  disBYa <- lmer(diff.auc.0.5.logit ~abundance.z*distance +(1|species.f),data=dat)
  disBYprev <- lmer(diff.auc.0.5.logit ~distance*prevalence.z +(1|species.f),data=dat)
  
  aic.table.traits.method <- anova(disBYid,disBYu,disBYa,disBYprev,expBYid,expBYu,expBYa,expBYprev,prev,id,ubiq,abun,disBYexp,exp,disExp,dis,null)
  print(aic.table.traits.method[order(aic.table.traits.method$AIC),])
  
}
