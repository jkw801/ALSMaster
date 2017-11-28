#library(survcomp)
library(Hmisc)
library(ranger)
library(randomForestSRC)
#library(rpart)
library(party)
# NA imputation 안하고 일단 지우고 분석.
swallowing_final <- droplevels(filter(swallowing_merge3, !is.na(OnsetAge)&!is.na(Onsetsite)&!is.na(Sex)&!is.na(Onset_Delta)&!is.na(weight_min)&!is.na(weight_max)&
                                      !is.na(q1_min)&q1_min!=Inf&!is.na(q2_min)&q2_min!=Inf&!is.na(q3_min)&q3_min!=Inf&!is.na(q4_min)&q4_min!=Inf&!is.na(q6_min)&q6_min!=Inf&!is.na(q7_min)&q7_min!=Inf&!is.na(q8_min)&q8_min!=Inf&!is.na(q9_min)&q9_min!=Inf
                                    &!is.na(fvc)&!is.na(totalsub_min)&!is.na(preslope)))
swallowing_final_sub <- droplevels(filter(swallowing_final, when>=0))

  
swallowing_randomsample <- sample(1:nrow(swallowing_final_sub),round(nrow(swallowing_final_sub)*0.8))
swallowing_training <- swallowing_final_sub[swallowing_randomsample,]
swallowing_test <- swallowing_final_sub[-swallowing_randomsample,]

vars1=c("OnsetAge","Onsetsite","Sex","Onset_Delta","weight_min","weight_max","q1_min","q1_max","q2_min","q2_max","q3_min","q3_max","q4_min","q4_max","q6_min","q6_max","q7_min","q7_max","q8_min","q8_max","q9_min","q9_max"
        ,"fvc","totalsub_min","totalsub_max","preslope")
vars1sub=c("OnsetAge","Onsetsite","Sex","Onset_Delta","weight_min","weight_max","q1_min","q2_min","q3_min","q4_min","q6_min","q7_min","q8_min","q9_min"
           ,"fvc","totalsub_min","preslope")
vars2=c("OnsetAge","Onsetsite","Sex","log(-Onset_Delta+10)","weight_min","weight_max","q1_min","q1_max","q2_min","q2_max","q3_min","q3_max","q4_min","q4_max","q6_min","q6_max","q7_min","q7_max","q8_min","q8_max","q9_min","q9_max"
        ,"fvc","totalsub_min","totalsub_max","preslope")
vars2sub=c("OnsetAge","Onsetsite","Sex","log(-Onset_Delta+10)","weight_min","weight_max","q1_min","q2_min","q3_min","q4_min","q6_min","q7_min","q8_min","q9_min"
           ,"fvc","totalsub_min","preslope")
fmla1=paste("Surv(when,is==1)","~",paste(vars1,collapse="+"))
fmla2=paste("Surv(when,is==1)","~",paste(vars2,collapse="+"))
fmla1sub=paste("Surv(when,is==1)","~",paste(vars1sub,collapse="+"))
fmla2sub=paste("Surv(when,is==1)","~",paste(vars2sub,collapse="+"))

# 이 아래는 콕스
swallowing_cox=coxph(formula(fmla2sub),data=swallowing_training)
survConcordance(Surv(when,is)~predict(swallowing_cox,swallowing_test),swallowing_test)$concordance


# 이 아래는 weibull
swallowing_weibull=survreg(formula(fmla2),data=swallowing_training,dist="weibull")
1-survConcordance(Surv(when,is)~ predict(swallowing_weibull,swallowing_test),swallowing_test)$concordance

# 이 아래는 randomforest.
swallowing_rf=rfsrc(formula(fmla1sub),data=swallowing_training,importance=TRUE)
rcorr.cens(-predict(swallowing_rf,swallowing_test)$predicted,Surv(swallowing_test$when,swallowing_test$is))

par(las=2)
par(mar=c(4,6,1,1))
barplot(swallowing_rf$importance[order(swallowing_rf$importance)],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Swallowing")

# 이 아래는 cforest
swallowing_cf=cforest(formula(fmla1),data=swallowing_training,control=cforest_unbiased(ntree=20))
swallowing_predcf=treeresponse(swallowing_cf,swallowing_test)
swallowing_predcfmed <- vector()
for (i in 1:length(swallowing_predcf)){
  swallowing_predcfmed[i]=quantile(swallowing_predcf[[i]],0.5)$quantile
}
rcorr.cens(swallowing_predcfmed,Surv(swallowing_test$when,swallowing_test$is))
