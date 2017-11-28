#library(survcomp)
library(Hmisc)
library(ranger)
library(randomForestSRC)
#library(rpart)
library(party)
# NA imputation 안하고 일단 지우고 분석.
death_final <- droplevels(filter(death_merge3, !is.na(OnsetAge)&!is.na(Onsetsite)&!is.na(Sex)&!is.na(Onset_Delta)&!is.na(weight_min)&!is.na(weight_max)&
                                        !is.na(q1_min)&q1_min!=Inf&!is.na(q2_min)&q2_min!=Inf&!is.na(q3_min)&q3_min!=Inf&!is.na(q4_min)&q4_min!=Inf&!is.na(q6_min)&q6_min!=Inf&!is.na(q7_min)&q7_min!=Inf&!is.na(q8_min)&q8_min!=Inf&!is.na(q9_min)&q9_min!=Inf
                                      &!is.na(fvc)&!is.na(totalsub_min)&!is.na(preslope)))
death_final_sub <- droplevels(filter(death_final, when>=0))


death_randomsample <- sample(1:nrow(death_final_sub),round(nrow(death_final_sub)*0.8))
death_training <- death_final_sub[death_randomsample,]
death_test <- death_final_sub[-death_randomsample,]

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
death_cox=coxph(formula(fmla2sub),data=death_training)
survConcordance(Surv(when,is)~predict(death_cox,death_test),death_test)$concordance


# 이 아래는 weibull
death_weibull=survreg(formula(fmla2),data=death_training,dist="weibull")
1-survConcordance(Surv(when,is)~ predict(death_weibull,death_test),death_test)$concordance

# 이 아래는 randomforest.
death_rf=rfsrc(formula(fmla1sub),data=death_training,importance=TRUE)
rcorr.cens(-predict(death_rf,death_test)$predicted,Surv(death_test$when,death_test$is))

par(las=2)
par(mar=c(4,6,1,1))
barplot(death_rf$importance[order(death_rf$importance)],hori=TRUE,las=1,cex.names=1,main="Variable Importance : death")

# 이 아래는 cforest
death_cf=cforest(formula(fmla1),data=death_training,control=cforest_unbiased(ntree=20))
death_predcf=treeresponse(death_cf,death_test)
death_predcfmed <- vector()
for (i in 1:length(death_predcf)){
  death_predcfmed[i]=quantile(death_predcf[[i]],0.5)$quantile
}
rcorr.cens(death_predcfmed,Surv(death_test$when,death_test$is))