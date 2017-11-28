#library(survcomp)
library(Hmisc)
library(ranger)
library(randomForestSRC)
#library(rpart)
library(party)
# NA imputation 안하고 일단 지우고 분석.
communicating_final <- droplevels(filter(communicating_merge3, !is.na(OnsetAge)&!is.na(Onsetsite)&!is.na(Sex)&!is.na(Onset_Delta)&!is.na(weight_min)&!is.na(weight_max)&
                                        !is.na(q1_min)&q1_min!=Inf&!is.na(q2_min)&q2_min!=Inf&!is.na(q3_min)&q3_min!=Inf&!is.na(q4_min)&q4_min!=Inf&!is.na(q6_min)&q6_min!=Inf&!is.na(q7_min)&q7_min!=Inf&!is.na(q8_min)&q8_min!=Inf&!is.na(q9_min)&q9_min!=Inf
                                      &!is.na(fvc)&!is.na(totalsub_min)&!is.na(preslope)))
communicating_final_sub <- droplevels(filter(communicating_final, when>=0))


# 9:1 비율로 training test 나누자.
communicating_randomsample <- sample(1:nrow(communicating_final_sub),round(nrow(communicating_final_sub)*0.8))
communicating_training <- communicating_final_sub[communicating_randomsample,]
communicating_test <- communicating_final_sub[-communicating_randomsample,]


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
communicating_cox=coxph(formula(fmla2sub),data=communicating_training)
survConcordance(Surv(when,is)~predict(communicating_cox,communicating_test),communicating_test)$concordance

#medianmedian=quantile(survfit(communicating_cox,newdata=communicating_training),0.5)$quantile
#subset2=data.frame("subject_id"=communicating_training$subject_id,medianmedian)
##subset1=subset(communicating_training,is==1,select=c("subject_id","when"))
#mergegg=merge(subset1,subset2,all.x=TRUE)
#plot(mergegg$when,mergegg$X50,xlim=c(0,1000),ylim=c(0,1000),main="Communicating",xlab="Real Survival Time", ylab="Predicted median survival time")


# 이 아래는 weibull
communicating_weibull=survreg(formula(fmla2),data=communicating_training,dist="weibull")
1-survConcordance(Surv(when,is)~ predict(communicating_weibull,communicating_test),communicating_test)$concordance

# 이 아래는 randomforest.
communicating_rf=rfsrc(formula(fmla1sub),data=communicating_training,importance=TRUE)
rcorr.cens(-predict(communicating_rf,communicating_test)$predicted,Surv(communicating_test$when,communicating_test$is))


par(las=2)
par(mar=c(4,6,1,1))
barplot(communicating_rf$importance[order(communicating_rf$importance)],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Communicating")



# 이 아래는 cforest
communicating_cf=cforest(formula(fmla1),data=communicating_training,control=cforest_unbiased(ntree=20))
communicating_predcf=treeresponse(communicating_cf,communicating_test)
communicating_predcfmed <- vector()
for (i in 1:length(communicating_predcf)){
  communicating_predcfmed[i]=quantile(communicating_predcf[[i]],0.5)$quantile
}
rcorr.cens(communicating_predcfmed,Surv(communicating_test$when,communicating_test$is))




