#library(survcomp)
library(Hmisc)
library(ranger)
library(randomForestSRC)
#library(rpart)
library(party)
library(flexsurv)
# NA imputation 안하고 일단 지우고 분석.
merge3 <- droplevels(filter(merge3, !is.na(Onsetsite)&!is.na(Sex)&!is.na(Onset_Delta)&!is.na(Diagnosis_Delta)&!is.na(weight_min)&!is.na(weight_max)))
merge3$Diagnostic_delay <- merge3$Diagnosis_Delta-merge3$Onset_Delta
# 9:1 비율로 training test 나누자.
randomsample <- sample(1:nrow(merge3),round(nrow(merge3)*0.9))
mergetraining <- merge3[randomsample,]
mergetest <- merge3[-randomsample,]


# for cross validation
#randomsample <- sample(1:nrow(merge3))
#merge3 <- merge3[randomsample,]
#v1 <- rep(0,10)
#v2 <- rep(0,10)
#v3 <- rep(0,10)
#v4 <- rep(0,10)
#for (i in 1:10){
#bbb <-  (303*(i-1)+1):(303*i)
#mergetraining <- merge3[-bbb,]
#mergetest<-merge3[bbb,]


# 이 아래는 콕스
coxph.training=coxph(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+log(Diagnostic_delay+10)+log(-Diagnosis_Delta+10)+weight_min+weight_max+q1_min+q1_max+q2_min+q2_max+q3_min+q3_max+q4_min+q4_max+q6_min+q6_max
                     +q7_min+q7_max+q8_min+q8_max+q9_min+q9_max,data=mergetraining)
#summary(coxph.training)
#cox.zph(coxph.training)
#1-rcorr.cens(predict(coxph.training,mergetest),Surv(mergetest$movement_whenevent,mergetest$movement_isevent))
survConcordance(Surv(movement_whenevent,movement_isevent)~predict(coxph.training,mergetest),mergetest)$concordance
rcorr.cens(summary(survfit(coxph.training,mergetest), time=365)$surv,Surv(mergetest$movement_whenevent,mergetest$movement_isevent))

# new_df<-with(mergetraining,mergetest)
# plot(survfit(coxph.training,newdata=mergetest))
#print(survfit(coxph.training,newdata=mergetest),print.rmean=TRUE)
#quantile(survfit(coxph.training,newdata=mergetest),0.5)$quantile
#cindex=concordance.index(predict(coxph.training,mergetest),mergetest$movement_whenevent,mergetest$movement_isevent,na.rm=TRUE)
#cindex$c.index
#concordance.index(-quantile(survfit(coxph.training,newdata=mergetraining),0.5)$quantile,mergetraining$movement_whenevent,mergetraining$movement_isevent,na.rm=TRUE)$c.index




# 이 아래는 weibull
weibull.training=survreg(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+log(Diagnostic_delay+10)+log(-Diagnosis_Delta+10)+weight_min+weight_max+q1_min+q1_max+q2_min+q2_max+q3_min+q3_max+q4_min+q4_max+q6_min+q6_max
                         +q7_min+q7_max+q8_min+q8_max+q9_min+q9_max,data=mergetraining,dist="weibull")

#summary(weibull.training)
1-survConcordance(Surv(movement_whenevent,movement_isevent)~ predict(weibull.training,mergetest),mergetest)$concordance
#rcorr.cens(predict(weibull.training,mergetest),Surv(mergetest$movement_whenevent,mergetest$movement_isevent))

# 이 아래는 lognormal
lognormal.training=survreg(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+log(Diagnostic_delay+10)+log(-Diagnosis_Delta+10)+weight_min+weight_max+q1_min+q1_max+q2_min+q2_max+q3_min+q3_max+q4_min+q4_max+q6_min+q6_max
                           +q7_min+q7_max+q8_min+q8_max+q9_min+q9_max,data=mergetraining,dist="lognormal")

1-survConcordance(Surv(movement_whenevent,movement_isevent)~ predict(lognormal.training,mergetest),mergetest)$concordance
#rcorr.cens(predict(lognormal.training,mergetest),Surv(mergetest$movement_whenevent,mergetest$movement_isevent))


#rf.training=ranger(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+Onset_Delta+Diagnosis_Delta+weight_min+weight_max,data=mergetraining,importance="permutation")
# importance="permutation, splitrule="extratrees" ?
#death_times <- rf.training$unique.death.times
#surv_prob <- data.frame(rf.training$survival)
#avg_prob <- sapply(surv_prob,mean)
#plot(rf.training$unique.death.times,rf.training$survival[1,], type="l")
#vi<-data.frame(sort(round(rf.training$variable.importance,4),decreasing=TRUE))
#names(vi)<-"importance"
#rf.training$prediction.error

# 이 아래는 randomforest.
rf=rfsrc(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+Diagnostic_delay+Diagnosis_Delta+weight_min+weight_max+q1_min+q1_max+q2_min+q2_max+q3_min+q3_max+q4_min+q4_max+q6_min+q6_max
         +q7_min+q7_max+q8_min+q8_max+q9_min+q9_max,data=mergetraining)
pred=predict(rf,mergetest)
rcorr.cens(-pred$predicted,Surv(mergetest$movement_whenevent,mergetest$movement_isevent))


# 이 아래는 cforest
cf=cforest(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+Onset_Delta+Diagnosis_Delta+weight_min+weight_max+q1_min+q1_max+q2_min+q2_max+q3_min+q3_max+q4_min+q4_max+q6_min+q6_max
           +q7_min+q7_max+q8_min+q8_max+q9_min+q9_max,data=mergetraining,control=cforest_unbiased(ntree=20))
predcf=treeresponse(cf,mergetest)
predcfmed <- vector()
for (i in 1:length(predcf)){
  predcfmed[i]=quantile(predcf[[i]],0.5)$quantile
}
rcorr.cens(predcfmed,Surv(mergetest$movement_whenevent,mergetest$movement_isevent))

cf2=ctree(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+Onset_Delta+Diagnosis_Delta+weight_min+weight_max+q1_min+q1_max+q2_min+q2_max+q3_min+q3_max+q4_min+q4_max+q6_min+q6_max
           +q7_min+q7_max+q8_min+q8_max+q9_min+q9_max,data=mergetraining,control=cforest_unbiased(ntree=20))
predcf2=treeresponse(cf2,mergetest)
predcfmed2 <- vector()
for (i in 1:length(predcf2)){
  predcfmed2[i]=quantile(predcf2[[i]],0.5)$quantile
}
rcorr.cens(predcfmed2,Surv(mergetest$movement_whenevent,mergetest$movement_isevent))

# 이 아래는 rnager. 그런데 잘 안되네. ranger는 survival 분석에는 좋지 않아 보인다.
rf2=ranger(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+Onset_Delta+Diagnosis_Delta+weight_min+weight_max+q1_min+q1_max+q2_min+q2_max+q3_min+q3_max+q4_min+q4_max+q6_min+q6_max
           +q7_min+q7_max+q8_min+q8_max+q9_min+q9_max,data=mergetraining)
pred2=predict(rf2,mergetest)
plot(pred2$unique.death.times,pred2$survival[1,],type="l",ylim=c(0,1))


# 아래는 (발표를 위한) 4애 모델의 생존분석 곡선 plot. 뭐 나중에 지우겠지.
plot(survfit(coxph.training,newdata=mergetest[25,]),conf.int=FALSE,xlab="day",ylab="Survival",xlim=c(0,1300))
plot(predict(weibull.training,newdata=mergetest[25,],type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red",type="l",lwd=1)
lines(predict(lognormal.training,newdata=mergetest[25,],type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue",type="l",lwd=1)
plot(pred$time.interest,pred$survival[150,],type="l",col="green",ylim=c(0,1))
lines(predcf[[25]])

legend("topright",c("Cox","Weibull","Lognormal","Randomforest"),lwd=2,bty="n",col=c("black","red","blue","green"))
quantile(survfit(coxph.training,newdata=mergetest[100,]),0.5)$quantile
predict(weibull.training,newdata=mergetest[100,],type="quantile",p=0.5)
predict(lognormal.training,newdata=mergetest[100,],type="quantile",p=0.5)
legend(900,1.05,bty="n", c("301","292.8","256.6","332","median survival"))


# 아래는 rpart인데 해보고 싶었는데 잘 안되네. 그냥 버릴 것.
#rf3=rpart(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+Onset_Delta+Diagnosis_Delta+weight_min+weight_max,data=mergetraining)
#pred3=predict(rf3)
