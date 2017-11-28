#library(survcomp)
library(Hmisc)
library(ranger)
library(randomForestSRC)
#library(rpart)
library(party)
library(prodlim)
# NA imputation 안하고 일단 지우고 분석.
movement_final <- droplevels(filter(movement_merge3, !is.na(OnsetAge)&!is.na(Onsetsite)&!is.na(Sex)&!is.na(Onset_Delta)&!is.na(weight_min)&!is.na(weight_max)&
                              !is.na(q1_min)&q1_min!=Inf&!is.na(q2_min)&q2_min!=Inf&!is.na(q3_min)&q3_min!=Inf&!is.na(q4_min)&q4_min!=Inf&!is.na(q6_min)&q6_min!=Inf&!is.na(q7_min)&q7_min!=Inf&!is.na(q8_min)&q8_min!=Inf&!is.na(q9_min)&q9_min!=Inf
                              &!is.na(fvc)&!is.na(totalsub_min)&!is.na(preslope)))
movement_final_sub <- droplevels(filter(movement_final, when>=0))


# 9:1 비율로 training test 나누자.
movement_randomsample <- sample(1:nrow(movement_final_sub),round(nrow(movement_final_sub)*0.8))
movement_training <- movement_final_sub[movement_randomsample,]
movement_test <- movement_final_sub[-movement_randomsample,]


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
movement_cox=coxph(formula(fmla2sub),data=movement_training)
#summary(coxph.training)
#cox.zph(coxph.training)
#1-rcorr.cens(predict(coxph.training,mergetest),Surv(mergetest$movement_whenevent,mergetest$movement_isevent))
survConcordance(Surv(when,is)~predict(movement_cox,movement_test),movement_test)$concordance
#rcorr.cens(summary(survfit(coxph.training,mergetest), time=365)$surv,Surv(mergetest$movement_whenevent,mergetest$movement_isevent))

# new_df<-with(mergetraining,mergetest)
# plot(survfit(coxph.training,newdata=mergetest))
#print(survfit(coxph.training,newdata=mergetest),print.rmean=TRUE)
#quantile(survfit(coxph.training,newdata=mergetest),0.5)$quantile
#cindex=concordance.index(predict(coxph.training,mergetest),mergetest$movement_whenevent,mergetest$movement_isevent,na.rm=TRUE)
#cindex$c.index
#concordance.index(-quantile(survfit(coxph.training,newdata=mergetraining),0.5)$quantile,mergetraining$movement_whenevent,mergetraining$movement_isevent,na.rm=TRUE)$c.index




# 이 아래는 weibull
movement_weibull=survreg(formula(fmla2),data=movement_training,dist="weibull")

#summary(weibull.training)
1-survConcordance(Surv(when,is)~ predict(movement_weibull,movement_test),movement_test)$concordance
#rcorr.cens(predict(weibull.training,mergetest),Surv(mergetest$movement_whenevent,mergetest$movement_isevent))

# 이 아래는 lognormal
movement_lognormal=survreg(formula(fmla2),data=movement_training,dist="lognormal")

1-survConcordance(Surv(when,is)~ predict(movement_lognormal,movement_test),movement_test)$concordance
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
movement_rf=rfsrc(formula(fmla1sub),data=movement_training,importance=TRUE)
rcorr.cens(-predict(movement_rf,movement_test)$predicted,Surv(movement_test$when,movement_test$is))

a=data.frame(movement_test,predict(movement_rf,movement_test)$predicted)
a=filter(a,is==1)
plot(a$when,a$predict.movement_rf..movement_test..predicted)
#확률 계산시 sindex(c(7,8,9),7) 등 ㅇ유

par(las=2)
par(mar=c(4,6,1,1))
barplot(movement_rf$importance[order(movement_rf$importance)],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Movement")

# 이 아래는 cforest
movement_cf=cforest(formula(fmla1),data=movement_training,control=cforest_unbiased(ntree=20))
movement_predcf=treeresponse(movement_cf,movement_test)
movement_predcfmed <- vector()
for (i in 1:length(movement_predcf)){
  movement_predcfmed[i]=quantile(movement_predcf[[i]],0.5)$quantile
}
rcorr.cens(movement_predcfmed,Surv(movement_test$when,movement_test$is))



# 이 아래는 rnager. 그런데 잘 안되네. ranger는 survival 분석에는 좋지 않아 보인다.
rf2=ranger(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+Onset_Delta+Diagnosis_Delta+weight_min+weight_max+q1_min+q1_max+q2_min+q2_max+q3_min+q3_max+q4_min+q4_max+q6_min+q6_max
           +q7_min+q7_max+q8_min+q8_max+q9_min+q9_max,data=mergetraining)
pred2=predict(rf2,mergetest)
plot(pred2$unique.death.times,pred2$survival[1,],type="l",ylim=c(0,1))


# 아래는 (발표를 위한) 4애 모델의 생존분석 곡선 plot. 뭐 나중에 지우겠지.
plot(survfit(coxph.training,newdata=mergetest[30,]),conf.int=FALSE,xlab="day",ylab="Survival",xlim=c(0,1300))
lines(predict(weibull.training,newdata=mergetest[30,],type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red",type="l",lwd=1)
lines(predict(lognormal.training,newdata=mergetest[30,],type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue",type="l",lwd=1)
lines(pred$time.interest,pred$survival[30,],type="l",col="green",ylim=c(0,1))
lines(predcf[[30]],conf.int=FALSE)

legend("topright",c("Cox","Weibull","Lognormal","Randomforest"),lwd=2,bty="n",col=c("black","red","blue","green"))
quantile(survfit(coxph.training,newdata=mergetest[100,]),0.5)$quantile
predict(weibull.training,newdata=mergetest[100,],type="quantile",p=0.5)
predict(lognormal.training,newdata=mergetest[100,],type="quantile",p=0.5)
legend(900,1.05,bty="n", c("301","292.8","256.6","332","median survival"))


# 아래는 rpart인데 해보고 싶었는데 잘 안되네. 그냥 버릴 것.
#rf3=rpart(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+Onset_Delta+Diagnosis_Delta+weight_min+weight_max,data=mergetraining)
#pred3=predict(rf3)
