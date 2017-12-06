#library(survcomp)
library(Hmisc)
library(ranger)
library(randomForestSRC)
#library(rpart)
library(party)
library(prodlim)
library(pec)

# merge
movement_merge3 <- merge(movement_survival_sub,allpatientsfeature_imputed,all.x=TRUE)

#movement_final_sub <- droplevels(filter(movement_final, when>=92))
movement_final_sub<- subset(movement_merge3, select=-SubjectID)

# 9:1 비율로 training test 나누자.
movement_randomsample <- sample(1:nrow(movement_final_sub),round(nrow(movement_final_sub)*0.8))
movement_training <- movement_final_sub[movement_randomsample,]
movement_test <- movement_final_sub[-movement_randomsample,]


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


# Cox
movement_cox=coxph(Surv(when,is==1)~.,data=movement_training)
summary(movement_cox)
cox.zph(movement_cox)
survConcordance(Surv(when,is)~predict(movement_cox,movement_test),movement_test)$concordance
#rcorr.cens(summary(survfit(coxph.training,mergetest), time=365)$surv,Surv(mergetest$movement_whenevent,mergetest$movement_isevent))

# 이아래는 대조군 쓸것들
plot(survfit(movement_cox,newdata=movement_test[10,]),conf.int=FALSE,xlab="day",ylab="Survival")
plot(survfit(movement_cox),conf.int=FALSE,xlab="day",ylab="Survival")
plot(survfit(coxph(Surv(when,is==1)~1,data=movement_training)),conf.int=FALSE,xlab="day",ylab="Survival")
plot(survfit(Surv(when,is==1)~1,data=movement_training),conf.int=FALSE,xlab="day",ylab="Survival")

# new_df<-with(mergetraining,mergetest)
# plot(survfit(coxph.training,newdata=mergetest))
#print(survfit(coxph.training,newdata=mergetest),print.rmean=TRUE)
#quantile(survfit(coxph.training,newdata=mergetest),0.5)$quantile


# 이 아래는 weibull
movement_weibull=survreg(Surv(when,is==1)~.,data=movement_training,dist="weibull")
#summary(weibull.training)
1-survConcordance(Surv(when,is)~ predict(movement_weibull,movement_test),movement_test)$concordance
#rcorr.cens(predict(weibull.training,mergetest),Surv(mergetest$movement_whenevent,mergetest$movement_isevent))

# 이 아래는 lognormal
movement_lognormal=survreg(Surv(when,is==1)~.,data=movement_training,dist="lognormal")
1-survConcordance(Surv(when,is)~ predict(movement_lognormal,movement_test),movement_test)$concordance
#rcorr.cens(predict(lognormal.training,mergetest),Surv(mergetest$movement_whenevent,mergetest$movement_isevent))


# 이 아래는 randomforest.
movement_rf=rfsrc(Surv(when,is==1)~.,data=movement_training,importance=FALSE)
pred=predict(movement_rf,movement_test)
rcorr.cens(-predict(movement_rf,movement_test)$predicted,Surv(movement_test$when,movement_test$is))
plot(pred$time.interest,pred$survival[1,],type="l",ylim=c(0,1))

par(las=2)
par(mar=c(4,6,1,1))
barplot(movement_rf$importance[order(movement_rf$importance)],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Movement")

# 이 아래는 cforest
movement_cf=cforest(Surv(when,is==1)~.,data=movement_training,control=cforest_unbiased(ntree=20))
movement_predcf=treeresponse(movement_cf,movement_test)
movement_predcfmed <- vector()
for (i in 1:length(movement_predcf)){
  movement_predcfmed[i]=quantile(movement_predcf[[i]],0.5)$quantile
}
rcorr.cens(movement_predcfmed,Surv(movement_test$when,movement_test$is))


# 이 아래는 rnager. 그런데 잘 안되네. ranger는 survival 분석에는 좋지 않아 보인다.
movement_ranger=ranger(Surv(when,is==1)~.,data=movement_training,importance="permutation")
pred2=predict(movement_ranger,movement_test)
plot(pred2$unique.death.times,pred2$survival[1,],type="l",ylim=c(0,1))

# 아래는 (발표를 위한) 4애 모델의 생존분석 곡선 plot. 뭐 나중에 지우겠지.
#plot(survfit(coxph.training,newdata=mergetest[30,]),conf.int=FALSE,xlab="day",ylab="Survival",xlim=c(0,1300))
#lines(predict(weibull.training,newdata=mergetest[30,],type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red",type="l",lwd=1)
#lines(predict(lognormal.training,newdata=mergetest[30,],type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue",type="l",lwd=1)
#lines(pred$time.interest,pred$survival[30,],type="l",col="green",ylim=c(0,1))
#lines(predcf[[30]],conf.int=FALSE)

#legend("topright",c("Cox","Weibull","Lognormal","Randomforest"),lwd=2,bty="n",col=c("black","red","blue","green"))
#quantile(survfit(coxph.training,newdata=mergetest[100,]),0.5)$quantile
#predict(weibull.training,newdata=mergetest[100,],type="quantile",p=0.5)
#predict(lognormal.training,newdata=mergetest[100,],type="quantile",p=0.5)
#legend(900,1.05,bty="n", c("301","292.8","256.6","332","median survival"))


# 아래는 rpart인데 해보고 싶었는데 잘 안되네. 그냥 버릴 것.
#rf3=rpart(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+Onset_Delta+Diagnosis_Delta+weight_min+weight_max,data=mergetraining)
#pred3=predict(rf3)



#####Swallowing

# merge
swallowing_merge3 <- merge(swallowing_survival_sub,allpatientsfeature_imputed,all.x=TRUE)


swallowing_final_sub<- subset(swallowing_merge3, select=-SubjectID)


swallowing_randomsample <- sample(1:nrow(swallowing_final_sub),round(nrow(swallowing_final_sub)*0.8))
swallowing_training <- swallowing_final_sub[swallowing_randomsample,]
swallowing_test <- swallowing_final_sub[-swallowing_randomsample,]


# 이 아래는 콕스
swallowing_cox=coxph(Surv(when,is==1)~.,data=swallowing_training)
survConcordance(Surv(when,is)~predict(swallowing_cox,swallowing_test),swallowing_test)$concordance


# 이 아래는 weibull
swallowing_weibull=survreg(Surv(when,is==1)~.,data=swallowing_training,dist="weibull")
1-survConcordance(Surv(when,is)~ predict(swallowing_weibull,swallowing_test),swallowing_test)$concordance

# 이 아래는 randomforest.
swallowing_rf=rfsrc(Surv(when,is==1)~.,data=swallowing_training,importance=FALSE)
rcorr.cens(-predict(swallowing_rf,swallowing_test)$predicted,Surv(swallowing_test$when,swallowing_test$is))

par(las=2)
par(mar=c(4,6,1,1))
barplot(swallowing_rf$importance[order(swallowing_rf$importance)],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Swallowing")

# 이 아래는 cforest
swallowing_cf=cforest(Surv(when,is==1)~.,data=swallowing_training,control=cforest_unbiased(ntree=20))
swallowing_predcf=treeresponse(swallowing_cf,swallowing_test)
swallowing_predcfmed <- vector()
for (i in 1:length(swallowing_predcf)){
  swallowing_predcfmed[i]=quantile(swallowing_predcf[[i]],0.5)$quantile
}
rcorr.cens(swallowing_predcfmed,Surv(swallowing_test$when,swallowing_test$is))

swallowing_ranger=ranger(Surv(when,is==1)~.,data=swallowing_training,importance="permutation")



######### Communicating
# merge
communicating_merge3 <- merge(communicating_survival_sub,allpatientsfeature_imputed,all.x=TRUE)

communicating_final_sub<- subset(communicating_merge3, select=-SubjectID)


# 9:1 비율로 training test 나누자.
communicating_randomsample <- sample(1:nrow(communicating_final_sub),round(nrow(communicating_final_sub)*0.8))
communicating_training <- communicating_final_sub[communicating_randomsample,]
communicating_test <- communicating_final_sub[-communicating_randomsample,]


# 이 아래는 콕스
communicating_cox=coxph(Surv(when,is==1)~.,data=communicating_training)
survConcordance(Surv(when,is)~predict(communicating_cox,communicating_test),communicating_test)$concordance

#medianmedian=quantile(survfit(communicating_cox,newdata=communicating_training),0.5)$quantile
#subset2=data.frame("subject_id"=communicating_training$subject_id,medianmedian)
##subset1=subset(communicating_training,is==1,select=c("subject_id","when"))
#mergegg=merge(subset1,subset2,all.x=TRUE)
#plot(mergegg$when,mergegg$X50,xlim=c(0,1000),ylim=c(0,1000),main="Communicating",xlab="Real Survival Time", ylab="Predicted median survival time")


# 이 아래는 weibull
communicating_weibull=survreg(Surv(when,is==1)~.,data=communicating_training,dist="weibull")
1-survConcordance(Surv(when,is)~ predict(communicating_weibull,communicating_test),communicating_test)$concordance

# 이 아래는 randomforest.
communicating_rf=rfsrc(Surv(when,is==1)~.,data=communicating_training,importance=TRUE)
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



communicating_ranger=ranger(Surv(when,is==1)~.,data=communicating_training,importance="permutation")




################# Breathing
# merge
breathing_merge3 <- merge(breathing_survival_sub,allpatientsfeature_imputed,all.x=TRUE)

breathing_final_sub<- subset(breathing_merge3, select=-SubjectID)

# 9:1 비율로 training test 나누자.
breathing_randomsample <- sample(1:nrow(breathing_final_sub),round(nrow(breathing_final_sub)*0.8))
breathing_training <- breathing_final_sub[breathing_randomsample,]
breathing_test <- breathing_final_sub[-breathing_randomsample,]


# 이 아래는 콕스
breathing_cox=coxph(Surv(when,is==1)~.,data=breathing_training)
survConcordance(Surv(when,is)~predict(breathing_cox,breathing_test),breathing_test)$concordance


# 이 아래는 weibull
breathing_weibull=survreg(Surv(when,is==1)~.,data=breathing_training,dist="weibull")
1-survConcordance(Surv(when,is)~ predict(breathing_weibull,breathing_test),breathing_test)$concordance

# 이 아래는 randomforest.
breathing_rf=rfsrc(Surv(when,is==1)~.,data=breathing_training,importance=FALSE)
rcorr.cens(-predict(breathing_rf,breathing_test)$predicted,Surv(breathing_test$when,breathing_test$is))

par(las=2)
par(mar=c(4,6,1,1))
barplot(breathing_rf$importance[order(breathing_rf$importance)],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Breathing")

# 이 아래는 cforest
breathing_cf=cforest(formula(fmla1),data=breathing_training,control=cforest_unbiased(ntree=20))
breathing_predcf=treeresponse(breathing_cf,breathing_test)
breathing_predcfmed <- vector()
for (i in 1:length(breathing_predcf)){
  breathing_predcfmed[i]=quantile(breathing_predcf[[i]],0.5)$quantile
}
rcorr.cens(breathing_predcfmed,Surv(breathing_test$when,breathing_test$is))

breathing_ranger=ranger(Surv(when,is==1)~.,data=breathing_training,importance="permutation")


######################### death

# merge
death_survival <-semi_join(death_survival,alsfrsfull)
death_merge3 <- merge(death_survival,allpatientsfeature_imputed,all.x=TRUE)
death_final_sub<- subset(death_merge3, select=-SubjectID)

death_randomsample <- sample(1:nrow(death_final_sub),round(nrow(death_final_sub)*0.8))
death_training <- death_final_sub[death_randomsample,]
death_test <- death_final_sub[-death_randomsample,]


# ??? ????????? 콕스
death_cox=coxph(Surv(when,is==1)~.,data=death_training)
survConcordance(Surv(when,is)~predict(death_cox,death_test),death_test)$concordance


# ??? ????????? weibull
death_weibull=survreg(Surv(when,is==1)~.,data=death_training,dist="weibull")
1-survConcordance(Surv(when,is)~ predict(death_weibull,death_test),death_test)$concordance

# ??? ????????? randomforest.
death_rf=rfsrc(Surv(when,is==1)~.,data=death_training,importance=TRUE)
death_rfpred=predict(death_rf,death_test)
rcorr.cens(-death_rfpred$predicted,Surv(death_test$when,death_test$is))
death_rf$importance

sindex=sindex(death_rfpred$time.interest,30.5*24)
rcorr.cens(death_rfpred$survival[,sindex],Surv(death_test$when,death_test$is))
par(las=2)
par(mar=c(4,6,1,1))
barplot(death_rf$importance[order(death_rf$importance)],hori=TRUE,las=1,cex.names=1,main="Variable Importance : death")
plot(survfit(death_cox,newdata=death_test[22,]),conf.int=FALSE,xlab="day",ylab="Survival")
lines(death_rfpred$time.interest,death_rfpred$survival[30,],type="l",col="green",ylim=c(0,1))


# ??? ????????? cforest
death_cf=cforest(Surv(when,is==1)~.,data=death_training,control=cforest_unbiased(ntree=20), variable="permutation")
death_predcf=treeresponse(death_cf,death_test)
death_predcfmed <- vector()
for (i in 1:length(death_predcf)){
  death_predcfmed[i]=quantile(death_predcf[[i]],0.5)$quantile
}
rcorr.cens(death_predcfmed,Surv(death_test$when,death_test$is))

death_ranger=ranger(Surv(when,is==1)~.,data=death_training,importance="permutation")
death_ranger$variable.importance[order(death_ranger$variable.importance)]
plot(death_ranger$variable.importance[order(death_ranger$variable.importance)])