library(survival)

demographics<-read.csv("demographics.csv")
riluzole <- read.csv("Riluzole.csv")

#  Age,Sex,Riluzole 변수 추가.
demographics_sub <- subset(demographics, select=c("subject_id","Age","Sex"))
riluzole_sub <- subset(riluzole, select=c("subject_id","Subject_used_Riluzole"))
merge2 <- merge(merge1,demographics_sub,by="subject_id",all.x=TRUE)
merge2 <- merge(merge2, riluzole_sub,by="subject_id",all.x=TRUE)
merge2 <- merge2[order(merge2$movement_whenevent),]

# Sex 변수가 정제가 안되어 있길래 살짝 수정
merge2$Sex <- droplevels(merge2$Sex)

# DiagnosisDelta >0인 것들은 오류일테니 NA처
merge2$Diagnosis_Delta[merge2$Diagnosis_Delta>0]=NA


# 이 아래들은 Onsetsite, Sex, Riluzole 변수에 따라 KM곡선그리고 log-rank test 해본 것
# 그냥 궁금해서 해본 것. 연구에 도움은 안됨
surv.by.onsetsite = survfit(Surv(movement_whenevent,movement_isevent==1)~Onsetsite,data=merge2)
plot(surv.by.onsetsite,xlab="Time",ylab="Survival",col=c("black","red","blue","Green"),lty=c(1,1,1,1), main="Kaplan-Meier Surival vs. Onsetsite in ALS")
legend(1300,.9, c("Bulbar","Limb","Limb&Bulbar","Other"),col=c("black","red","blue","Green"),lty=c(1,1,1,1))
surv.diff.by.onsetsite=survdiff(Surv(movement_whenevent,movement_isevent==1)~Onsetsite,data=merge2)
surv.diff.by.onsetsite
surv.by.sex=survfit(Surv(movement_whenevent,movement_isevent==1)~Sex,data=merge2)
plot(surv.by.sex,xlab="Time",ylab="Survival",col=c("black","red"),lty=c(1,1), main="Kaplan-Meier Surival vs. Sex in ALS")
legend(1300,.9, c("Female","Male"),col=c("black","red"),lty=c(1,1))
surv.diff.by.sex=survdiff(Surv(movement_whenevent,movement_isevent==1)~Sex,data=merge2)
surv.diff.by.sex
surv.by.riluzole=survfit(Surv(movement_whenevent,movement_isevent==1)~Subject_used_Riluzole,data=merge2)
plot(surv.by.riluzole,xlab="Time",ylab="Survival",col=c("black","red"),lty=c(1,1), main="Kaplan-Meier Surival vs. used_Riluzole in ALS")
legend(800,.9, c("No","Yes"),col=c("black","red"),lty=c(1,1))
surv.diff.by.riluzole=survdiff(Surv(movement_whenevent,movement_isevent==1)~Subject_used_Riluzole,data=merge2)
surv.diff.by.riluzole


# 콕스 모형 만들기
coxph.all=coxph(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+log(-Onset_Delta)+log(-Diagnosis_Delta+10),data=merge2)
summary(coxph.all)
cox.zph(coxph.all)

# 아래는 콕스로 new data 예측해보기. print.rmean은 생존함수 곡선 밑 면적, 즉 생존시간 기댓값 구해줌.
#new_df<-with(merge2,data.frame(Onsetsite=c("Limb","Bulbar"),Sex=c("Male","Female"),Onset_Delta=c(-70,-50),Diagnosis_Delta=c(-50,-30)))
print(survfit(coxph.all,newdata=data.frame(Onsetsite=c("Limb","Bulbar"),Sex=c("Male","Female"),Onset_Delta=c(-70,-50),Diagnosis_Delta=c(-50,-30))),print.rmean=TRUE)
#print(survfit(coxph.all,newdata=new_df),print.rmean=TRUE)

# qunatile은 생존함수의 역함수값. summary는 생존함수의 값 구해줌.
quantile(survfit(coxph.all,newdata=data.frame(Onsetsite="Limb",Sex="Male",Onset_Delta=-70,Diagnosis_Delta=-50)),c(0.3,0.5))
summary(survfit(coxph.all,newdata=data.frame(Onsetsite="Limb",Sex="Male",Onset_Delta=-70,Diagnosis_Delta=-50)), time=400)$surv


# new data 생존함수 plot.  ggsurvplot 굳이 안해도되네.
plot(survfit(coxph.all,newdata=data.frame(Onsetsite=c("Limb","Bulbar"),Sex=c("Male","Female"),Onset_Delta=c(-70,-50),Diagnosis_Delta=c(-50,-30))))
# ggsurvplot(survfit(coxph.all,newdata=new_df),legend.labs="Limb onset, Male, Delta : (-100,-50)",ggtheme=theme_minimal())


# Weibull 모형 만들
survreg.all=survreg(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+log(-Onset_Delta)+log(-Diagnosis_Delta+10),data=merge2,dist="weibull")
summary(survreg.all)

# 생존함수의 역함수 값 predict
predict(survreg.all,data.frame(Onsetsite="Limb",Sex="Male",Onset_Delta=-100,Diagnosis_Delta=-50),type="quantile",p=0.5)

# new data 생존함수 plot. 왜 parametric 모형은 plot을 직접 짜야 하는 것인가.
plot(predict(survreg.all,data.frame(Onsetsite="Limb",Sex="Male",Onset_Delta=-100,Diagnosis_Delta=-50),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red",type="l",lwd=1)


# 이 아래는 비교를 위한 것. 후에 지운다.
plot(survfit(coxph.all,newdata=data.frame(Onsetsite="Limb",Sex="Male",Onset_Delta=-100,Diagnosis_Delta=-50)),conf.int=FALSE)
plot(predict(survreg(with(merge2,Surv(movement_whenevent,movement_isevent==1))~Onsetsite+Sex+log(-Onset_Delta)+log(-Diagnosis_Delta+10),data=merge2,dist="weibull"),data.frame(Onsetsite="Limb",Sex="Male",Onset_Delta=-100,Diagnosis_Delta=-50),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red")



