#library(survcomp)
library(Hmisc)
library(ranger)
library(randomForestSRC)
#library(rpart)
library(party)
colnames(movement_merge3)[names(movement_merge3)=="when"]="movement_when"
colnames(movement_merge3)[names(movement_merge3)=="is"]="movement_is"
colnames(swallowing_merge3)[names(swallowing_merge3)=="when"]="swallowing_when"
colnames(swallowing_merge3)[names(swallowing_merge3)=="is"]="swallowing_is"
colnames(communicating_merge3)[names(communicating_merge3)=="when"]="communicating_when"
colnames(communicating_merge3)[names(communicating_merge3)=="is"]="communicating_is"
colnames(breathing_merge3)[names(breathing_merge3)=="when"]="breathing_when"
colnames(breathing_merge3)[names(breathing_merge3)=="is"]="breathing_is"
allmerge <- merge(merge(merge(movement_merge3,subset(swallowing_merge3,select=c("subject_id","swallowing_when","swallowing_is")),by="subject_id"),subset(communicating_merge3,select=c("subject_id","communicating_when","communicating_is")),by="subject_id"),subset(breathing_merge3,select=c("subject_id","breathing_when","breathing_is")),by="subject_id")


allfinal <- droplevels(filter(allmerge, !is.na(OnsetAge)&!is.na(Onsetsite)&!is.na(Sex)&!is.na(Onset_Delta)&!is.na(weight_min)&!is.na(weight_max)&
                                      !is.na(q1_min)&q1_min!=Inf&!is.na(q2_min)&q2_min!=Inf&!is.na(q3_min)&q3_min!=Inf&!is.na(q4_min)&q4_min!=Inf&!is.na(q6_min)&q6_min!=Inf&!is.na(q7_min)&q7_min!=Inf&!is.na(q8_min)&q8_min!=Inf&!is.na(q9_min)&q9_min!=Inf
                                    &!is.na(fvc)&!is.na(totalsub_min)&!is.na(preslope)))


vars1=c("OnsetAge","Onsetsite","Sex","Onset_Delta","weight_min","weight_max","q1_min","q1_max","q2_min","q2_max","q3_min","q3_max","q4_min","q4_max","q6_min","q6_max","q7_min","q7_max","q8_min","q8_max","q9_min","q9_max"
        ,"fvc","totalsub_min","totalsub_max","preslope")
vars1sub=c("OnsetAge","Onsetsite","Sex","Onset_Delta","weight_min","weight_max","q1_min","q2_min","q3_min","q4_min","q6_min","q7_min","q8_min","q9_min"
           ,"fvc","totalsub_min","preslope")
vars2=c("OnsetAge","Onsetsite","Sex","log(-Onset_Delta+10)","weight_min","weight_max","q1_min","q1_max","q2_min","q2_max","q3_min","q3_max","q4_min","q4_max","q6_min","q6_max","q7_min","q7_max","q8_min","q8_max","q9_min","q9_max"
        ,"fvc","totalsub_min","totalsub_max","preslope")
vars2sub=c("OnsetAge","Onsetsite","Sex","log(-Onset_Delta+10)","weight_min","weight_max","q1_min","q2_min","q3_min","q4_min","q6_min","q7_min","q8_min","q9_min"
           ,"fvc","totalsub_min","preslope")

movement_fmla2=paste("Surv(movement_when,movement_is==1)","~",paste(vars2,collapse="+"))
movement_fmla2sub=paste("Surv(movement_when,movement_is==1)","~",paste(vars2sub,collapse="+"))
swallowing_fmla2=paste("Surv(swallowing_when,swallowing_is==1)","~",paste(vars2,collapse="+"))
swallowing_fmla2sub=paste("Surv(swallowing_when,swallowing_is==1)","~",paste(vars2sub,collapse="+"))
communicating_fmla2=paste("Surv(communicating_when,communicating_is==1)","~",paste(vars2,collapse="+"))
communicating_fmla2sub=paste("Surv(communicating_when,communicating_is==1)","~",paste(vars2sub,collapse="+"))
breathing_fmla2=paste("Surv(breathing_when,breathing_is==1)","~",paste(vars2,collapse="+"))
breathing_fmla2sub=paste("Surv(breathing_when,breathing_is==1)","~",paste(vars2sub,collapse="+"))



allmovement_cox=coxph(formula(movement_fmla2),data=allfinal)
allswallowing_cox=coxph(formula(swallowing_fmla2),data=allfinal)
allcommunicating_cox=coxph(formula(communicating_fmla2),data=allfinal)
allbreathing_cox=coxph(formula(breathing_fmla2),data=allfinal)

movement_median=quantile(survfit(allmovement_cox,newdata=allfinal),0.5)$quantile
swallowing_median=quantile(survfit(allswallowing_cox,newdata=allfinal),0.5)$quantile
communicating_median=quantile(survfit(allcommunicating_cox,newdata=allfinal),0.5)$quantile
breathing_median=quantile(survfit(allbreathing_cox,newdata=allfinal),0.5)$quantile
allmedian=data.frame("subject_id"=allfinal$subject_id,movement_median,swallowing_median,communicating_median,breathing_median)
names(allmedian)=c("subject_id","movement","swallowing","communicating","breathing")



