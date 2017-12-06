#library(survcomp)
library(Hmisc)
library(ranger)
library(randomForestSRC)
#library(rpart)
library(party)
library(prodlim)

allmovement_cox=coxph(Surv(when,is==1)~.,data=movement_final_sub)
allswallowing_cox=coxph(Surv(when,is==1)~.,data=swallowing_final_sub)
allcommunicating_cox=coxph(Surv(when,is==1)~.,data=communicating_final_sub)
allbreathing_cox=coxph(Surv(when,is==1)~.,data=breathing_final_sub)
alldeath_cox=coxph(Surv(when,is==1)~.,data=death_final_sub)

movement_1=movement_merge3[,c(1,2,3)]
names(movement_1)=c("SubjectID","m_when","m_is")
swallowing_1=swallowing_merge3[,c(1,2,3)]
names(swallowing_1)=c("SubjectID","s_when","s_is")
communicating_1=communicating_merge3[,c(1,2,3)]
names(communicating_1)=c("SubjectID","c_when","c_is")
breathing_1=breathing_merge3[,c(1,2,3)]
names(breathing_1)=c("SubjectID","b_when","b_is")
death_1=death_merge3[,c(1,2,3)]
names(death_1)=c("SubjectID","d_when","d_is")
all_1 <- movement_1 %>% merge(swallowing_1) %>% merge(communicating_1) %>% merge( breathing_1) %>% merge(death_1)
alldata=merge(all_1,allpatientsfeature_imputed,all.x=TRUE)
# 음..위처럼 안하고 그냥 allpatientsfeature_imputed로 해도 되지 않나?

movement_curve=survfit(allmovement_cox,newdata=allpatientsfeature_imputed)
movement_prob=movement_curve$surv[sindex(movement_curve$time,30.5 * 12),]
swallowing_curve=survfit(allswallowing_cox,newdata=allpatientsfeature_imputed)
swallowing_prob=swallowing_curve$surv[sindex(swallowing_curve$time,30.5 * 12),]
communicating_curve=survfit(allcommunicating_cox,newdata=allpatientsfeature_imputed)
communicating_prob=communicating_curve$surv[sindex(communicating_curve$time,30.5 * 12),]
breathing_curve=survfit(allbreathing_cox,newdata=allpatientsfeature_imputed)
breathing_prob=breathing_curve$surv[sindex(breathing_curve$time,30.5 * 12),]
death_curve=survfit(alldeath_cox,newdata=allpatientsfeature_imputed)
death_prob=death_curve$surv[sindex(death_curve$time,30.5 * 12),]

allprob=data.frame("SubjectID"=allpatientsfeature_imputed$SubjectID,movement_prob,swallowing_prob,communicating_prob,breathing_prob,death_prob)

wss<-0
for ( i in 1:15){
  km.out <- kmeans(allprob[,-1],centers=i,nstart=20)
  wss[i] <- km.out$tot.withinss
}
plot(1:15, wss, type="b")
km.out <- kmeans(allprob[,-1],centers=3,nstart=20)
allcluster <- data.frame(allpatientsfeature_imputed,allprob[,-1],km.out$cluster)
allcluster1 <- allcluster[allcluster$km.out.cluster==1,]
allcluster2 <- allcluster[allcluster$km.out.cluster==2,]
allcluster3<- allcluster[allcluster$km.out.cluster==3,]
#plot(survfit(Surv(b_when,b_is)~1,allcluster1),xlab="Cluster1",ylab="Proportion surviving",conf.int=FALSE,xlim=c(0,1000))
#lines(survfit(Surv(c_when,c_is)~1,allcluster1),col="blue",conf.int=FALSE)
#lines(survfit(Surv(s_when,s_is)~1,allcluster1),col="red",conf.int=FALSE)
#lines(survfit(Surv(m_when,m_is)~1,allcluster1),col="green",conf.int=FALSE)
#lines(survfit(Surv(d_when,d_is)~1,allcluster1),col=6,conf.int=FALSE)
#legend("topright",c("Movement","Breathing","Swallowing","Communicating","Death"),lwd=2,bty="n",col=c("green","black","red","blue",6))
#plot(survfit(Surv(b_when,b_is)~1,allcluster2),xlab="Cluster2",ylab="Proportion surviving",conf.int=FALSE,xlim=c(0,1000))
#lines(survfit(Surv(c_when,c_is)~1,allcluster2),col="blue",conf.int=FALSE)
#lines(survfit(Surv(s_when,s_is)~1,allcluster2),col="red",conf.int=FALSE)
#lines(survfit(Surv(m_when,m_is)~1,allcluster2),col="green",conf.int=FALSE)
#lines(survfit(Surv(d_when,d_is)~1,allcluster2),col=6,conf.int=FALSE)
#legend("topright",c("Movement","Breathing","Swallowing","Communicating","Death"),lwd=2,bty="n",col=c("green","black","red","blue",6))
#plot(survfit(Surv(b_when,b_is)~1,allcluster3),xlab="Cluster3",ylab="Proportion surviving",conf.int=FALSE,xlim=c(0,1000))
#lines(survfit(Surv(c_when,c_is)~1,allcluster3),col="blue",conf.int=FALSE)
#lines(survfit(Surv(s_when,s_is)~1,allcluster3),col="red",conf.int=FALSE)
#lines(survfit(Surv(m_when,m_is)~1,allcluster3),col="green",conf.int=FALSE)
#lines(survfit(Surv(d_when,d_is)~1,allcluster3),col=6,conf.int=FALSE)
#legend("topright",c("Movement","Breathing","Swallowing","Communicating","Death"),lwd=2,bty="n",col=c("green","black","red","blue",6))

plot(allcluster$onset_site~allcluster$km.out.cluster)

boxplot(allcluster$movement_prob~allcluster$km.out.cluster)
boxplot(allcluster$swallowing_prob~allcluster$km.out.cluster)
boxplot(allcluster$communicating_prob~allcluster$km.out.cluster)
boxplot(allcluster$breathing_prob~allcluster$km.out.cluster)
boxplot(allcluster$death_prob~allcluster$km.out.cluster)

barplot(c("movement"=mean(allcluster[allcluster$km.out.cluster==1,]$movement_prob),"swallowing"=mean(allcluster[allcluster$km.out.cluster==1,]$swallowing_prob),"communi"=mean(allcluster[allcluster$km.out.cluster==1,]$communicating_prob),"breathing"=mean(allcluster[allcluster$km.out.cluster==1,]$breathing_prob),"death"=mean(allcluster[allcluster$km.out.cluster==1,]$death_prob)),ylim=c(0,1),main="Cluster 1")
barplot(c("movement"=mean(allcluster[allcluster$km.out.cluster==2,]$movement_prob),"swallowing"=mean(allcluster[allcluster$km.out.cluster==2,]$swallowing_prob),"communi"=mean(allcluster[allcluster$km.out.cluster==2,]$communicating_prob),"breathing"=mean(allcluster[allcluster$km.out.cluster==2,]$breathing_prob),"death"=mean(allcluster[allcluster$km.out.cluster==2,]$death_prob)),ylim=c(0,1),main="Cluster 2")
barplot(c("movement"=mean(allcluster[allcluster$km.out.cluster==3,]$movement_prob),"swallowing"=mean(allcluster[allcluster$km.out.cluster==3,]$swallowing_prob),"communi"=mean(allcluster[allcluster$km.out.cluster==3,]$communicating_prob),"breathing"=mean(allcluster[allcluster$km.out.cluster==3,]$breathing_prob),"death"=mean(allcluster[allcluster$km.out.cluster==3,]$death_prob)),ylim=c(0,1),main="Cluster 3")


hclust.out <- hclust(dist(allprob[,-1]))
plot(hclust.out)
hclust=cutree(hclust.out,k=3)
table(km.out$cluster,hclust)
allclusterbyh <- data.frame(allpatientsfeature_imputed,allprob[,-1],hclust)

plot(allclusterbyh$onset_site~allclusterbyh$hclust)

boxplot(allclusterbyh$movement_prob~allclusterbyh$hclust)
boxplot(allclusterbyh$swallowing_prob~allclusterbyh$hclust)
boxplot(allclusterbyh$communicating_prob~allclusterbyh$hclust)
boxplot(allclusterbyh$breathing_prob~allclusterbyh$hclust)
boxplot(allclusterbyh$death_prob~allclusterbyh$hclust)
