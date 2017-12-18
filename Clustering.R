ans1 <- gammaImpute(formula(paste("Surv(movement_whenevent,movement_isevent==1)","~",paste(names(movement_final_sub)[c(3:15)],collapse="+"))),data=movement_final_sub,m=2 , gamma.factor=0.5,DCO.time=max(movement_final_sub$movement_whenevent))
b1=ExtractSingle(ans1,index=1)
b1=b1$data
training1=subset(b1,select=-c(1:15,internalDCO.time,internal_gamma_val))
names(training1)[c(length(names(training1))-1,length(names(training1)))]=c("when","is")
movement_cox=coxph(Surv(when,is==1)~.,data=training1)

ans2 <- gammaImpute(formula(paste("Surv(swallowing_whenevent,swallowing_isevent==1)","~",paste(names(swallowing_final_sub)[c(3:15)],collapse="+"))),data=swallowing_final_sub,m=2 , gamma.factor=2,DCO.time=max(swallowing_final_sub$swallowing_whenevent))
b2=ExtractSingle(ans2,index=1)
b2=b2$data
training2=subset(b2,select=-c(1:15,internalDCO.time,internal_gamma_val))
names(training2)[c(length(names(training2))-1,length(names(training2)))]=c("when","is")
swallowing_cox=coxph(Surv(when,is==1)~.,data=training2)

ans3 <- gammaImpute(formula(paste("Surv(communicating_whenevent,communicating_isevent==1)","~",paste(names(communicating_final_sub)[c(3:15)],collapse="+"))),data=communicating_final_sub,m=2 , gamma.factor=2,DCO.time=max(communicating_final_sub$communicating_whenevent))
b3=ExtractSingle(ans3,index=1)
b3=b3$data
training3=subset(b3,select=-c(1:15,internalDCO.time,internal_gamma_val))
names(training3)[c(length(names(training3))-1,length(names(training3)))]=c("when","is")
communicating_cox=coxph(Surv(when,is==1)~.,data=training3)

ans4 <- gammaImpute(formula(paste("Surv(breathing_whenevent,breathing_isevent==1)","~",paste(names(breathing_final_sub)[c(3:15)],collapse="+"))),data=breathing_final_sub,m=2 , gamma.factor=1,DCO.time=max(breathing_final_sub$breathing_whenevent))
b4=ExtractSingle(ans4,index=1)
b4=b4$data
training4=subset(b4,select=-c(1:15,internalDCO.time,internal_gamma_val))
names(training4)[c(length(names(training4))-1,length(names(training4)))]=c("when","is")
breathing_cox=coxph(Surv(when,is==1)~.,data=training4)

ans5 <- gammaImpute(formula(paste("Surv(when,is==1)","~",paste(names(death_final_sub)[c(3:13)],collapse="+"))),data=death_final_sub,m=2 , gamma.factor=1,DCO.time=max(death_final_sub$when))
b5=ExtractSingle(ans5,index=1)
b5=b5$data
training5=subset(b5,select=-c(1:13,internalDCO.time,internal_gamma_val))
names(training5)[c(length(names(training5))-1,length(names(training5)))]=c("when","is")
death_cox=coxph(Surv(when,is==1)~.,data=training5)


#movement_cox1=coxph(Surv(movement_whenevent,movement_isevent==1)~.,data=movement_final_sub[,-c(3:15)])
#swallowing_cox1=coxph(Surv(swallowing_whenevent,swallowing_isevent==1)~.,data=swallowing_final_sub[,-c(3:15)])
#communicating_cox1=coxph(Surv(communicating_whenevent,communicating_isevent==1)~.,data=communicating_final_sub[,-c(3:15)])
#breathing_cox1=coxph(Surv(breathing_whenevent,breathing_isevent==1)~.,data=breathing_final_sub[,-c(3:15)])
#death_cox1=coxph(Surv(when,is==1)~.,data=death_final_sub[,-c(3:13)])

movement_cox1=coxph(Surv(movement_whenevent,movement_isevent==1)~.,data=movement_final_sub)
swallowing_cox1=coxph(Surv(swallowing_whenevent,swallowing_isevent==1)~.,data=swallowing_final_sub)
communicating_cox1=coxph(Surv(communicating_whenevent,communicating_isevent==1)~.,data=communicating_final_sub)
breathing_cox1=coxph(Surv(breathing_whenevent,breathing_isevent==1)~.,data=breathing_final_sub)
death_cox1=coxph(Surv(when,is==1)~.,data=death_final_sub)





movement_cox1=ranger(Surv(movement_whenevent,movement_isevent==1)~.,data=movement_final_sub,importance="permutation")
swallowing_cox1=ranger(Surv(swallowing_whenevent,swallowing_isevent==1)~.,data=swallowing_final_sub,importance="permutation")
communicating_cox1=ranger(Surv(communicating_whenevent,communicating_isevent==1)~.,data=communicating_final_sub,importance="permutation")
breathing_cox1=ranger(Surv(breathing_whenevent,breathing_isevent==1)~.,data=breathing_final_sub,importance="permutation")
death_cox1=ranger(Surv(when,is==1)~.,data=death_final_sub,importance="permutation")

aaa=breathing_cox1$variable.importance[breathing_cox1$variable.importance>0.0015] 
aaa=aaa[order(aaa)]

par(las=2)
par(mar=c(4,8,1,1))
barplot(aaa,hori=TRUE,las=1,cex.names=1,main="Variable Importance : Death")



#plot(survfit(Surv(movement_final_sub_imputed$impute.time,movement_final_sub_imputed$impute.event)~1),conf.int=FALSE)
#lines(survfit(Surv(swallowing_final_sub_imputed$impute.time,swallowing_final_sub_imputed$impute.event)~1),conf.int=FALSE,col="red")
#lines(survfit(Surv(communicating_final_sub_imputed$impute.time,communicating_final_sub_imputed$impute.event)~1),conf.int=FALSE,col="orange")
#lines(survfit(Surv(breathing_final_sub_imputed$impute.time,breathing_final_sub_imputed$impute.event)~1),conf.int=FALSE, col="yellow")
#lines(survfit(Surv(death_final_sub_imputed$impute.time,death_final_sub_imputed$impute.event)~1),conf.int=FALSE, col="green")

#allmovement_cox=coxph(Surv(impute.time,impute.event==1)~.,data=movement_final_sub_imputed)
#allswallowing_cox=coxph(Surv(impute.time,impute.event==1)~.,data=swallowing_final_sub_imputed)
#allcommunicating_cox=coxph(Surv(impute.time,impute.event==1)~.,data=communicating_final_sub_imputed)
#allbreathing_cox=coxph(Surv(impute.time,impute.event==1)~.,data=breathing_final_sub_imputed)
#alldeath_cox=coxph(Surv(impute.time,impute.event==1)~.,data=death_final_sub_imputed)

#allmovement_ranger=ranger(Surv(when,is==1)~.,data=movement_final_sub)
#allswallowing_ranger=ranger(Surv(when,is==1)~.,data=swallowing_final_sub)
#allcommunicating_ranger=ranger(Surv(when,is==1)~.,data=communicating_final_sub)
#allbreathing_ranger=ranger(Surv(when,is==1)~.,data=breathing_final_sub)
#alldeath_ranger=ranger(Surv(when,is==1)~.,data=death_final_sub)

i=100
plot(survfit(breathing_cox1,allpatientsfeature_imputed[i,]),xlab="Time",ylab="Proportion non-loss",conf.int=FALSE)
lines(survfit(movement_cox1,allpatientsfeature_imputed[i,]),col="green",conf.int=FALSE)
lines(survfit(swallowing_cox1,allpatientsfeature_imputed[i,]),col="red",conf.int=FALSE)
lines(survfit(communicating_cox1,allpatientsfeature_imputed[i,]),col="blue",conf.int=FALSE)
lines(survfit(death_cox1,allpatientsfeature_imputed[i,]),col=6,conf.int=FALSE)
legend("topright",c("Movement","Swallowing","Communicating","Breathing","Death"),lwd=2,bty="n",col=c("green","red","blue","black",6))


movement_curve=survfit(movement_cox1,newdata=allpatientsfeature_imputed)
movement_prob=movement_curve$surv[sindex(movement_curve$time,30.5 * 12),]
swallowing_curve=survfit(swallowing_cox1,newdata=allpatientsfeature_imputed)
swallowing_prob=swallowing_curve$surv[sindex(swallowing_curve$time,30.5 * 12),]
communicating_curve=survfit(communicating_cox1,newdata=allpatientsfeature_imputed)
communicating_prob=communicating_curve$surv[sindex(communicating_curve$time,30.5 * 12),]
breathing_curve=survfit(breathing_cox1,newdata=allpatientsfeature_imputed)
breathing_prob=breathing_curve$surv[sindex(breathing_curve$time,30.5 * 12),]
death_curve=survfit(death_cox1,newdata=allpatientsfeature_imputed)
death_prob=death_curve$surv[sindex(death_curve$time,30.5 * 12),]

#movement_curve=predict(allmovement_ranger,allpatientsfeature_imputed)
#movement_prob=movement_curve$survival[,sindex(movement_curve$unique.death.times,30.5 * 12)]
#rm(movement_curve)
#gc()
#swallowing_curve=predict(allswallowing_ranger,allpatientsfeature_imputed)
#swallowing_prob=swallowing_curve$survival[,sindex(swallowing_curve$unique.death.times,30.5 * 12)]
#rm(swallowing_curve)
#gc()
#communicating_curve=predict(allcommunicating_ranger,allpatientsfeature_imputed)
#communicating_prob=communicating_curve$survival[,sindex(communicating_curve$unique.death.times,30.5 * 12)]
#rm(communicating_curve)
#gc()
#breathing_curve=predict(allbreathing_ranger,allpatientsfeature_imputed)
#breathing_prob=breathing_curve$survival[,sindex(breathing_curve$unique.death.times,30.5 * 12)]
#rm(breathing_curve)
#gc()
#death_curve=predict(alldeath_ranger,allpatientsfeature_imputed)
#death_prob=death_curve$survival[,sindex(death_curve$unique.death.times,30.5 * 12)]
#rm(death_curve)
#gc()

movement_prob=1-movement_prob
swallowing_prob=1-swallowing_prob
communicating_prob=1-communicating_prob
breathing_prob=1-breathing_prob
death_prob=1-death_prob

movement_prob_scaled=scale(movement_prob)
swallowing_prob_scaled=scale(swallowing_prob)
communicating_prob_scaled=scale(communicating_prob)
breathing_prob_scaled=scale(breathing_prob)
death_prob_scaled=scale(death_prob)

allprob=data.frame("SubjectID"=allpatientsfeature_imputed$SubjectID,movement_prob,swallowing_prob,communicating_prob,breathing_prob,death_prob)
allprob_scaled=data.frame("SubjectID"=allpatientsfeature_imputed$SubjectID,movement_prob_scaled,swallowing_prob_scaled,communicating_prob_scaled,breathing_prob_scaled,death_prob_scaled)

#allprob <- mutate(group_by(allprob,SubjectID),top=which(rank(c(movement_prob,swallowing_prob,communicating_prob,breathing_prob,death_prob))==5))
#hist(allprob$top)

wss<-0
for ( i in 1:15){
  km.out <- kmeans(scale(allprob[,-1]),centers=i,nstart=20)
  wss[i] <- km.out$tot.withinss
}
plot(1:15, wss, type="b")
km.out <- kmeans(allprob[,-1],centers=6,nstart=20)

wss_scaled<-0
for ( i in 1:15){
  km.out_scaled <- kmeans(allprob_scaled[,-1],centers=i,nstart=20)
  wss_scaled[i] <- km.out_scaled$tot.withinss
}
plot(1:15, wss_scaled, type="b")
km.out_scaled <- kmeans(allprob_scaled[,-1],centers=3,nstart=20)
table(km.out$cluster,km.out_scaled$cluster)


allcluster <- data.frame(allpatientsfeature_imputed,allprob[,-1],km.out$cluster)
allcluster$km.out.cluster <- factor(allcluster$km.out.cluster)
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
plot(allcluster$km.out.cluster~allcluster$onset_site)

plot(allcluster$km.out.cluster ~ allcluster$death_prob)
barplot(c("movement"=mean(allcluster[allcluster$km.out.cluster==1,]$movement_prob),"swallowing"=mean(allcluster[allcluster$km.out.cluster==1,]$swallowing_prob),"communi"=mean(allcluster[allcluster$km.out.cluster==1,]$communicating_prob),"breathing"=mean(allcluster[allcluster$km.out.cluster==1,]$breathing_prob),"death"=mean(allcluster[allcluster$km.out.cluster==1,]$death_prob)),ylim=c(0,1),main="Cluster 1")
barplot(c("movement"=mean(allcluster[allcluster$km.out.cluster==2,]$movement_prob),"swallowing"=mean(allcluster[allcluster$km.out.cluster==2,]$swallowing_prob),"communi"=mean(allcluster[allcluster$km.out.cluster==2,]$communicating_prob),"breathing"=mean(allcluster[allcluster$km.out.cluster==2,]$breathing_prob),"death"=mean(allcluster[allcluster$km.out.cluster==2,]$death_prob)),ylim=c(0,1),main="Cluster 2")
barplot(c("movement"=mean(allcluster[allcluster$km.out.cluster==3,]$movement_prob),"swallowing"=mean(allcluster[allcluster$km.out.cluster==3,]$swallowing_prob),"communi"=mean(allcluster[allcluster$km.out.cluster==3,]$communicating_prob),"breathing"=mean(allcluster[allcluster$km.out.cluster==3,]$breathing_prob),"death"=mean(allcluster[allcluster$km.out.cluster==3,]$death_prob)),ylim=c(0,1),main="Cluster 3")

boxplot(allcluster$movement_prob~allcluster$km.out.cluster,outline=FALSE)
boxplot(allcluster$swallowing_prob~allcluster$km.out.cluster,outline=FALSE)
boxplot(allcluster$communicating_prob~allcluster$km.out.cluster,outline=FALSE)
boxplot(allcluster$breathing_prob~allcluster$km.out.cluster,outline=FALSE)
boxplot(allcluster$death_prob~allcluster$km.out.cluster,outline=FALSE)



hclust.out <- hclust(dist(scale(allprob[,-1])))
plot(hclust.out)
hclust=cutree(hclust.out,k=4)
table(km.out$cluster,hclust)
allclusterbyh <- data.frame(allpatientsfeature_imputed,allprob[,-1],hclust)
allclusterbyh$hclust <- factor(allclusterbyh$hclust)

plot(allclusterbyh$onset_site~allclusterbyh$hclust)

boxplot(allclusterbyh$movement_prob~allclusterbyh$hclust,outline=FALSE)
boxplot(allclusterbyh$swallowing_prob~allclusterbyh$hclust,outline=FALSE)
boxplot(allclusterbyh$communicating_prob~allclusterbyh$hclust,outline=FALSE)
boxplot(allclusterbyh$breathing_prob~allclusterbyh$hclust,outline=FALSE)
boxplot(allclusterbyh$death_prob~allclusterbyh$hclust,outline=FALSE)

barplot(c("movement"=mean(allclusterbyh[allclusterbyh$hclust==1,]$movement_prob),"swallowing"=mean(allclusterbyh[allclusterbyh$hclust==1,]$swallowing_prob),"communi"=mean(allclusterbyh[allclusterbyh$hclust==1,]$communicating_prob),"breathing"=mean(allclusterbyh[allclusterbyh$hclust==1,]$breathing_prob),"death"=mean(allclusterbyh[allclusterbyh$hclust==1,]$death_prob)),ylim=c(0,1),main="Cluster 1")
barplot(c("movement"=mean(allclusterbyh[allclusterbyh$hclust==2,]$movement_prob),"swallowing"=mean(allclusterbyh[allclusterbyh$hclust==2,]$swallowing_prob),"communi"=mean(allclusterbyh[allclusterbyh$hclust==2,]$communicating_prob),"breathing"=mean(allclusterbyh[allclusterbyh$hclust==2,]$breathing_prob),"death"=mean(allclusterbyh[allclusterbyh$hclust==2,]$death_prob)),ylim=c(0,1),main="Cluster 2")
barplot(c("movement"=mean(allclusterbyh[allclusterbyh$hclust==3,]$movement_prob),"swallowing"=mean(allclusterbyh[allclusterbyh$hclust==3,]$swallowing_prob),"communi"=mean(allclusterbyh[allclusterbyh$hclust==3,]$communicating_prob),"breathing"=mean(allclusterbyh[allclusterbyh$hclust==3,]$breathing_prob),"death"=mean(allclusterbyh[allclusterbyh$hclust==3,]$death_prob)),ylim=c(0,1),main="Cluster 3")
barplot(c("movement"=mean(allclusterbyh[allclusterbyh$hclust==4,]$movement_prob),"swallowing"=mean(allclusterbyh[allclusterbyh$hclust==4,]$swallowing_prob),"communi"=mean(allclusterbyh[allclusterbyh$hclust==4,]$communicating_prob),"breathing"=mean(allclusterbyh[allclusterbyh$hclust==4,]$breathing_prob),"death"=mean(allclusterbyh[allclusterbyh$hclust==4,]$death_prob)),ylim=c(0,1),main="Cluster 4")


