library(dplyr)
library(tidyr)
deathdata <-read.csv("DeathData.csv")
deathfrs <- read.csv("alsfrs.csv")
deathadverse <- read.csv("AdverseEvents.csv")
deathconmed <- read.csv("ConMeds.csv")
deathfvc <- read.csv("Fvc.csv")
deathlab <- read.csv("Labs.csv")
deathriluzole <- read.csv("Riluzole.csv")
deathsvc <- read.csv("Svc.csv")
deathfam <- read.csv("FamilyHistory.csv")
deathtr <- read.csv("Treatment.csv")
deathvital <- read.csv("VitalSigns.csv")
deathfrs <- subset(deathfrs,select=c("subject_id","ALSFRS_Delta"))
deathconmed <- subset(deathconmed,!(!is.na(Stop_Delta)&(Stop_Delta==73299 | Stop_Delta==1096331)),select=c("subject_id","Start_Delta","Stop_Delta"))
deathfvc <- subset(deathfvc,select=c("subject_id","Forced_Vital_Capacity_Delta"))
deathlab <- subset(deathlab,select=c("subject_id","Laboratory_Delta"))
deathriluzole <- subset(deathriluzole,select=c("subject_id","Riluzole_use_Delta"))
deathsvc <- subset(deathsvc, select=c("subject_id","Slow_vital_Capacity_Delta"))
deathfam <- subset(deathfam, select=c("subject_id","Family_History_Delta"))
deathtr <- subset(deathtr, select=c("subject_id","Treatment_Group_Delta"))
deathvital <- subset(deathvital, select=c("subject_id","Vital_Signs_Delta"))
d1 <- summarize(group_by(deathfrs,subject_id),"max1"=max(ALSFRS_Delta,na.rm=TRUE))
d2 <- summarize(group_by(deathadverse,subject_id),"max2"=max(max(Start_Date_Delta,na.rm=TRUE),max(End_Date_Delta,na.rm=TRUE)))
d3 <- summarize(group_by(deathconmed,subject_id),"max3"=max(max(Start_Delta,na.rm=TRUE),max(Stop_Delta,na.rm=TRUE)))
d4 <- summarize(group_by(deathfvc,subject_id),"max4"=max(Forced_Vital_Capacity_Delta,na.rm=TRUE))
d5 <- summarize(group_by(deathlab,subject_id),"max5"=max(Laboratory_Delta,na.rm=TRUE))
d6 <- summarize(group_by(deathriluzole,subject_id),"max6"=max(Riluzole_use_Delta,na.rm=TRUE))
d7 <- summarize(group_by(deathsvc,subject_id),"max7"=max(Slow_vital_Capacity_Delta,na.rm=TRUE))
d8 <- summarize(group_by(deathfam,subject_id),"max8"=max(Family_History_Delta,na.rm=TRUE))
d9 <- summarize(group_by(deathtr,subject_id),"max9"=max(Treatment_Group_Delta,na.rm=TRUE))
d10 <- summarize(group_by(deathvital,subject_id),"max10"=max(Vital_Signs_Delta,na.rm=TRUE))
deathfull <- merge(d1,d2,all=TRUE)
deathfull <- merge(deathfull, d3, all=TRUE)
deathfull <- merge(deathfull, d4, all=TRUE)
deathfull <- merge(deathfull, d5, all=TRUE)
deathfull <- merge(deathfull, d6, all=TRUE)
deathfull <- merge(deathfull, d7, all=TRUE)
deathfull <- merge(deathfull, d8, all=TRUE)
deathfull <- merge(deathfull, d9, all=TRUE)
deathfull <- merge(deathfull, d10, all=TRUE)


#death : 사망 날짜. 최종은 deathdata2
adsub <- filter(deathadverse,Outcome=="Death"|High_Level_Term=="Death and sudden death")
##tracheo add later. adsub2 <- filter(deathadverse, Lowest_Level_Term=="Tracheostomy")
death <-summarize(group_by(adsub,subject_id),"when"=max(max(Start_Date_Delta,na.rm=TRUE),max(End_Date_Delta,na.rm=TRUE)))
#death2 <- s

deathdata2 <- merge(filter(deathdata,!is.na(Death_Days)), death, all=TRUE)
deathdata2$Death_Days[is.na(deathdata2$Death_Days)]=deathdata2$when[is.na(deathdata2$Death_Days)]
deathdata2 <-subset(deathdata2,select=-when)
deathdata2$Subject_Died="Yes"


# deathfull2 : 최후 관측 날
deathfull2<-summarize(group_by(gather(deathfull,key, value,max1:max10),subject_id),"max"=max(value,na.rm=TRUE))
deathfull2 <- filter(deathfull2,max!=-Inf)

# some are Death_days != max. Later I may have to add tracheostomy data as detah from alsfrs data.
#merge(deathdata2,deathfull2,all=TRUE)
death_survival=merge(deathdata2,deathfull2,all=TRUE)
death_survival$Subject_Died[is.na(death_survival$Subject_Died)]="No"
death_survival$Death_Days[death_survival$Subject_Died=="No"]=death_survival$max[death_survival$Subject_Died=="No"]
death_survival<-subset(death_survival,select=-max)
death_survival$Subject_Died[death_survival$Subject_Died=="Yes"]=1
death_survival$Subject_Died[death_survival$Subject_Died=="No"]=0
death_survival <- death_survival[order(death_survival$Death_Days),]
death_survival$Subject_Died <- as.numeric(death_survival$Subject_Died)
names(death_survival) <- c("subject_id","is","when")
death_survival <- subset(death_survival,select=c("subject_id","when","is"))
death_survival_sub <- droplevels(filter(death_survival,when>0))

death.survfit=survfit(Surv(when,is==1)~1,data=death_survival_sub)
plot(death.survfit,xlab="Time",ylab="Proportion surviving")
