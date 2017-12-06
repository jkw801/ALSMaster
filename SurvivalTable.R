library(dplyr)
library(tidyr)
library(survival)
data.allforms_training<-read.delim("all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("all_forms_PROACT_validation_full.txt",sep="|", header=T)
data.allforms <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)

data.ALSslope_training<-read.delim("ALSFRS_slope_PROACT_training.txt",sep="|", header=T) 
data.ALSslope_training2<-read.delim("ALSFRS_slope_PROACT_training2.txt",sep="|", header=T) 
data.ALSslope_leaderboard<-read.delim("ALSFRS_slope_PROACT_leaderboard.txt",sep="|", header=T) 
data.ALSslope_validation<-read.delim("ALSFRS_slope_PROACT_validation.txt",sep="|", header=T) 
data.ALSslope <-rbind(data.ALSslope_training,data.ALSslope_training2,data.ALSslope_leaderboard,data.ALSslope_validation)

data.alsfrs1 <- droplevels(data.allforms[data.allforms$form_name=="ALSFRS", ])
data.alsfrs1 = subset(data.alsfrs1,select=-c(form_name,feature_unit))
data.alsfrs1$feature_value <- as.numeric(as.character(data.alsfrs1$feature_value))
data.alsfrs1$feature_delta <- as.numeric(as.character(data.alsfrs1$feature_delta))
data.alsfrs1 <- subset(data.alsfrs1,!(SubjectID==137943 & feature_delta==371))

a <- mutate(group_by(data.alsfrs1,SubjectID,feature_name,feature_delta),n=n())
b=subset(a,n>1)
c=subset(a,n==1)

c=subset(c,select=-n)
b <- summarize(b,feature_value=mean(feature_value,na.rm=TRUE))
c <- as.data.frame(c)
b <- as.data.frame(b)
d <- rbind(c,b)
alsfrsfull <- spread(d,feature_name,feature_value)

alsfrsfull <- droplevels(filter(alsfrsfull, !is.na(feature_delta)))
alsfrsfull <- droplevels(filter(alsfrsfull,!is.na(Q1_Speech)&!is.na(Q2_Salivation)&!is.na(Q3_Swallowing)&!is.na(Q4_Handwriting)&(!(is.na(Q5a_Cutting_without_Gastrostomy)&is.na(Q5b_Cutting_with_Gastrostomy)))&!is.na(Q6_Dressing_and_Hygiene)&!is.na(Q7_Turning_in_Bed)&!is.na(Q8_Walking)&!is.na(Q9_Climbing_Stairs)&!(is.na(Q10_Respiratory)&(is.na(R1_Dyspnea)|is.na(R2_Orthopnea)|is.na(R3_Respiratory_Insufficiency)))))


alsfrsfull$Q10R1=ifelse(alsfrsfull$R1_Dyspnea==4,4,ifelse(alsfrsfull$R1_Dyspnea>1,3,ifelse(alsfrsfull$R1_Dyspnea>0,2,1)))
alsfrsfull$Q10R3=ifelse(alsfrsfull$R3_Respiratory_Insufficiency==2,ifelse(alsfrsfull$R1_Dyspnea>1,2,1),ifelse(alsfrsfull$R3_Respiratory_Insufficiency<2 & alsfrsfull$R3_Respiratory_Insufficiency>0,1,ifelse(alsfrsfull$R3_Respiratory_Insufficiency==0,0,NA)))
alsfrsfull$Q10R3[is.na(alsfrsfull$Q10R3)]=alsfrsfull$Q10R1[is.na(alsfrsfull$Q10R3)]  
alsfrsfull$Q10R3[is.na(alsfrsfull$Q10R3)]=alsfrsfull$Q10_Respiratory[is.na(alsfrsfull$Q10R3)]
alsfrsfull$Q10R=alsfrsfull$Q10R3
alsfrsfull <- subset(alsfrsfull,select=-c(Q10R1,Q10R3))
alsfrsfull$ALSFRS_TotalR = alsfrsfull$ALSFRS_Total - alsfrsfull$respiratory + alsfrsfull$Q10R

# King's staging
alsfrsfull$kingbulbar <- (alsfrsfull$Q1_Speech<4) | (alsfrsfull$Q2_Salivation<4) | (alsfrsfull$Q3_Swallowing<4) 
alsfrsfull$kingulimb[is.na(alsfrsfull$Q5a_Cutting_without_Gastrostomy)] <- (alsfrsfull$Q4_Handwriting[is.na(alsfrsfull$Q5a_Cutting_without_Gastrostomy)]<4)
alsfrsfull$kingulimb[!is.na(alsfrsfull$Q5a_Cutting_without_Gastrostomy)] <- (alsfrsfull$Q4_Handwriting[!is.na(alsfrsfull$Q5a_Cutting_without_Gastrostomy)]<4) | (alsfrsfull$Q5a_Cutting_without_Gastrostomy[!is.na(alsfrsfull$Q5a_Cutting_without_Gastrostomy)]<4)
alsfrsfull$kingllimb <- (alsfrsfull$Q8_Walking<4)
alsfrsfull$kingnut <- !is.na(alsfrsfull$Q5b_Cutting_with_Gastrostomy)

# revise needed below
alsfrsfull$kingres <- (alsfrsfull$R1_Dyspnea==0) | (alsfrsfull$R3_Respiratory_Insufficiency<4)
alsfrsfull$kingres[is.na(alsfrsfull$kingres)] <- (alsfrsfull$Q10_Respiratory[is.na(alsfrsfull$kingres)] <=1 )

alsfrsfull$kings=ifelse(alsfrsfull$kingres==1 | alsfrsfull$kingnut==1,4,alsfrsfull$kingbulbar+alsfrsfull$kingllimb+alsfrsfull$kingulimb)


## 3점스케일 후 Q1~Q3/Q4~Q9/Q10~Q12 3개 변수 추가해보자.
alsfrsfull$temp1 <- ifelse(alsfrsfull$Q1_Speech==4,2,ifelse(alsfrsfull$Q1_Speech>=2,1,0))
alsfrsfull$temp2 <- ifelse(alsfrsfull$Q2_Salivation==4,2,ifelse(alsfrsfull$Q2_Salivation>=2,1,0))
alsfrsfull$temp3 <- ifelse(alsfrsfull$Q3_Swallowing==4,2,ifelse(alsfrsfull$Q3_Swallowing>=2,1,0))
alsfrsfull$temp4 <- ifelse(alsfrsfull$Q4_Handwriting==4,2,ifelse(alsfrsfull$Q4_Handwriting>=2,1,0))
alsfrsfull$temp5 <- ifelse(alsfrsfull$Q5_Cutting==4,2,ifelse(alsfrsfull$Q5_Cutting>=2,1,0))
alsfrsfull$temp6 <- ifelse(alsfrsfull$Q6_Dressing_and_Hygiene==4,2,ifelse(alsfrsfull$Q6_Dressing_and_Hygiene>=2,1,0))
alsfrsfull$temp7 <- ifelse(alsfrsfull$Q7_Turning_in_Bed==4,2,ifelse(alsfrsfull$Q7_Turning_in_Bed>=2,1,0))
alsfrsfull$temp8 <- ifelse(alsfrsfull$Q8_Walking==4,2,ifelse(alsfrsfull$Q8_Walking>=2,1,0))
alsfrsfull$temp9 <- ifelse(alsfrsfull$Q9_Climbing_Stairs==4,2,ifelse(alsfrsfull$Q9_Climbing_Stairs>=2,1,0))
alsfrsfull$temp10 <- ifelse(alsfrsfull$Q10_Respiratory==4,2,ifelse(alsfrsfull$Q10_Respiratory>=2,1,0))
alsfrsfull$temp11 <- ifelse(alsfrsfull$R1_Dyspnea==4,2,ifelse(alsfrsfull$R1_Dyspnea>=1,1,0))
alsfrsfull$temp12 <- ifelse(alsfrsfull$R2_Orthopnea==4,2,ifelse(alsfrsfull$R2_Orthopnea>=2,1,0))
alsfrsfull$temp13 <- ifelse(alsfrsfull$R3_Respiratory_Insufficiency==4,2,ifelse(alsfrsfull$R3_Respiratory_Insufficiency>=1,1,0))

alsfrsfull$multibulbar <- (alsfrsfull$temp1 + alsfrsfull$temp2 + alsfrsfull$temp3)/3
alsfrsfull$multimotor <- (alsfrsfull$temp4+alsfrsfull$temp5+alsfrsfull$temp6+alsfrsfull$temp7+alsfrsfull$temp8+alsfrsfull$temp9)/6
alsfrsfull$multirespi=alsfrsfull$temp10
alsfrsfull$multirespi[is.na(alsfrsfull$multirespi)]=alsfrsfull$temp11[is.na(alsfrsfull$multirespi)]
alsfrsfull <- subset(alsfrsfull,select=-c(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11,temp12,temp13))


# 4개의 functional domain ?????? 변??? 추???
Movement <- (alsfrsfull$Q8_Walking<=1) | (alsfrsfull$Q6_Dressing_and_Hygiene<=1)
Swallowing <- (alsfrsfull$Q3_Swallowing<=1)
Communicating <- (alsfrsfull$Q1_Speech<=1) & (alsfrsfull$Q4_Handwriting<=1)
Breathing <- (alsfrsfull$R1_Dyspnea<=1) | (alsfrsfull$R3_Respiratory_Insufficiency<=2)
Breathing[is.na(Breathing)]=(alsfrsfull$Q10_Respiratory[is.na(Breathing)]<=2)

ALSMITOS <- Movement + Swallowing + Communicating + Breathing
alsfrsfull <- mutate(alsfrsfull, Movement,Swallowing,Communicating,Breathing,ALSMITOS)



# Movement ???존분?????? ?????? table 만들??? : movement_survival_sub가 최종
group<-group_by(alsfrsfull,SubjectID)
movement_isevent <- summarize(group,any(Movement==1))
movement_whenevent1 <- summarize(group, min(feature_delta[which(Movement==1)]),min(feature_delta))
movement_whenevent2 <- summarize(group, max(feature_delta[which(Movement==0)]),min(feature_delta))
movement_whenevent <- vector()
movement_deltaleast <- vector()
for (i in 1:length(movement_isevent[[2]])){
  if (movement_isevent[[2]][i]==0) {
    movement_whenevent[i]=movement_whenevent2[[2]][i]
    movement_deltaleast[i]=movement_whenevent2[[3]][i]
  }
  else if (movement_isevent[[2]][i]==1){
    movement_whenevent[i]=movement_whenevent1[[2]][i]
    movement_deltaleast[i]=movement_whenevent1[[3]][i]
  }
}
movement_survival <- data.frame("SubjectID"=movement_isevent[[1]],movement_whenevent,movement_deltaleast,"movement_isevent"=movement_isevent[[2]])
movement_survival=movement_survival[order(movement_survival$movement_whenevent),]

# left-censoring ??????
movement_survival <- droplevels(subset(movement_survival,!(movement_isevent & movement_whenevent==movement_deltaleast)))
movement_survival <- subset(movement_survival,select=-movement_deltaleast)


# 0???????????? censoring??? ?????? ??????.???마도 ????????????....????????? 콕스??? ????????? parametric??? ??????. 그래??? ?????? ??????.
movement_survival_sub <-droplevels(filter(movement_survival,movement_whenevent>=92))


# ????????? 그냥 ??????  그려??? K-M plot
movement.survfit=survfit(Surv(movement_whenevent,movement_isevent==1)~1,data=movement_survival_sub)
plot(movement.survfit,xlab="Time",ylab="Proportion surviving")

# Swallowing ???존분?????? ?????? table 만들??? : swallowing_survival_sub가 최종
group<-group_by(alsfrsfull,SubjectID)
swallowing_isevent <- summarize(group,any(Swallowing==1))
swallowing_whenevent1 <- summarize(group, min(feature_delta[which(Swallowing==1)]),min(feature_delta))
swallowing_whenevent2 <- summarize(group, max(feature_delta[which(Swallowing==0)]),min(feature_delta))
swallowing_whenevent <- vector()
swallowing_deltaleast <- vector()
for (i in 1:length(swallowing_isevent[[2]])){
  if (swallowing_isevent[[2]][i]==0) {
    swallowing_whenevent[i]=swallowing_whenevent2[[2]][i]
    swallowing_deltaleast[i]=swallowing_whenevent2[[3]][i]
  }
  else if (swallowing_isevent[[2]][i]==1){
    swallowing_whenevent[i]=swallowing_whenevent1[[2]][i]
    swallowing_deltaleast[i]=swallowing_whenevent1[[3]][i]
  }
}
swallowing_survival <- data.frame("SubjectID"=swallowing_isevent[[1]],swallowing_whenevent,swallowing_deltaleast,"swallowing_isevent"=swallowing_isevent[[2]])
swallowing_survival=swallowing_survival[order(swallowing_survival$swallowing_whenevent),]

# left-censoring ??????
swallowing_survival <- droplevels(subset(swallowing_survival,!(swallowing_isevent & swallowing_whenevent==swallowing_deltaleast)))
swallowing_survival <- subset(swallowing_survival,select=-swallowing_deltaleast)


# 0???????????? censoring??? ?????? ??????.???마도 ????????????....????????? 콕스??? ????????? parametric??? ??????. 그래??? ?????? ??????.
swallowing_survival_sub <-droplevels(filter(swallowing_survival,swallowing_whenevent>=92))


# ????????? 그냥 ??????  그려??? K-M plot
swallowing.survfit=survfit(Surv(swallowing_whenevent,swallowing_isevent==1)~1,data=swallowing_survival_sub)
plot(swallowing.survfit,xlab="Time",ylab="Proportion surviving")

# Communicating ???존분?????? ?????? table 만들??? : communicating_survival_sub가 최종
group<-group_by(alsfrsfull,SubjectID)
communicating_isevent <- summarize(group,any(Communicating==1))
communicating_whenevent1 <- summarize(group, min(feature_delta[which(Communicating==1)]),min(feature_delta))
communicating_whenevent2 <- summarize(group, max(feature_delta[which(Communicating==0)]),min(feature_delta))
communicating_whenevent <- vector()
communicating_deltaleast <- vector()
for (i in 1:length(communicating_isevent[[2]])){
  if (communicating_isevent[[2]][i]==0) {
    communicating_whenevent[i]=communicating_whenevent2[[2]][i]
    communicating_deltaleast[i]=communicating_whenevent2[[3]][i]
  }
  else if (communicating_isevent[[2]][i]==1){
    communicating_whenevent[i]=communicating_whenevent1[[2]][i]
    communicating_deltaleast[i]=communicating_whenevent1[[3]][i]
  }
}
communicating_survival <- data.frame("SubjectID"=communicating_isevent[[1]],communicating_whenevent,communicating_deltaleast,"communicating_isevent"=communicating_isevent[[2]])
communicating_survival=communicating_survival[order(communicating_survival$communicating_whenevent),]

# left-censoring ??????
communicating_survival <- droplevels(subset(communicating_survival,!(communicating_isevent & communicating_whenevent==communicating_deltaleast)))
communicating_survival <- subset(communicating_survival,select=-communicating_deltaleast)


# 0???????????? censoring??? ?????? ??????.???마도 ????????????....????????? 콕스??? ????????? parametric??? ??????. 그래??? ?????? ??????.
communicating_survival_sub <-droplevels(filter(communicating_survival,communicating_whenevent>=92))


# ????????? 그냥 ??????  그려??? K-M plot
communicating.survfit=survfit(Surv(communicating_whenevent,communicating_isevent==1)~1,data=communicating_survival_sub)
plot(communicating.survfit,xlab="Time",ylab="Proportion surviving")

# breathing ???존분?????? ?????? table 만들??? : breathing_survival_sub가 최종
group<-group_by(alsfrsfull,SubjectID)
breathing_isevent <- summarize(group,any(Breathing==1))
breathing_whenevent1 <- summarize(group, min(feature_delta[which(Breathing==1)]),min(feature_delta))
breathing_whenevent2 <- summarize(group, max(feature_delta[which(Breathing==0)]),min(feature_delta))
breathing_whenevent <- vector()
breathing_deltaleast <- vector()
for (i in 1:length(breathing_isevent[[2]])){
  if (breathing_isevent[[2]][i]==0) {
    breathing_whenevent[i]=breathing_whenevent2[[2]][i]
    breathing_deltaleast[i]=breathing_whenevent2[[3]][i]
  }
  else if (breathing_isevent[[2]][i]==1){
    breathing_whenevent[i]=breathing_whenevent1[[2]][i]
    breathing_deltaleast[i]=breathing_whenevent1[[3]][i]
  }
}
breathing_survival <- data.frame("SubjectID"=breathing_isevent[[1]],breathing_whenevent,breathing_deltaleast,"breathing_isevent"=breathing_isevent[[2]])
breathing_survival=breathing_survival[order(breathing_survival$breathing_whenevent),]

# left-censoring ??????
breathing_survival <- droplevels(subset(breathing_survival,!(breathing_isevent & breathing_whenevent==breathing_deltaleast)))
breathing_survival <- subset(breathing_survival,select=-breathing_deltaleast)


# 0???????????? censoring??? ?????? ??????.???마도 ????????????....????????? 콕스??? ????????? parametric??? ??????. 그래??? ?????? ??????.
breathing_survival_sub <-droplevels(filter(breathing_survival,breathing_whenevent>=92))


# ????????? 그냥 ??????  그려??? K-M plot
breathing.survfit=survfit(Surv(breathing_whenevent,breathing_isevent==1)~1,data=breathing_survival_sub)
plot(breathing.survfit,xlab="Time",ylab="Proportion surviving")


# death ???존분?????? ?????? table 만들??? : breathing_survival가 최종
data.surv_training<-read.delim("surv_response_PROACT_training.txt",sep="|", header=T) 
data.surv_training2<-read.delim("surv_response_PROACT_training2.txt",sep="|", header=T) 
data.surv_leaderboard<-read.delim("surv_response_PROACT_leader.txt",sep="|", header=T) 
data.surv_validation<-read.delim("surv_response_PROACT_validation.txt",sep="|", header=T) 
death_survival <- rbind(data.surv_training,data.surv_training2,data.surv_leaderboard,data.surv_validation)
names(death_survival) <- c("SubjectID","when","is")
death.survfit=survfit(Surv(when,is==1)~1,data=death_survival)
plot(death.survfit,xlab="Time",ylab="Proportion surviving")


# all plot
plot(breathing.survfit,xlab="Time",ylab="Proportion surviving",conf.int=FALSE)
lines(communicating.survfit,col="blue",conf.int=FALSE)
lines(swallowing.survfit,col="red",conf.int=FALSE)
lines(movement.survfit,col="green",conf.int=FALSE)
lines(death.survfit,col=6,conf.int=FALSE)


legend("topright",c("Movement","Breathing","Swallowing","Communicating","Death"),lwd=2,bty="n",col=c("green","black","red","blue",6))

names(movement_survival_sub) <- c("SubjectID","when","is")
names(swallowing_survival_sub) <- c("SubjectID","when","is")
names(communicating_survival_sub) <- c("SubjectID","when","is")
names(breathing_survival_sub) <- c("SubjectID","when","is")

