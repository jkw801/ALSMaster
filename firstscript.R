library(dplyr)
library(survival)

alsfrs <-read.csv("alsfrs.csv")

# Delta 누락된 데이터 제거. alsfrs item 하나라도 없는 사람도 제거.
alsfrs <- droplevels(filter(alsfrs, !is.na(ALSFRS_Delta)))
alsfrs <- droplevels(filter(alsfrs,!is.na(alsfrs$Q1_Speech)&!is.na(alsfrs$Q2_Salivation)&!is.na(alsfrs$Q3_Swallowing)&!is.na(alsfrs$Q4_Handwriting)&(!(is.na(alsfrs$Q5a_Cutting_without_Gastrostomy)&is.na(alsfrs$Q5b_Cutting_with_Gastrostomy)))&!is.na(alsfrs$Q6_Dressing_and_Hygiene)&!is.na(alsfrs$Q7_Turning_in_Bed)&!is.na(alsfrs$Q8_Walking)&!is.na(alsfrs$Q9_Climbing_Stairs)&!(is.na(alsfrs$Q10_Respiratory)&(is.na(alsfrs$R_1_Dyspnea)|is.na(alsfrs$R_2_Orthopnea)|is.na(alsfrs$R_3_Respiratory_Insufficiency)))))

# totalsub : total excluding breathing
alsfrs$totalsub=NA
alsfrs$rtotalsub=NA
alsfrs$totalsub[!is.na(alsfrs$Q10_Respiratory)]=alsfrs$ALSFRS_Total[!is.na(alsfrs$Q10_Respiratory)]-alsfrs$Q10_Respiratory[!is.na(alsfrs$Q10_Respiratory)]
alsfrs$rtotalsub[!is.na(alsfrs$R_1_Dyspnea)&!is.na(alsfrs$R_2_Orthopnea)&!is.na(alsfrs$R_3_Respiratory_Insufficiency)]=alsfrs$ALSFRS_R_Total[!is.na(alsfrs$R_1_Dyspnea)&!is.na(alsfrs$R_2_Orthopnea)&!is.na(alsfrs$R_3_Respiratory_Insufficiency)]-alsfrs$R_1_Dyspnea[!is.na(alsfrs$R_1_Dyspnea)&!is.na(alsfrs$R_2_Orthopnea)&!is.na(alsfrs$R_3_Respiratory_Insufficiency)]-alsfrs$R_2_Orthopnea[!is.na(alsfrs$R_1_Dyspnea)&!is.na(alsfrs$R_2_Orthopnea)&!is.na(alsfrs$R_3_Respiratory_Insufficiency)]-alsfrs$R_3_Respiratory_Insufficiency[!is.na(alsfrs$R_1_Dyspnea)&!is.na(alsfrs$R_2_Orthopnea)&!is.na(alsfrs$R_3_Respiratory_Insufficiency)]
alsfrs$totalsub[is.na(alsfrs$totalsub)]=alsfrs$rtotalsub[is.na(alsfrs$totalsub)]
alsfrs$rtotalsub=NA
#alsfrs$Q1_Speech <- factor(alsfrs$Q1_Speech,order=TRUE)
#alsfrs$Q2_Salivation<- factor(alsfrs$Q2_Salivation,order=TRUE)
#alsfrs$Q3_Swallowing<- factor(alsfrs$Q3_Swallowing,order=TRUE)
#alsfrs$Q4_Handwriting<- factor(alsfrs$Q4_Handwriting,order=TRUE)
#alsfrs$Q5a_Cutting_without_Gastrostomy<- factor(alsfrs$Q5a_Cutting_without_Gastrostomy,order=TRUE)
#alsfrs$Q5b_Cutting_with_Gastrostomy<- factor(alsfrs$Q5b_Cutting_with_Gastrostomy,order=TRUE)
#alsfrs$Q6_Dressing_and_Hygiene<- factor(alsfrs$Q6_Dressing_and_Hygiene,order=TRUE)
#alsfrs$Q7_Turning_in_Bed<- factor(alsfrs$Q7_Turning_in_Bed,order=TRUE)
#alsfrs$Q8_Walking<- factor(alsfrs$Q8_Walking,order=TRUE)
#alsfrs$Q9_Climbing_Stairs<- factor(alsfrs$Q9_Climbing_Stairs,order=TRUE)
#alsfrs$Q10_Respiratory<- factor(alsfrs$Q10_Respiratory,order=TRUE)
#alsfrs$R_1_Dyspnea<- factor(alsfrs$R_1_Dyspnea,order=TRUE)
#alsfrs$R_2_Orthopnea<- factor(alsfrs$R_2_Orthopnea,order=TRUE)
#alsfrs$R_3_Respiratory_Insufficiency<- factor(alsfrs$R_3_Respiratory_Insufficiency,order=TRUE)

# 4개의 functional domain 점수 변수 추가
Movement <- (alsfrs$Q8_Walking<=1) | (alsfrs$Q6_Dressing_and_Hygiene<=1)
Swallowing <- (alsfrs$Q3_Swallowing<=1)
Communicating <- (alsfrs$Q1_Speech<=1) & (alsfrs$Q4_Handwriting<=1)
Breathing <- (alsfrs$R_1_Dyspnea<=1) | (alsfrs$R_3_Respiratory_Insufficiency<=2)
Breathing[is.na(Breathing)]=(alsfrs$Q10_Respiratory[is.na(Breathing)]<=2)

ALSMITOS <- Movement + Swallowing + Communicating + Breathing
alsfrs <- mutate(alsfrs,Movement,Swallowing,Communicating,Breathing,ALSMITOS)
alsfrs$subject_id<-factor(alsfrs$subject_id)


# Movement 생존분석을 위한 table 만들기 : movement_survival_sub가 최종
group<-group_by(alsfrs,subject_id)
movement_isevent <- summarize(group,any(Movement==1))
movement_whenevent1 <- summarize(group, min(ALSFRS_Delta[which(Movement==1)]),min(ALSFRS_Delta))
movement_whenevent2 <- summarize(group, max(ALSFRS_Delta[which(Movement==0)]),min(ALSFRS_Delta))
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
movement_survival <- data.frame("subject_id"=movement_isevent[[1]],movement_whenevent,movement_deltaleast,"movement_isevent"=movement_isevent[[2]])
movement_survival=movement_survival[order(movement_survival$movement_whenevent),]

# left-censoring 제외
movement_survival <- droplevels(subset(movement_survival,!(movement_isevent & movement_whenevent==movement_deltaleast)))
movement_survival <- subset(movement_survival,select=-movement_deltaleast)


# 0시점에서 censoring된 사람 제외.아마도 필요할것....안해도 콕스는 되는데 parametric은 안됨. 그래서 일단 쓴다.
movement_survival_sub <-droplevels(filter(movement_survival,movement_whenevent>0))


# 아래는 그냥 한번  그려본 K-M plot
movement.survfit=survfit(Surv(movement_whenevent,movement_isevent==1)~1,data=movement_survival_sub)
plot(movement.survfit,xlab="Time",ylab="Proportion surviving")

# Swallowing 생존분석을 위한 table 만들기 : swallowing_survival_sub가 최종
group<-group_by(alsfrs,subject_id)
swallowing_isevent <- summarize(group,any(Swallowing==1))
swallowing_whenevent1 <- summarize(group, min(ALSFRS_Delta[which(Swallowing==1)]),min(ALSFRS_Delta))
swallowing_whenevent2 <- summarize(group, max(ALSFRS_Delta[which(Swallowing==0)]),min(ALSFRS_Delta))
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
swallowing_survival <- data.frame("subject_id"=swallowing_isevent[[1]],swallowing_whenevent,swallowing_deltaleast,"swallowing_isevent"=swallowing_isevent[[2]])
swallowing_survival=swallowing_survival[order(swallowing_survival$swallowing_whenevent),]

# left-censoring 제외
swallowing_survival <- droplevels(subset(swallowing_survival,!(swallowing_isevent & swallowing_whenevent==swallowing_deltaleast)))
swallowing_survival <- subset(swallowing_survival,select=-swallowing_deltaleast)


# 0시점에서 censoring된 사람 제외.아마도 필요할것....안해도 콕스는 되는데 parametric은 안됨. 그래서 일단 쓴다.
swallowing_survival_sub <-droplevels(filter(swallowing_survival,swallowing_whenevent>0))


# 아래는 그냥 한번  그려본 K-M plot
swallowing.survfit=survfit(Surv(swallowing_whenevent,swallowing_isevent==1)~1,data=swallowing_survival_sub)
plot(swallowing.survfit,xlab="Time",ylab="Proportion surviving")

# Communicating 생존분석을 위한 table 만들기 : communicating_survival_sub가 최종
group<-group_by(alsfrs,subject_id)
communicating_isevent <- summarize(group,any(Communicating==1))
communicating_whenevent1 <- summarize(group, min(ALSFRS_Delta[which(Communicating==1)]),min(ALSFRS_Delta))
communicating_whenevent2 <- summarize(group, max(ALSFRS_Delta[which(Communicating==0)]),min(ALSFRS_Delta))
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
communicating_survival <- data.frame("subject_id"=communicating_isevent[[1]],communicating_whenevent,communicating_deltaleast,"communicating_isevent"=communicating_isevent[[2]])
communicating_survival=communicating_survival[order(communicating_survival$communicating_whenevent),]

# left-censoring 제외
communicating_survival <- droplevels(subset(communicating_survival,!(communicating_isevent & communicating_whenevent==communicating_deltaleast)))
communicating_survival <- subset(communicating_survival,select=-communicating_deltaleast)


# 0시점에서 censoring된 사람 제외.아마도 필요할것....안해도 콕스는 되는데 parametric은 안됨. 그래서 일단 쓴다.
communicating_survival_sub <-droplevels(filter(communicating_survival,communicating_whenevent>0))


# 아래는 그냥 한번  그려본 K-M plot
communicating.survfit=survfit(Surv(communicating_whenevent,communicating_isevent==1)~1,data=communicating_survival_sub)
plot(communicating.survfit,xlab="Time",ylab="Proportion surviving")

# breathing 생존분석을 위한 table 만들기 : breathing_survival_sub가 최종
group<-group_by(alsfrs,subject_id)
breathing_isevent <- summarize(group,any(Breathing==1))
breathing_whenevent1 <- summarize(group, min(ALSFRS_Delta[which(Breathing==1)]),min(ALSFRS_Delta))
breathing_whenevent2 <- summarize(group, max(ALSFRS_Delta[which(Breathing==0)]),min(ALSFRS_Delta))
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
breathing_survival <- data.frame("subject_id"=breathing_isevent[[1]],breathing_whenevent,breathing_deltaleast,"breathing_isevent"=breathing_isevent[[2]])
breathing_survival=breathing_survival[order(breathing_survival$breathing_whenevent),]

# left-censoring 제외
breathing_survival <- droplevels(subset(breathing_survival,!(breathing_isevent & breathing_whenevent==breathing_deltaleast)))
breathing_survival <- subset(breathing_survival,select=-breathing_deltaleast)


# 0시점에서 censoring된 사람 제외.아마도 필요할것....안해도 콕스는 되는데 parametric은 안됨. 그래서 일단 쓴다.
breathing_survival_sub <-droplevels(filter(breathing_survival,breathing_whenevent>0))


# 아래는 그냥 한번  그려본 K-M plot
breathing.survfit=survfit(Surv(breathing_whenevent,breathing_isevent==1)~1,data=breathing_survival_sub)
plot(breathing.survfit,xlab="Time",ylab="Proportion surviving")

# all plot
plot(breathing.survfit,xlab="Time",ylab="Proportion surviving",conf.int=FALSE)
lines(communicating.survfit,col="blue",conf.int=FALSE)
lines(swallowing.survfit,col="red",conf.int=FALSE)
lines(movement.survfit,col="green",conf.int=FALSE)
legend("topright",c("Movement","Breathing","Swallowing","Communicating"),lwd=2,bty="n",col=c("green","black","red","blue"))

names(movement_survival_sub) <- c("subject_id","when","is")
names(swallowing_survival_sub) <- c("subject_id","when","is")
names(communicating_survival_sub) <- c("subject_id","when","is")
names(breathing_survival_sub) <- c("subject_id","when","is")