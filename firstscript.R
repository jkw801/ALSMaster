library(dplyr)
library(survival)

alsfrs <-read.csv("alsfrs.csv")

# Delta 누락된 데이터 제거
alsfrs <- droplevels(filter(alsfrs, !is.na(ALSFRS_Delta)))

# 4개의 functional domain 점수 변수 추가
Movement <- (alsfrs$Q8_Walking<=1) | (alsfrs$Q6_Dressing_and_Hygiene<=1)
Swallowing <- (alsfrs$Q3_Swallowing<=1)
Communicating <- (alsfrs$Q1_Speech<=1) & (alsfrs$Q4_Handwriting<=1)
Breathing <- (alsfrs$R_1_Dyspnea<=1) | (alsfrs$R_3_Respiratory_Insufficiency<=2)
for ( i in 1:length(Breathing)) {
  if ( is.na(Breathing[i])){
    Breathing[i]=(alsfrs$Q10_Respiratory[i]<=2)
  }
}
ALSMITOS <- Movement + Swallowing + Communicating + Breathing
alsfrs <- mutate(alsfrs,Movement,Swallowing,Communicating,Breathing,ALSMITOS)
alsfrs$subject_id<-factor(alsfrs$subject_id)가

# 각 item 3점 스케일로 변화. (지워도 될듯)
alsfrs <- mutate(alsfrs, q1r=NA,q2r=NA,q3r=NA,q4r=NA,q6r=NA,q7r=NA,q8r=NA,q9r=NA)
for ( i in 1:length(alsfrs$subject_id)){
  if ( !is.na(alsfrs$Q1_Speech[i])){
    if (alsfrs$Q1_Speech[i]==4){
      alsfrs$q1r[i]=2
    }
    else if (alsfrs$Q1_Speech[i]==2 | alsfrs$Q1_Speech[i]==3){
      alsfrs$q1r[i]=1
    }
    else if (alsfrs$Q1_Speech[i]==1 | alsfrs$Q1_Speech[i]==0){
      alsfrs$q1r[i]=0
    }
  }
}
for ( i in 1:length(alsfrs$subject_id)){
  if ( !is.na(alsfrs$Q2_Salivation[i])){
    if (alsfrs$Q2_Salivation[i]==4){
      alsfrs$q2r[i]=2
    }
    else if (alsfrs$Q2_Salivation[i]==2 | alsfrs$Q2_Salivation[i]==3){
      alsfrs$q2r[i]=1
    }
    else if (alsfrs$Q2_Salivation[i]==1 | alsfrs$Q2_Salivation[i]==0){
      alsfrs$q2r[i]=0
    }
  }
}
for ( i in 1:length(alsfrs$subject_id)){
  if ( !is.na(alsfrs$Q3_Swallowing[i])){
    if (alsfrs$Q3_Swallowing[i]==4){
      alsfrs$q3r[i]=2
    }
    else if (alsfrs$Q3_Swallowing[i]==2 | alsfrs$Q3_Swallowing[i]==3){
      alsfrs$q3r[i]=1
    }
    else if (alsfrs$Q3_Swallowing[i]==1 | alsfrs$Q3_Swallowing[i]==0){
      alsfrs$q3r[i]=0
    }
  }
}
for ( i in 1:length(alsfrs$subject_id)){
  if ( !is.na(alsfrs$Q4_Handwriting[i])){
    if (alsfrs$Q4_Handwriting[i]==4){
      alsfrs$q4r[i]=2
    }
    else if (alsfrs$Q4_Handwriting[i]==2 | alsfrs$Q4_Handwriting[i]==3){
      alsfrs$q4r[i]=1
    }
    else if (alsfrs$Q4_Handwriting[i]==1 | alsfrs$Q4_Handwriting[i]==0){
      alsfrs$q4r[i]=0
    }
  }
}
for ( i in 1:length(alsfrs$subject_id)){
  if ( !is.na(alsfrs$Q6_Dressing_and_Hygiene[i])){
    if (alsfrs$Q6_Dressing_and_Hygiene[i]==4){
      alsfrs$q6r[i]=2
    }
    else if (alsfrs$Q6_Dressing_and_Hygiene[i]==2 | alsfrs$Q6_Dressing_and_Hygiene[i]==3){
      alsfrs$q6r[i]=1
    }
    else if (alsfrs$Q6_Dressing_and_Hygiene[i]==1 | alsfrs$Q6_Dressing_and_Hygiene[i]==0){
      alsfrs$q6r[i]=0
    }
  }
}
for ( i in 1:length(alsfrs$subject_id)){
  if ( !is.na(alsfrs$Q7_Turning_in_Bed[i])){
    if (alsfrs$Q7_Turning_in_Bed[i]==4){
      alsfrs$q7r[i]=2
    }
    else if (alsfrs$Q7_Turning_in_Bed[i]==2 | alsfrs$Q7_Turning_in_Bed[i]==3){
      alsfrs$q7r[i]=1
    }
    else if (alsfrs$Q7_Turning_in_Bed[i]==1 | alsfrs$Q7_Turning_in_Bed[i]==0){
      alsfrs$q7r[i]=0
    }
  }
}
for ( i in 1:length(alsfrs$subject_id)){
  if ( !is.na(alsfrs$Q8_Walking[i])){
    if (alsfrs$Q8_Walking[i]==4){
      alsfrs$q8r[i]=2
    }
    else if (alsfrs$Q8_Walking[i]==2 | alsfrs$Q8_Walking[i]==3){
      alsfrs$q8r[i]=1
    }
    else if (alsfrs$Q8_Walking[i]==1 | alsfrs$Q8_Walking[i]==0){
      alsfrs$q8r[i]=0
    }
  }
}
for ( i in 1:length(alsfrs$subject_id)){
  if ( !is.na(alsfrs$Q9_Climbing_Stairs[i])){
    if (alsfrs$Q9_Climbing_Stairs[i]==4){
      alsfrs$q9r[i]=2
    }
    else if (alsfrs$Q9_Climbing_Stairs[i]==2 | alsfrs$Q9_Climbing_Stairs[i]==3){
      alsfrs$q9r[i]=1
    }
    else if (alsfrs$Q9_Climbing_Stairs[i]==1 | alsfrs$Q9_Climbing_Stairs[i]==0){
      alsfrs$q9r[i]=0
    }
  }
}


# 생존분석을 위한 table 만들기 movement_survival_sub가 최종
group<-group_by(alsfrs,subject_id)
movement_isevent <- summarize(group,any(Movement==1))
movement_whenevent1 <- summarize(group, min(ALSFRS_Delta[which(!is.na(Movement)&Movement==1)]),min(ALSFRS_Delta))
movement_whenevent2 <- summarize(group, max(ALSFRS_Delta[which(!is.na(Movement)&Movement==0)]),min(ALSFRS_Delta))
movement_whenevent <- vector()
movement_deltaleast <- vector()
for (i in 1:length(movement_isevent[[2]])){
  if (is.na(movement_isevent[[2]][i])){
  movement_whenevent[i]=movement_whenevent2[[2]][i]
  movement_deltaleast[i]=movement_whenevent2[[3]][i]
  if (movement_whenevent[i]!=-Inf){
    movement_isevent[[2]][i]=0
  }
  }
  else if (movement_isevent[[2]][i]==0) {
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

# -Inf는 movement가 모든 시점에서 NA인 사람, 그 뒤는 left-censoring
movement_survival_sub <- droplevels(subset(movement_survival,movement_whenevent!=-Inf & !(movement_isevent & movement_whenevent==movement_deltaleast)))
movement_survival_sub <- subset(movement_survival_sub,select=-movement_deltaleast)


# 0시점에서 censoring된 사람 제외.아마도 필요할것....안해도 콕스는 되는데 parametric은 안됨. 그래서 일단 쓴다.
movement_survival_sub <-droplevels(filter(movement_survival_sub,movement_whenevent>0))


# 아래는 그냥 한번  그려본 K-M plot
movement.survfit=survfit(Surv(movement_whenevent,movement_isevent==1)~1,data=movement_survival_sub)
plot(movement.survfit,xlab="Time",ylab="Proportion surviving")


