# csv 파일 자체를 R 외부에서조금 수정해서 ALSHistory_r.csv 만듬
alshistory<-read.csv("AlsHistory_r.csv")

# OnsetSite 변수 추가 과정.
for (i in 1:length(alshistory$subject_id))
{
  if (alshistory$Site_of_Onset[i]=="Onset: Bulbar"){
    alshistory$Onsetsite[i]="Bulbar"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Limb"){
    alshistory$Onsetsite[i]="Limb"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Limb and Bulbar"){
    alshistory$Onsetsite[i]="Limb and Bulbar"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Other"){
    alshistory$Onsetsite[i]="Other"
  }
  if (alshistory$Site_of_Onset[i]=="Onset: Spine"){
    alshistory$Onsetsite[i]="Limb"
  }
  if (!is.na(alshistory$Site_of_Onset___Bulbar[i])) {
    if (alshistory$Site_of_Onset___Bulbar[i]==1) {
      alshistory$Onsetsite[i]="Bulbar"
    }
  }
  if (!is.na(alshistory$Site_of_Onset___Limb[i])) {
    if (alshistory$Site_of_Onset___Limb[i]==1) {
      alshistory$Onsetsite[i]="Limb"
    }
  }
  if (!is.na(alshistory$Site_of_Onset___Bulbar[i]) & !is.na(alshistory$Site_of_Onset___Limb[i])){
    if (alshistory$Site_of_Onset___Bulbar[i]==1 & alshistory$Site_of_Onset___Limb[i]==1) {
      alshistory$Onsetsite[i]="Limb and Bulbar"
    }
  }
  
}
alshistory$Onsetsite <- factor(alshistory$Onsetsite)


# DiagnosisDelta >0인 것들은 오류일테니 NA처리
alshistory$Diagnosis_Delta[alshistory$Diagnosis_Delta>0]=NA

# Make 'Mode' function
Mode <- function(x){
  x <- x[!is.na(x)]
  uqx <- unique(x)
  uqx[which.max(tabulate(match(x,uqx)))]
}

alshistory_sub <- summarize(group_by(alshistory,subject_id),Onsetsite=Mode(Onsetsite),Onset_Delta=Mode(Onset_Delta),Diagnosis_Delta=Mode(Diagnosis_Delta))

# 생존분석table에 onsetsite,OnsetDelta,DiagnosisDelta,Diagnostic delay 변수 추가
alshistory_sub$Diagnostic_delay=alshistory_sub$Diagnosis_Delta-alshistory_sub$Onset_Delta
movement_merge1 <- merge(movement_survival_sub,alshistory_sub,by="subject_id",all.x=TRUE)
swallowing_merge1 <- merge(swallowing_survival_sub,alshistory_sub,by="subject_id",all.x=TRUE)
communicating_merge1 <- merge(communicating_survival_sub,alshistory_sub,by="subject_id",all.x=TRUE)
breathing_merge1 <- merge(breathing_survival_sub,alshistory_sub,by="subject_id",all.x=TRUE)
death_merge1 <- merge(death_survival_sub,alshistory_sub,by="subject_id",all.x=TRUE)
