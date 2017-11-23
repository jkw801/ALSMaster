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

# 생존분석table에 onsetsite,OnsetDelta,DiagnosisDelta 변수 추가
alshistory_sub <- subset(alshistory, subset=!is.na(Onsetsite),select=c(subject_id, Onsetsite,Onset_Delta,Diagnosis_Delta))
merge1 <- merge(movement_survival_sub,alshistory_sub,by="subject_id",all.x=TRUE)
merge1 <- merge1[order(merge1$movement_whenevent),]
