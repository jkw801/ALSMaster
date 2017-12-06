library(dplyr)
# data.allforms 이용
data = droplevels(data.allforms[data.allforms$form_name != "ALSFRS" & data.allforms$form_name != "Adverse Event" & data.allforms$form_name != "Concomitant Medication",] )

features_challenge= c("Lymphocytes","Basophils","Monocytes",
    "Total Cholesterol","Gamma-glutamyltransferase", "CK","height","Red Blood Cells (RBC)",
    "White Blood Cell (WBC)","Urine Ph","Bicarbonate","if_use_Riluzole",
    "respiratory_rate","Calcium","Phosphorus","Platelets","Alkaline Phosphatase","bp_diastolic",
    "bp_systolic","pulse","treatment_group","Hematocrit","Hemoglobin","Chloride","fvc",
    "fvc_normal","fvc_percent","Potassium","Sodium",
    "AST(SGOT)","Blood Urea Nitrogen (BUN)","Creatinine","ALT(SGPT)","Bilirubin (Total)",
    "onset_delta","Age","Gender","onset_site",
    "Race","weight") 

features=levels(data$feature_name)
idnum <- vector()
for (i in features){
  print(i)
  idnum[i]=length(unique(data[data$feature_name==i,]$SubjectID))
}
featnames1 =names(idnum[idnum>3500])
featnames1=featnames1[!(featnames1 %in% c("fvc_percent1","fvc1"))]

databyfeature <- list()
for ( i in featnames1) {
  print(i)
  databyfeature[[i]]=droplevels(data[data$feature_name==i,-c(2,3)])
  databyfeature[[i]]$feature_delta=as.numeric(as.character(databyfeature[[i]]$feature_delta))
}

###############################이 아래부터 돌리자.

feature_92 <- list()
# static feature
#####Riluzole 사용여부 측정 시간있는데 이리해도될지 차차 고민
## categorical 
for (i in c("Race","onset_site","Gender","if_use_Riluzole")){
  feature_92[[i]]=databyfeature[[i]][,-c(3,4)]
  names(feature_92[[i]])=c("SubjectID",i)
}
feature_92[["Gender"]] <-droplevels(subset(feature_92[["Gender"]],Gender!=""))

feature_92[["Race"]]=droplevels(subset(feature_92[["Race"]],Race!="Unknown"))

feature_92[["treatment_group"]]=droplevels(databyfeature[["treatment_group"]][!is.na(databyfeature[["treatment_group"]]$feature_delta)&databyfeature[["treatment_group"]]$feature_delta<92,])
feature_92[["treatment_group"]]=feature_92[["treatment_group"]][,-c(3,4)]
names(feature_92[["treatment_group"]])=c("SubjectID","treatment_group")

## numeric
for (i in c("Age","diag_delta","onset_delta") ){
  feature_92[[i]]=databyfeature[[i]][,-c(3,4)]
  feature_92[[i]]$feature_value=as.numeric(as.character(feature_92[[i]]$feature_value))
  names(feature_92[[i]])=c("SubjectID",i)
}

sfeat=c("Race","onset_site","Gender","if_use_Riluzole","Age","diag_delta","onset_delta","treatment_group")
dfeat=setdiff(featnames1,sfeat)



# dynamic feature
## 우선 ALSFRS form 부터
alsfrs92 <- droplevels(filter(alsfrsfull,feature_delta<92&feature_delta>=0))
feature_92[["Q1_Speech"]] <- summarize(group_by(alsfrs92,SubjectID),q1_min=min(Q1_Speech,na.rm=TRUE))
feature_92[["Q2_Salivation"]] <- summarize(group_by(alsfrs92,SubjectID),q2_min=min(Q2_Salivation,na.rm=TRUE))
feature_92[["Q3_Swallowing"]] <- summarize(group_by(alsfrs92,SubjectID),q3_min=min(Q3_Swallowing,na.rm=TRUE))
feature_92[["Q4_Handwriting"]] <- summarize(group_by(alsfrs92,SubjectID),q4_min=min(Q4_Handwriting,na.rm=TRUE))
feature_92[["Q5_Cutting"]] <- summarize(group_by(alsfrs92,SubjectID),q5_min=min(Q5_Cutting,na.rm=TRUE))
feature_92[["Q6_Dressing_and_Hygiene"]] <- summarize(group_by(alsfrs92,SubjectID),q6_min=min(Q6_Dressing_and_Hygiene,na.rm=TRUE))
feature_92[["Q7_Turning_in_Bed"]] <- summarize(group_by(alsfrs92,SubjectID),q7_min=min(Q7_Turning_in_Bed,na.rm=TRUE))
feature_92[["Q8_Walking"]] <- summarize(group_by(alsfrs92,SubjectID),q8_min=min(Q8_Walking,na.rm=TRUE))
feature_92[["Q9_Climbing_Stairs"]] <- summarize(group_by(alsfrs92,SubjectID),q9_min=min(Q9_Climbing_Stairs,na.rm=TRUE))
feature_92[["Q10"]] <- summarize(group_by(alsfrs92,SubjectID),q10_min=min(respiratory,na.rm=TRUE))
feature_92[["ALSFRS_TotalR"]] <- summarize(group_by(alsfrs92,SubjectID),total_min=min(ALSFRS_TotalR,na.rm=TRUE))
feature_92[["Q10R"]] <- summarize(group_by(alsfrs92,SubjectID),q10r_min=min(Q10R,na.rm=TRUE))
feature_92[["ALSMITOS"]]<- summarize(group_by(alsfrs92,SubjectID),alsmitos=max(ALSMITOS,na.rm=TRUE))
feature_92[["ALSMITOS"]]$alsmitos <- factor(feature_92[["ALSMITOS"]]$alsmitos,order=TRUE)
feature_92[["KINGS"]]<- summarize(group_by(alsfrs92,SubjectID),kings=max(kings,na.rm=TRUE))
feature_92[["KINGS"]]$kings <- factor(feature_92[["KINGS"]]$kings,order=TRUE)
feature_92[["Multibublar"]] <- summarize(group_by(alsfrs92,SubjectID),multibublar_min=min(multibulbar,na.rm=TRUE))
feature_92[["Multimotor"]] <- summarize(group_by(alsfrs92,SubjectID),multimotor_min=min(multimotor,na.rm=TRUE))
feature_92[["Multirespi"]] <- summarize(group_by(alsfrs92,SubjectID),multirespi_min=min(multirespi,na.rm=TRUE))


### total : Q5,Q10 ,      Q10 how?

#### 아래는 preslope 만들기
preslope<-mutate(group_by(alsfrs92,SubjectID),rank=rank(-feature_delta))
preslope <- droplevels(filter(preslope,rank==1))
preslope <- subset(preslope,select=c(SubjectID,feature_delta,ALSFRS_TotalR))
preslope <- as.data.frame(preslope)

temp <- list()
temp[["preslope"]]=merge(feature_92[["onset_delta"]],preslope,all=TRUE)
temp[["preslope"]]$preslope=(temp[["preslope"]]$ALSFRS_TotalR-39)/(temp[["preslope"]]$feature_delta-temp[["preslope"]]$onset_delta)
temp[["preslope"]]$preslope=temp[["preslope"]]$preslope * 30.5
temp[["preslope"]] <- temp[["preslope"]][,c("SubjectID","preslope")]
feature_92[["preslope"]]=temp[["preslope"]]

#### 아래는 onsetage 만들기
temp[["onsetage"]]=merge(feature_92[["Age"]],feature_92[["onset_delta"]],all=TRUE)
medianonset=median(feature_92[["onset_delta"]]$onset_delta)
temp[["onsetage"]]$onsetage=temp[["onsetage"]]$Age +temp[["onsetage"]]$onset_delta/365.25
temp[["onsetage"]][is.na(temp[["onsetage"]]$onsetage),]$onsetage=temp[["onsetage"]][is.na(temp[["onsetage"]]$onsetage),]$Age +medianonset/365.25

temp[["onsetage"]] <- temp[["onsetage"]][,c("SubjectID","onsetage")]
feature_92[["onsetage"]]=temp[["onsetage"]]

## 그 밖에 feature
### 먼저 단위 통일 안된 feature 찾기.
num_unit <- list()
for (i in dfeat) {
  num_unit[[i]]=levels(databyfeature[[i]]$feature_unit)
}

# WBC Protein Platelets Glucose !!?

for (i in dfeat){
  temp[[i]]=droplevels(databyfeature[[i]][!is.na(databyfeature[[i]]$feature_delta)&databyfeature[[i]]$feature_delta<92&databyfeature[[i]]$feature_delta>=0,])
  temp[[i]]$feature_value=as.numeric(as.character(temp[[i]]$feature_value))
  temp[[i]]=subset(temp[[i]],!is.na(feature_value))
} 

temp[["Red Blood Cells (RBC)"]][temp[["Red Blood Cells (RBC)"]]$feature_unit=="x10E12/L",]$feature_value=temp[["Red Blood Cells (RBC)"]][temp[["Red Blood Cells (RBC)"]]$feature_unit=="x10E12/L",]$feature_value*1000

### 0.01?
temp[["Absolute Basophil Count"]][temp[["Absolute Basophil Count"]]$feature_unit=="10E12/L",]$feature_value=temp[["Absolute Basophil Count"]][temp[["Absolute Basophil Count"]]$feature_unit=="10E12/L",]$feature_value*0.01
temp[["Albumin"]]=droplevels(temp[["Albumin"]][temp[["Albumin"]]$feature_unit=="g/L",])


temp[["Platelets"]][temp[["Platelets"]]$feature_value>1000,]$feature_value=temp[["Platelets"]][temp[["Platelets"]]$feature_value>1000,]$feature_value*0.001
temp[["Platelets"]][temp[["Platelets"]]$feature_value<1,]$feature_value=temp[["Platelets"]][temp[["Platelets"]]$feature_value<1,]$feature_value*1000
temp[["Hematocrit"]][temp[["Hematocrit"]]$feature_value<1,]$feature_value=temp[["Hematocrit"]][temp[["Hematocrit"]]$feature_value<1,]$feature_value*100
temp[["Hematocrit"]]=droplevels(temp[["Hematocrit"]][temp[["Hematocrit"]]$feature_value!=0 & temp[["Hematocrit"]]$feature_value<80,])
temp[["temperature"]]=droplevels(temp[["temperature"]][temp[["temperature"]]$feature_value>30 & temp[["temperature"]]$feature_value<50 ,])
temp[["Red Blood Cells (RBC)"]]=droplevels(temp[["Red Blood Cells (RBC)"]][temp[["Red Blood Cells (RBC)"]]$feature_value<10^5 & temp[["Red Blood Cells (RBC)"]]$feature_value>100,])
temp[["Potassium"]]=droplevels(temp[["Potassium"]][temp[["Potassium"]]$feature_value<10,])
temp[["Phophorus"]]=droplevels(temp[["Phosphorus"]][temp[["Phosphorus"]]$feature_value<3,])
temp[["Creatinine"]]=droplevels(temp[["Creatinine"]][temp[["Creatinine"]]$feature_value<300,])
temp[["Calcium"]]=droplevels(temp[["Calcium"]][temp[["Calcium"]]$feature_value<10,])
temp[["Absolute Eosinophil Count"]]=droplevels(temp[["Absolute Eosinophil Count"]][temp[["Absolute Eosinophil Count"]]$feature_value<3,])
temp[["Glucose"]]=droplevels(temp[["Glucose"]][temp[["Glucose"]]$feature_value>1,])
temp[["Absolute Basophil Count"]]=droplevels(temp[["Absolute Basophil Count"]][temp[["Absolute Basophil Count"]]$feature_value<0.3,])
temp[["Hemoglobin"]]=droplevels(temp[["Hemoglobin"]][temp[["Hemoglobin"]]$feature_value>50,])

for (i in dfeat)
{ print (i)
  hist(temp[[i]]$feature_value,xlab=i)
}


for (i in dfeat){
  feature_92[[i]]=summarize(group_by(temp[[i]],SubjectID),mean(feature_value))
  names(feature_92[[i]])=c("SubjectID",i)
  } 

#feature_92[["ALSslope"]]=data.ALSslope

## 63 variables fullmerge!!! -> left join to movement...etc...
names(feature_92[["Red Blood Cells (RBC)"]])=c("SubjectID","RBC")
names(feature_92[["Absolute Basophil Count"]])=c("SubjectID","Abasophil")
names(feature_92[["Absolute Eosinophil Count"]])=c("SubjectID","Aeosinophil")
names(feature_92[["Absolute Lymphocyte Count"]])=c("SubjectID","Alymphocyte")
names(feature_92[["Absolute Monocyte Count"]])=c("SubjectID","Amonocyte")
names(feature_92[["Absolute Neutrophil Count"]])=c("SubjectID","Aneutrophil")
names(feature_92[["Alkaline Phosphatase"]])=c("SubjectID","ALP")
names(feature_92[["ALT(SGPT)"]])=c("SubjectID","ALT")
names(feature_92[["AST(SGOT)"]])=c("SubjectID","AST")
names(feature_92[["Bilirubin (Total)"]])=c("SubjectID","Bilirubintotal")
names(feature_92[["Blood Urea Nitrogen (BUN)"]])=c("SubjectID","BUN")
names(feature_92[["Total Cholesterol"]])=c("SubjectID","Cholesteroltotal")
names(feature_92[["Urine Ph"]])=c("SubjectID","urine_ph")
names(feature_92[["White Blood Cell (WBC)"]])=c("SubjectID","WBC")
names(feature_92[["Gamma-glutamyltransferase"]])=c("SubjectID","GGT")

a=feature_92[[1]]
for (i in 2:length(feature_92)){
  print(i)
  a=merge(a, feature_92[[i]],all=TRUE)
}
fullfeature=a
