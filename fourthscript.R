library(tidyr)
library(dplyr)
library(survival)

vital<-read.csv("VitalSigns.csv")
familyhistory <- read.csv("FamilyHistory.csv")

# 데이터 정제. weight은 단위를 kg로 height은 m로 통일
vital$Weight[vital$Weight_Units=="Pounds"] <- vital$Weight[vital$Weight_Units=="Pounds"] * 0.453592
vital$Height[vital$Height_Units=="Inches"] <- vital$Height[vital$Height_Units=="Inches"] * 2.54

# height, 30일 이하 중 weight과 temperature의 max min, 뽑기. 데이터 정제 안되어 있어서 여러 컬럼 데이터들 종합.
height <- summarize(group_by(vital,subject_id),height=mean(Height,na.rm=TRUE))
vital <- droplevels(filter(vital, !is.na(Vital_Signs_Delta)))
vital <- droplevels(filter(vital,Vital_Signs_Delta<=91 & Vital_Signs_Delta>=0))
weight1 <-summarize(group_by(vital,subject_id),weight_min=min(Weight,na.rm=TRUE),weight_max=max(Weight,na.rm=TRUE))
weight2 <- summarize(group_by(vital,subject_id),weight_min=mean(Baseline_Weight,na.rm=TRUE),weight_max=mean(Baseline_Weight,na.rm=TRUE))
for (i in 1:length(weight1$subject_id)){
  if ( weight1$weight_min[i]==Inf){
    weight1$weight_min[i]=weight2$weight_min[i]
  weight1$weight_max[i]=weight2$weight_min[i]
  }
}
temperature <- summarize(group_by(vital,subject_id),temp_min=min(Temperature,na.rm=TRUE),temp_max=max(Temperature,na.rm=TRUE))

# weight, height, temperature 데이터 합.
vitalmerge <- merge(weight1,height,all.x=TRUE)
vitalmerge <- merge(vitalmerge,temperature,all.x=TRUE)

# familyhistory 정제. 있으면 1. 없으면 0. NA가 너무 너무 너무 많아서 쓰지 못할 자료일 듯.
familyhistory <- select(familyhistory,-c(Family_History_Delta,Other_Specify,Family_Hx_of_ALS_Mutation,Family_Hx_of_ALS_Mutation_Other,Cousin__Paternal_,Cousin__Maternal_,Nephew__Maternal_,Nephew__Paternal_,Niece__Maternal_,Niece__Paternal_,Other,Sibling,Volunteer,Family_Hx_of_Neuro_Disease_Other))
familyhistory <- mutate(familyhistory,"history"=(Family_Hx_of_Neuro_Disease=="ALS" | Aunt==1 | Aunt__Maternal_==1 | Aunt__Paternal_==1 | Cousin==1 | Father==1 | Grandfather==1 | Grandfather__Maternal_==1 | Grandfather__Paternal_==1 | Grandmother==1 | Grandmother__Maternal_==1 | Grandmother__Paternal_==1 | Mother==1 | Nephew==1 | Niece==1 | Uncle==1 | Uncle__Maternal_==1 | Uncle__Paternal_==1 | Neurological_Disease=="ALS" | Son==1 | Daughter==1 | Sister==1 | Brother==1))
history <- select(familyhistory,subject_id,history)
history$history[is.na(history$history)]=FALSE
history <- summarize(group_by(history,subject_id),"history"=max(history))

# lab data들 30일 이내로 max min 뽑기.
lab <- read.csv("Labs.csv")
lab <- filter(lab, !is.na(Laboratory_Delta))
lab <- filter(lab, Laboratory_Delta>=0 & Laboratory_Delta <=91)
filter_hb <- filter(lab,Test_Name=="Hemoglobin")
filter_hb$Test_Result <- as.numeric(as.character(filter_hb$Test_Result))
lab_hb <- summarize(group_by(filter_hb,subject_id), hb_min=min(Test_Result,na.rm=TRUE),hb_max=max(Test_Result,na.rm=TRUE))
filter_ua <- filter(lab, Test_Name=="Uric Acid")
filter_ua$Test_Result <- as.numeric(as.character(filter_ua$Test_Result))
lab_ua <- summarize(group_by(filter_ua,subject_id), ua_min=min(Test_Result,na.rm=TRUE),ua_max=max(Test_Result,na.rm=TRUE))
filter_na <- filter(lab, Test_Name=="Sodium")
filter_na$Test_Result <- as.numeric(as.character(filter_na$Test_Result))
lab_na <- summarize(group_by(filter_na,subject_id), na_min=min(Test_Result,na.rm=TRUE),na_max=max(Test_Result,na.rm=TRUE))
filter_glu <- filter(lab, Test_Name=="Glucose")
filter_glu$Test_Result <- as.numeric(as.character(filter_glu$Test_Result))
lab_glu <- summarize(group_by(filter_glu,subject_id), glu_min=min(Test_Result,na.rm=TRUE),glu_max=max(Test_Result,na.rm=TRUE))
filter_cl <- filter(lab, Test_Name=="Chloride")
filter_cl$Test_Result <- as.numeric(as.character(filter_cl$Test_Result))
lab_cl <- summarize(group_by(filter_cl,subject_id), cl_min=min(Test_Result,na.rm=TRUE),cl_max=max(Test_Result,na.rm=TRUE))
filter_bicap <- filter(lab, Test_Name=="Bicarbonate")
filter_bicap$Test_Result <- as.numeric(as.character(filter_bicap$Test_Result))
lab_bicap <- summarize(group_by(filter_bicap,subject_id), bicap_min=min(Test_Result,na.rm=TRUE),bicap_max=max(Test_Result,na.rm=TRUE))
filter_pt <- filter(lab, Test_Name=="Platelets")
filter_pt$Test_Result <- as.numeric(as.character(filter_pt$Test_Result))
lab_pt <- summarize(group_by(filter_pt,subject_id), pt_min=min(Test_Result,na.rm=TRUE),pt_max=max(Test_Result,na.rm=TRUE))
filter_ht <- filter(lab, Test_Name=="Hematocrit")
filter_ht$Test_Result <- as.numeric(as.character(filter_ht$Test_Result))
lab_ht <- summarize(group_by(filter_ht,subject_id), ht_min=min(Test_Result,na.rm=TRUE),ht_max=max(Test_Result,na.rm=TRUE))
filter_k <- filter(lab, Test_Name=="Potassium")
filter_k$Test_Result <- as.numeric(as.character(filter_k$Test_Result))
lab_k <- summarize(group_by(filter_k,subject_id), k_min=min(Test_Result,na.rm=TRUE),k_max=max(Test_Result,na.rm=TRUE))
filter_CK <- filter(lab, Test_Name=="CK")
filter_CK$Test_Result <- as.numeric(as.character(filter_CK$Test_Result))
lab_CK <- summarize(group_by(filter_CK,subject_id), CK_min=min(Test_Result,na.rm=TRUE),CK_max=max(Test_Result,na.rm=TRUE))
filter_ca <- filter(lab, Test_Name=="Calcium")
filter_ca$Test_Result <- as.numeric(as.character(filter_ca$Test_Result))
lab_ca <- summarize(group_by(filter_ca,subject_id), ca_min=min(Test_Result,na.rm=TRUE),ca_max=max(Test_Result,na.rm=TRUE))
filter_cr <- filter(lab, Test_Name=="Creatinine")
filter_cr$Test_Result <- as.numeric(as.character(filter_cr$Test_Result))
lab_cr <- summarize(group_by(filter_cr,subject_id), cr_min=min(Test_Result,na.rm=TRUE),cr_max=max(Test_Result,na.rm=TRUE))
filter_rbc <- filter(lab, Test_Name=="Red Blood Cells (RBC)")
filter_rbc$Test_Result <- as.numeric(as.character(filter_rbc$Test_Result))
lab_rbc <- summarize(group_by(filter_rbc,subject_id), rbc_min=min(Test_Result,na.rm=TRUE),rbc_max=max(Test_Result,na.rm=TRUE))
filter_wbc <- filter(lab, Test_Name=="White Blood Cell (WBC)")
filter_wbc$Test_Result <- as.numeric(as.character(filter_wbc$Test_Result))
lab_wbc <- summarize(group_by(filter_wbc,subject_id), wbc_min=min(Test_Result,na.rm=TRUE),wbc_max=max(Test_Result,na.rm=TRUE))

labmerge <- lab_bicap %>% full_join(lab_ca) %>% full_join(lab_CK) %>% full_join(lab_cl) %>% full_join(lab_cr) %>% full_join(lab_glu) %>% full_join(lab_hb) %>% full_join(lab_ht) %>% full_join(lab_k) %>% full_join(lab_na) %>% full_join(lab_pt) %>% full_join(lab_rbc) %>% full_join(lab_ua) %>%
  full_join(lab_wbc)

# 아래 처럼 짜면 위의 긴 코드를 한줄로 끝낼 수 있으나 계산량 폭발한다. 그래서 보류.
# lab <- summarize(group_by(lab,subject_id,Test_Name,Test_Unit,Laboratory_Delta),mean(Test_Result))
# lab_wide <- spread(lab,Test_Name,Test_Result)
# 그냥 mean하면 안될듯.  "-" 적혀있는 것도 있고.



# alsfrs30일 내의 최대 최소 뽑기.
alsfrs90 <- droplevels(filter(alsfrs,ALSFRS_Delta<=91&ALSFRS_Delta>=0))
totalsub <- summarize(group_by(alsfrs90,subject_id),totalsub_min=min(totalsub,na.rm=TRUE),totalsub_max=max(totalsub,na.rm=TRUE))
q1 <- summarize(group_by(alsfrs90,subject_id),q1_min=min(Q1_Speech,na.rm=TRUE),q1_max=max(Q1_Speech,na.rm=TRUE))
q2 <- summarize(group_by(alsfrs90,subject_id),q2_min=min(Q2_Salivation,na.rm=TRUE),q2_max=max(Q2_Salivation,na.rm=TRUE))
q3 <- summarize(group_by(alsfrs90,subject_id),q3_min=min(Q3_Swallowing,na.rm=TRUE),q3_max=max(Q3_Swallowing,na.rm=TRUE))
q4 <- summarize(group_by(alsfrs90,subject_id),q4_min=min(Q4_Handwriting,na.rm=TRUE),q4_max=max(Q4_Handwriting,na.rm=TRUE))
q6 <- summarize(group_by(alsfrs90,subject_id),q6_min=min(Q6_Dressing_and_Hygiene,na.rm=TRUE),q6_max=max(Q6_Dressing_and_Hygiene,na.rm=TRUE))
q7 <- summarize(group_by(alsfrs90,subject_id),q7_min=min(Q7_Turning_in_Bed,na.rm=TRUE),q7_max=max(Q7_Turning_in_Bed,na.rm=TRUE))
q8 <- summarize(group_by(alsfrs90,subject_id),q8_min=min(Q8_Walking,na.rm=TRUE),q8_max=max(Q8_Walking,na.rm=TRUE))
q9 <- summarize(group_by(alsfrs90,subject_id),q9_min=min(Q9_Climbing_Stairs,na.rm=TRUE),q9_max=max(Q9_Climbing_Stairs,na.rm=TRUE))
qmerge <- totalsub %>% full_join(q1) %>% full_join(q2) %>% full_join(q3) %>% full_join(q4) %>% full_join(q6) %>% full_join(q7) %>% full_join(q8) %>% full_join(q9)


# FVC data
fvc<-read.csv("Fvc.csv")
fvc <- filter(fvc, !is.na(Forced_Vital_Capacity_Delta))
for(i in 1:length(fvc$subject_id)){
  if (!is.na(fvc$Subject_Liters_Trial_1[i])&!is.na(fvc$Subject_Normal[i])){
    fvc$pct_of_Normal_Trial_1[i]=fvc$Subject_Liters_Trial_1/fvc$Subject_Normal*100
  }
  if (!is.na(fvc$Subject_Liters_Trial_2[i])&!is.na(fvc$Subject_Normal[i])){
    fvc$pct_of_Normal_Trial_2[i]=fvc$Subject_Liters_Trial_2/fvc$Subject_Normal*100
  }
  if (!is.na(fvc$Subject_Liters_Trial_3[i])&!is.na(fvc$Subject_Normal[i])){
    fvc$pct_of_Normal_Trial_3[i]=fvc$Subject_Liters_Trial_3/fvc$Subject_Normal*100
  }
}
fvc1 <- gather(fvc, trial,Liter,c(Subject_Liters_Trial_1,Subject_Liters_Trial_2,Subject_Liters_Trial_3))
fvc1<- fvc1[order(fvc1$subject_id),]
fvc1gp <- summarize(group_by(fvc1,subject_id,Forced_Vital_Capacity_Delta),fvc=mean(Liter,na.rm=TRUE))
fvc2 <- gather(fvc, trial,pct,c(pct_of_Normal_Trial_1,pct_of_Normal_Trial_2,pct_of_Normal_Trial_3))
fvc2<- fvc2[order(fvc2$subject_id),]
fvc2gp <- summarize(group_by(fvc2,subject_id,Forced_Vital_Capacity_Delta),fvcpct=mean(pct,na.rm=TRUE))
fvcmerge <- merge(fvc1gp, fvc2gp, all=TRUE)
fvcmerge <- droplevels(filter(fvcmerge,Forced_Vital_Capacity_Delta<=91 & Forced_Vital_Capacity_Delta>=0))
fvcmerge <- summarize(group_by(fvcmerge,subject_id), fvc=mean(fvc,na.rm=TRUE),fvcpct=mean(fvcpct,na.rm=TRUE))

# preslope : onset 당시~ 91일까지 alsfrs slope
preslope <- droplevels(filter(alsfrs,ALSFRS_Delta<=91&ALSFRS_Delta>=0))
preslope1<-mutate(group_by(preslope,subject_id),rank=rank(-ALSFRS_Delta))
preslope1 <- droplevels(filter(preslope1,rank==1))
preslope1 <- subset(preslope1,select=c(subject_id,ALSFRS_Delta,totalsub))
# all join. 최종 결과는 merge3.
moremerge <- vitalmerge %>% full_join(history) %>% full_join(labmerge) %>% full_join(fvcmerge)
moremerge <- merge(moremerge,qmerge,all=TRUE)
moremerge <- merge(moremerge, preslope1, all=TRUE)

movement_merge3 <- merge(movement_merge2,moremerge,all.x=TRUE)
movement_merge3$preslope=(35-movement_merge3$totalsub)/(movement_merge3$ALSFRS_Delta-movement_merge3$Onset_Delta)
movement_merge3 <- subset(movement_merge3,select=-c(totalsub,ALSFRS_Delta))
swallowing_merge3 <- merge(swallowing_merge2,moremerge,all.x=TRUE)
swallowing_merge3$preslope=(35-swallowing_merge3$totalsub)/(swallowing_merge3$ALSFRS_Delta-swallowing_merge3$Onset_Delta)
swallowing_merge3 <- subset(swallowing_merge3,select=-c(totalsub,ALSFRS_Delta))
communicating_merge3 <- merge(communicating_merge2,moremerge,all.x=TRUE)
communicating_merge3$preslope=(35-communicating_merge3$totalsub)/(communicating_merge3$ALSFRS_Delta-communicating_merge3$Onset_Delta)
communicating_merge3 <- subset(communicating_merge3,select=-c(totalsub,ALSFRS_Delta))
breathing_merge3 <- merge(breathing_merge2,moremerge,all.x=TRUE)
breathing_merge3$preslope=(35-breathing_merge3$totalsub)/(breathing_merge3$ALSFRS_Delta-breathing_merge3$Onset_Delta)
breathing_merge3 <- subset(breathing_merge3,select=-c(totalsub,ALSFRS_Delta))
death_merge3 <- merge(death_merge2,moremerge,all.x=TRUE)
death_merge3$preslope=(35-death_merge3$totalsub)/(death_merge3$ALSFRS_Delta-death_merge3$Onset_Delta)
death_merge3 <- subset(death_merge3,select=-c(totalsub,ALSFRS_Delta))

