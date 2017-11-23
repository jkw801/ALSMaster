library(tidyr)
library(dplyr)
library(survival)

vital<-read.csv("VitalSigns.csv")
familyhistory <- read.csv("FamilyHistory.csv")

# 데이터 정제. weight은 단위를 kg로 height은 m로 통일
vital$Weight[vital$Weight_Units=="Pounds"] <- vital$Weight[vital$Weight_Units=="Pounds"] * 0.453592
vital$Height[vital$Height_Units=="Inches"] <- vital$Height[vital$Height_Units=="Inches"] * 2.54

# 90일 이하 중 weight의 max min, 뽑기. 데이터 정제 안되어 있어서 여러 컬럼 데이터들 종합.
vital <- filter(vital,is.na(Vital_Signs_Delta) | Vital_Signs_Delta<=90)
weight1 <-summarize(group_by(vital,subject_id),weight_min=min(Weight,na.rm=TRUE),weight_max=max(Weight,na.rm=TRUE))
weight2 <- summarize(group_by(vital,subject_id),weight_min=mean(Baseline_Weight,na.rm=TRUE),weight_max=mean(Baseline_Weight,na.rm=TRUE))
for (i in 1:length(weight1$subject_id)){
  if ( weight1$weight_min[i]==Inf){
    weight1$weight_min[i]=weight2$weight_min[i]
  weight1$weight_max[i]=weight2$weight_min[i]
  }
}

# 마찬가지로 height temperature도.
height <- summarize(group_by(vital,subject_id),height=mean(Height,na.rm=TRUE))
temperature <- summarize(group_by(vital,subject_id),temp_min=min(Temperature,na.rm=TRUE),temp_max=max(Temperature,na.rm=TRUE))

# weight, height, temperature 데이터 합.
vitalmerge <- merge(weight1,height)
vitalmerge <- merge(vitalmerge,temperature)

# familyhistory 정제. 있으면 1. 없으면 0. NA가 너무 너무 너무 많아서 쓰지 못할 자료일 듯.
familyhistory <- select(familyhistory,-c(Family_History_Delta,Other_Specify,Family_Hx_of_ALS_Mutation,Family_Hx_of_ALS_Mutation_Other,Cousin__Paternal_,Cousin__Maternal_,Nephew__Maternal_,Nephew__Paternal_,Niece__Maternal_,Niece__Paternal_,Other,Sibling,Volunteer,Family_Hx_of_Neuro_Disease_Other))
familyhistory <- mutate(familyhistory,"history"=Family_Hx_of_Neuro_Disease=="ALS" | Aunt==1 | Aunt__Maternal_==1 | Aunt__Paternal_==1 | Cousin==1 | Father==1 | Grandfather==1 | Grandfather__Maternal_==1 | Grandfather__Paternal_==1 | Grandmother==1 | Grandmother__Maternal_==1 | Grandmother__Paternal_==1 | Mother==1 | Nephew==1 | Niece==1 | Uncle==1 | Uncle__Maternal_==1 | Uncle__Paternal_==1 | Neurological_Disease=="ALS" | Son==1 | Daughter==1 | Sister==1 | Brother==1)
history <- select(familyhistory,subject_id,history)
history$history[is.na(history$history)]=FALSE
history <- summarize(group_by(history,subject_id),"history"=max(history))
history$history<-as.logical(history$history)



# lab data들 90일 이내로 max min 뽑기.
lab <- read.csv("Labs.csv")
lab <- filter(lab, is.na(Laboratory_Delta) | Laboratory_Delta <=90)
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



# alsfrs 90일 내의 최대 최소 뽑기.
alsfrs90 <- droplevels(filter(alsfrs,ALSFRS_Delta<=90))
q1 <- summarize(group_by(alsfrs90,subject_id),q1_min=min(Q1_Speech,na.rm=TRUE),q1_max=max(Q1_Speech,na.rm=TRUE))
q2 <- summarize(group_by(alsfrs90,subject_id),q2_min=min(Q2_Salivation,na.rm=TRUE),q2_max=max(Q2_Salivation,na.rm=TRUE))
q3 <- summarize(group_by(alsfrs90,subject_id),q3_min=min(Q3_Swallowing,na.rm=TRUE),q3_max=max(Q3_Swallowing,na.rm=TRUE))
q4 <- summarize(group_by(alsfrs90,subject_id),q4_min=min(Q4_Handwriting,na.rm=TRUE),q4_max=max(Q4_Handwriting,na.rm=TRUE))
q6 <- summarize(group_by(alsfrs90,subject_id),q6_min=min(Q6_Dressing_and_Hygiene,na.rm=TRUE),q6_max=max(Q6_Dressing_and_Hygiene,na.rm=TRUE))
q7 <- summarize(group_by(alsfrs90,subject_id),q7_min=min(Q7_Turning_in_Bed,na.rm=TRUE),q7_max=max(Q7_Turning_in_Bed,na.rm=TRUE))
q8 <- summarize(group_by(alsfrs90,subject_id),q8_min=min(Q8_Walking,na.rm=TRUE),q8_max=max(Q8_Walking,na.rm=TRUE))
q9 <- summarize(group_by(alsfrs90,subject_id),q9_min=min(Q9_Climbing_Stairs,na.rm=TRUE),q9_max=max(Q9_Climbing_Stairs,na.rm=TRUE))
qmerge <- q1 %>% full_join(q2) %>% full_join(q3) %>% full_join(q4) %>% full_join(q6) %>% full_join(q7) %>% full_join(q8) %>% full_join(q9)

# 이 아래는 3점 스케일로 한 것. 차차 지울 듯.
q1r <- summarize(group_by(alsfrs90,subject_id),q1r_min=min(q1r,na.rm=TRUE),q1r_max=max(q1r,na.rm=TRUE))
q2r <- summarize(group_by(alsfrs90,subject_id),q2r_min=min(q2r,na.rm=TRUE),q2r_max=max(q2r,na.rm=TRUE))
q3r <- summarize(group_by(alsfrs90,subject_id),q3r_min=min(q3r,na.rm=TRUE),q3r_max=max(q3r,na.rm=TRUE))
q4r <- summarize(group_by(alsfrs90,subject_id),q4r_min=min(q4r,na.rm=TRUE),q4r_max=max(q4r,na.rm=TRUE))
q6r <- summarize(group_by(alsfrs90,subject_id),q6r_min=min(q6r,na.rm=TRUE),q6r_max=max(q6r,na.rm=TRUE))
q7r <- summarize(group_by(alsfrs90,subject_id),q7r_min=min(q7r,na.rm=TRUE),q7r_max=max(q7r,na.rm=TRUE))
q8r <- summarize(group_by(alsfrs90,subject_id),q8r_min=min(q8r,na.rm=TRUE),q8r_max=max(q8r,na.rm=TRUE))
q9r <- summarize(group_by(alsfrs90,subject_id),q9r_min=min(q9r,na.rm=TRUE),q9r_max=max(q9r,na.rm=TRUE))
qrmerge <- q1r %>% full_join(q2r) %>% full_join(q3r) %>% full_join(q4r) %>% full_join(q6r) %>% full_join(q7r) %>% full_join(q8r) %>% full_join(q9r)



# all join. 최종 결과는 merge3.
moremerge <- vitalmerge %>% full_join(history) %>% full_join(labmerge)
moremerge <- merge(moremerge,qmerge,all=TRUE)
moremerge <- merge(moremerge,qrmerge,all=TRUE)
merge3 <- merge(merge2,moremerge,all.x=TRUE)

# merge3 만든 기념으로 콕스 한번 해보자. 차차 지우기.
coxph.all2=coxph(Surv(movement_whenevent,movement_isevent==1)~Onsetsite+Sex+log(-Onset_Delta)+log(-Diagnosis_Delta+10)+weight_min+weight_max,data=merge3)
summary(coxph.all2)
cox.zph(coxph.all2)