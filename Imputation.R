library(impute) 
#allpatients <- merge(data.frame("SubjectID"=unique(alsfrsfull$SubjectID)),death_survival["SubjectID"],all=TRUE)
allpatients <- data.frame("SubjectID"=unique(alsfrsfull$SubjectID))
allpatientsfeature <- merge(allpatients,fullfeature,all.x=TRUE)
missing<-vector()
for( i in names(allpatientsfeature)){
  missing[i]=sum(is.na(allpatientsfeature[[i]]))/length(unique(allpatientsfeature$SubjectID))
}
missing=missing[order(missing)]
manyvariables=missing[missing<0.3]

allpatientsfeature_manyvariables <- allpatientsfeature[,names(manyvariables)]
imputedcategorical=allpatientsfeature_manyvariables[,c("Race","onset_site","Gender","if_use_Riluzole","treatment_group","alsmitos","kings")]

numericalfeatures=setdiff(names(manyvariables),c("SubjectID","Race","onset_site","Gender","if_use_Riluzole","treatment_group","alsmitos","kings"))
# Make 'Mode' function
Mode <- function(x){
  x <- x[!is.na(x)]
  uqx <- unique(x)
  uqx[which.max(tabulate(match(x,uqx)))]
}


for(i in c("Race","onset_site","Gender","if_use_Riluzole","treatment_group","alsmitos","kings")){
  imputedcategorical[i][is.na(imputedcategorical[i])]=Mode(imputedcategorical[i])
}


imputednumericalmatrix=impute.knn(as.matrix(allpatientsfeature_manyvariables[numericalfeatures]),k = 10, rowmax = 0.8, colmax = 0.8, maxp = 1500)$data
scalednumericalmatrix=imputednumericalmatrix

rightSkewed = c("GGT", "CK", "RBC", "urine_ph", "AST", "ALT", "Bilirubintotal") 
leftSkewed = c("onset_delta", "q10_min", "q10r_min","total_min") 
for(i in colnames(imputednumericalmatrix)){ 
   	if(i %in% rightSkewed){ 
     		scalednumericalmatrix[,i] = scale(log(imputednumericalmatrix[,i]+1)) 
     	} else if(i %in% leftSkewed){ 
     	  scalednumericalmatrix[,i] = scale(log(-imputednumericalmatrix[,i]+max(imputednumericalmatrix[,i])+1)) 
       	} else{ 
       	  scalednumericalmatrix[,i] = scale(imputednumericalmatrix[,i]) 
         	} 
   } 



allpatientsfeature_imputed=data.frame("SubjectID"=allpatientsfeature$SubjectID,imputedcategorical,scalednumericalmatrix)

