# ALSMaster
I am studying "Predicting survival and loss of physical functions in ALS applying machine learning".


Now, there are 5 scripts.
The order of scrips is "SurvivalTable", "Datacleaning", "Imputation", "CV_cox","CV_coxboost","CV_randomforest", "Clustering".

"SurvivalTable" is for making a table for survival analysis and plotting K-M curves for 5 domains.

"Datacleaning" is for adding features.

"Imputation" is for imputation.

"CV_cox", "CV_coxboost", "CV_randomforest" are for making models and validating the performances by cross-validation. Each one uses cox, coxboost, randomforest, respectively.

"Clustering" is for integrating 5 models' results and clustering patietns based on probability at 12 month in 5 domains. It is ongoing.
