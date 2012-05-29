.libPaths("packages")
source('classification.r')
load('data_files/colon.RData')
registerDoMC(1)
getDoParWorkers()
classification(nncolon, labels,  300,
'rez/90_nncolon_svm-rfe_classification.txt',
'rez/90_nncolon_svm-rfe_robustness.txt',
'rez/90_nncolon_svm-rfe_decision_value.txt', 
'rez/90_nncolon_svm-rfe_test_labels.txt', 
get_svm_rfe_ranking)
getDoParWorkers()
q('no')
