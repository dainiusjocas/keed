.libPaths("packages")
source('classification.r')
load('data_files/prostate.RData')
registerDoMC(1)
getDoParWorkers()
classification(prostate, prostate_labels,  300, 
'rez/90_prostate_svm_classification.txt', 
'rez/90_prostate_svm_robustness.txt',
'rez/90_prostate_svm_decision_value.txt', 
'rez/90_prostate_svm_test_labels.txt', 
get_svm_ranking)
getDoParWorkers()
q('no')
