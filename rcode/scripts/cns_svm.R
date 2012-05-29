.libPaths("packages")
source('classification.r')
load('data_files/cns.RData')
registerDoMC(1)
getDoParWorkers()
classification(nncns, cns_labels,  300,
'rez/90_nncns_svm_classification.txt',
'rez/90_nncns_svm_robustness.txt',
'rez/90_nncns_svm_decision_value.txt', 
'rez/90_nncns_svm_test_labels.txt', 
get_svm_ranking)
getDoParWorkers()
q('no')
