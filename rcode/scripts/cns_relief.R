.libPaths("packages")
source('classification.r')
load('data_files/cns.RData')
registerDoMC(1)
getDoParWorkers()
classification(nncns, cns_labels,  300,
'rez/90_nncns_relief_classification.txt',
'rez/90_nncns_relief_robustness.txt',
'rez/90_nncns_relief_decision_value.txt', 
'rez/90_nncns_relief_test_labels.txt', 
get_relief_ranking)
getDoParWorkers()
q('no')
