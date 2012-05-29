.libPaths("packages")
source('classification.r')
load('data_files/cns.RData')
registerDoMC(1)
getDoParWorkers()
classification(nncns, cns_labels,  300,
'rez/90_nncns_fisher_classification.txt',
'rez/90_nncns_fisher_robustness.txt',
'rez/90_nncns_fisher_decision_value.txt', 
'rez/90_nncns_fisher_test_labels.txt', 
get_fisher_ranking)
getDoParWorkers()
q('no')
