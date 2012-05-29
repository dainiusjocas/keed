.libPaths("packages")
source('classification.r')
load('data_files/prostate.RData')
registerDoMC(1)
getDoParWorkers()
classification(prostate, prostate_labels,  300, 
'rez/90_prostate_relief_classification.txt', 
'rez/90_prostate_relief_robustness.txt',
'rez/90_prostate_relief_decision_value.txt', 
'rez/90_prostate_relief_test_labels.txt', 
get_relief_ranking)
getDoParWorkers()
q('no')
