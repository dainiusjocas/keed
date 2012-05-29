.libPaths("packages")
source('classification.r')
load('data_files/prostate.RData')
registerDoMC(1)
getDoParWorkers()
classification(prostate, prostate_labels,  300, 
'rez/90_prostate_mcf_classification.txt', 
'rez/90_prostate_mcf_robustness.txt',
'rez/90_prostate_mcf_decision_value.txt', 
'rez/90_prostate_mcf_test_labels.txt', 
get_mcf_rfe_ranking)
getDoParWorkers()
q('no')
