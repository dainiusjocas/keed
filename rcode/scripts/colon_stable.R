.libPaths("packages")
source('stable_features.r')
source('classification.r')
load('data_files/colon.RData')
registerDoMC(1)
getDoParWorkers()
classification(nncolon, labels,  300,
'rez/90_colon_stable_mcf_classification.txt',
'rez/90_colon_stable_mcf_robustness.txt',
'rez/90_colon_stable_mcf_decision_value.txt', 
'rez/90_colon_stable_mcf_test_labels.txt', 
get_stable_features)
getDoParWorkers()
q('no')
