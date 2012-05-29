.libPaths("packages")
source('classification.r')
load('data_files/colon.RData')
registerDoMC(1)
getDoParWorkers()
classification(nncolon, labels,  300,
'rez/90_nncolon_relief_classification.txt',
'rez/90_nncolon_relief_robustness.txt',
'rez/90_nncolon_relief_decision_value.txt', 
'rez/90_nncolon_relief_test_labels.txt', 
get_relief_ranking)
getDoParWorkers()
q('no')
