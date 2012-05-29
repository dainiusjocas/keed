.libPaths("packages")
source('classification.r')
load('data_files/colon.RData')
registerDoMC(1)
getDoParWorkers()
classification(colon, labels,  300,
'rez/90_nncolon_fisher_classification.txt',
'rez/90_nncolon_fisher_robustness.txt',
'rez/90_nncolon_fisher_decision_value.txt', 
'rez/90_nncolon_fisher_test_labels.txt', 
get_fisher_ranking)
getDoParWorkers()
q('no')
