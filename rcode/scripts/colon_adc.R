.libPaths("packages")
source('classification.r')
load('data_files/colon.RData')
registerDoMC(1)
getDoParWorkers()
snncolon <- scale(log2(nncolon))
classification(snncolon, labels,  300,
'rez/90_nncolon_adc_classification.txt',
'rez/90_nncolon_adc_robustness.txt',
'rez/90_nncolon_adc_decision_value.txt', 
'rez/90_nncolon_adc_test_labels.txt', 
get_adc_ranking)
getDoParWorkers()
q('no')
