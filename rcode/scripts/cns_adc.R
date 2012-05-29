.libPaths("packages")
source('classification.r')
load('data_files/cns.RData')
registerDoMC(1)
getDoParWorkers()
snncns <- scale(nncns)
classification(snncns, cns_labels,  300,
'rez/90_nncns_adc_classification.txt',
'rez/90_nncns_adc_robustness.txt',
'rez/90_nncns_adc_decision_value.txt', 
'rez/90_nncns_adc_test_labels.txt', 
get_adc_ranking)
getDoParWorkers()
q('no')
