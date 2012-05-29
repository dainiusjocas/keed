.libPaths("packages")
source('classification.r')
load('data_files/prostate.RData')
registerDoMC(1)
getDoParWorkers()
classification(prostate, prostate_labels,  300, 
'rez/90_prostate_adc_classification.txt', 
'rez/90_prostate_adc_robustness.txt',
'rez/90_prostate_adc_decision_value.txt', 
'rez/90_prostate_adc_test_labels.txt', 
get_adc_ranking)
getDoParWorkers()
q('no')
