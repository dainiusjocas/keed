.libPaths("packages")
source('classification.r')
load('data_files/altarAmass.RData')
registerDoMC(1)
getDoParWorkers()
bpdataset <- altarAmass[, c(bp, cc)]
pos <- 1:length(bp)
neg <- (length(bp) + 1):(length(bp)+length(cc))
labels <- c()
labels[pos] <- 1
labels[neg] <- -1
classification(bpdataset, labels,  300,
'rez/90_altarAmass_svm-rfe_classification.txt',
'rez/90_altarAmass_svm-rfe_robustness.txt',
'rez/90_altarAmass_svm-rfe_decision_value.txt', 
'rez/90_altarAmass_svm-rfe_test_labels.txt', 
get_svm_rfe_ranking)
getDoParWorkers()
q('no')
