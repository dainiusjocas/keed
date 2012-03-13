.libPaths("../")
source('../classiffy.r')
registerDoMC(8)
getDoParWorkers()
classify_with_folds(ndata, labels, 150, 39, 300)
q('yes')

