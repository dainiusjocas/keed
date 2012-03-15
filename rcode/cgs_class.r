library(foreach)
library(doMC)
library(e1071)
source('fusion.r')
source('consensus.r')
source('fscore.r')

cgs_classification <- function(dataset, labels, train_size, number_of_folds)
{
  features <- seq(10, 50, by=10)
  rez <- foreach(j = features) %dopar%
  {
    classification_errors <- foreach(i = 1:number_of_folds) %dopar%
    {
      do_cgs_classification(dataset, labels, train_size, j)
    }
    write(j, file='rez/classification_cgs.txt', append=T)
    for(k in 1:length(classification_errors))
    {
      write(j, classification_errors[[k]], file='rez/classification_fusion.cgs', append=T)
    }
  }
  return(rez)
}

do_cgs_classification <- function(dataset, labels, train_size, number_of_features)
{
  train_indexes <- sample(1:length(dataset[1, ]), train_size)
  test_indexes <- setdiff(1:length(dataset[1, ]), train_indexes)
  train_data <- dataset[, train_indexes] # buvo transponuota
  train_labels <- labels[train_indexes]
  pos <- which(train_labels == 1, arr.ind=T)
  neg <- which(train_labels == -1, arr.ind=T)
  best_features <- get_consensus_groups(t(train_data), pos, neg, 2, get_fisher_scores)[1:number_of_features]
#     get_mcf_rfe(train_data, pos, neg, number_of_features)
  print(best_features)
  write(best_features, file='rez/best_features_cgs.txt', append=T, ncolumns=100)
  write(" ", file='rez/best_features_cgs.txt', append=T)
  train_data <- train_data[best_features, ]
  model <- best.svm(x=t(train_data), y=as.factor(labels[train_indexes]), kernel='linear', cost=0.01)
  print('classification step')
  #svm(t(train_data), as.factor(labels[train_indexes]), kernel="linear")
  test_data <-  t(dataset[best_features, test_indexes])
  pred <- predict(model, (test_data))
  errors<- 0
  for(j in 1:length(test_indexes))
  {
    if (as.vector(pred)[j] != as.factor(labels[test_indexes])[j]) {
      errors <- errors + 1
    }
  }
  rm(model)
  gc()
  print('-------------------')
  return(errors)
}
