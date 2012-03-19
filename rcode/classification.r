###############################################################################
# Library for classification automation
###############################################################################

library(foreach)
library(doMC)
library(e1071)
source('fusion.r')

# This method does the bootstraping and classification with various subsets of
#   best ranked features.
# input: dataset - rows -> features; columns -> tuples
# input: labels - vector of labels for dataset tuples. normal=1, tumor=-1
# input: number_of_folds - how many times we'll do the classifikation
# input: classification_file: file where the results will be written. 
#   Line format: <number_of_features, errors>
# input: feature_ranking_file - file where the features rankings will be placed.
#   Data from this file will be user to measure robustness of feature ranking.
# output: TRUE just to return something.
classification <- function(dataset, labels, number_of_folds,
                           classification_file='classification_errors.txt',
                           feature_ranking_file='feature_rankings',
                           feature_ranking_method)
{
  foreach(i = 1:number_of_folds) %dopar%
  {
    do_classification(dataset, labels, classification_file,
                      feature_ranking_file, feature_ranking_method)
  }
  return(TRUE)
}

do_classification <- function(dataset, labels, classification_file,
                              feature_ranking_file,
                              feature_ranking_method)
{
  train_size <- round(length(dataset[1, ]) * 0.632)
  train_indexes <- sample(1:length(dataset[1, ]), train_size)
  test_indexes <- setdiff(1:length(dataset[1, ]), train_indexes)
  train_labels <- labels[train_indexes]
  train_data <- dataset[, train_indexes]
  pos <- which(train_labels == 1, arr.ind=T)
  neg <- which(train_labels == -1, arr.ind=T)
  best_features <- feature_ranking_method(train_data, pos, neg)
  write(best_features, file=feature_ranking_file, append=T, ncolumns=length(dataset[, 1]))
  write(" ", file=feature_ranking_file, append=T)
  number_of_features <- seq(from=10, to=300, by=10)
  for (i in number_of_features)
  {
    train_data <- dataset[best_features[1:i], train_indexes]
    test_data <-  t(dataset[best_features[1:i], test_indexes])
    model <- best.svm(x=t(train_data), y=as.factor(labels[train_indexes]), kernel='linear', cost=0.01)
    pred <- predict(model, (test_data))
    errors <- 0
    for(j in 1:length(test_indexes))
    {
      if (as.vector(pred)[j] != as.factor(labels[test_indexes])[j]) {
        errors <- errors + 1
      }
    }
    write(c(i, errors), file=classification_file, append=T)
    rm(model)
    gc()
  }
  return(number_of_features)
}

