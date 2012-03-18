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
                         feature_ranking_file='feature_rankings')
{
  foreach(i = 1:number_of_folds) %dopar%
  {
    do_classification(dataset, labels, classification_file, feature_ranking_file)
  }
  return(TRUE)
}

do_classification <- function(dataset, labels, classification_file, feature_ranking_file)
{
  train_size <- round(length(dataset[1, ]) * 0.632)
  train_indexes <- sample(1:length(dataset[1, ]), train_size)
  test_indexes <- setdiff(1:length(dataset[1, ]), train_indexes)
  train_labels <- labels[train_indexes]
  train_data <- dataset[, train_indexes]
  pos <- which(train_labels == 1, arr.ind=T)
  neg <- which(train_labels == -1, arr.ind=T)
  best_features <- sort(get_fisher_scores(train_data, pos, neg), decreasing=T, index.return=T)$ix
  write(best_features, file=feature_ranking_file, append=T, ncolumns=100)
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

###############################################################################
# MCF-RFE implementation library
###############################################################################

# This method gets n best features using fusion feature ranking algorithm
#   recursively 
# input: dataset - rows -> features; columns - tuples
# input: pos - first class, neg - second class
# input: n - how many best features we want to get (default=20)
mcf_rfe_ranking <- function(dataset, pos, neg)
{
  ranking <- c()
  indexes = c(1:length(dataset[ , 1]))
  i <- length(dataset[ , 1])
  while (i > 100)
  {
    ranks <- get_best_features(dataset[indexes, ], pos, neg)
    i <- round(i * 0.5)
    ranking <- c(ranking, indexes[tail(ranks, n=i)])
    indexes <- indexes[-tail(ranks, n=i)]
  } 
  while (i > 2)
  {
    ranks <- get_best_features(dataset[indexes, ], pos, neg)
    ranking <- c(ranking, indexes[tail(ranks, n=1)])
    indexes <- indexes[-tail(ranks, n=1)]
    i <- i - 1
  }
  ranking <- c(ranking, indexes)
  return(rev(ranking))
}
