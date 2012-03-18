library(foreach)
library(doMC)
library(e1071)
source('fusion.r')

# input: dataset - rows -> features; columns -> tuples
cns_classify <- function(dataset, labels, number_of_folds)
{
  foreach(i = 1:number_of_folds) %dopar%
  {
    do_cns_classification(dataset, labels)
  }
  return(TRUE)
}

do_cns_classification <- function(dataset, labels)
{
  train_size <- round(length(dataset[1, ]) * 0.632)
  train_indexes <- sample(1:length(dataset[1, ]), train_size)
  test_indexes <- setdiff(1:length(dataset[1, ]), train_indexes)
  train_labels <- labels[train_indexes]
  train_data <- dataset[, train_indexes]
  pos <- which(train_labels == 1, arr.ind=T)
  neg <- which(train_labels == -1, arr.ind=T)
  best_features <- mcf_rfe_ranking(train_data, pos, neg)
  write(best_features, file='rez/best_features_colon2.txt', append=T, ncolumns=100)
  write(" ", file='rez/best_features_colon2.txt', append=T)
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
    write(c(i, errors), file='rez/colon2_classification.txt', append=T)
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
