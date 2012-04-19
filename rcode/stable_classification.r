###############################################################################
# Library for classification automation
###############################################################################

# library(foreach)
# library(doMC)
library(e1071)
# source('fusion.r')

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
stable_classification <- function(dataset, labels, number_of_features, number_of_folds, feature_ranking_method, method_name,
                           classification_file='classification_errors.txt',
                           feature_ranking_file='feature_rankings.txt',
                           decision_value_file='decision_value.txt',
                           test_labels_file='test_label_file.txt',
                           size_of_subsample=0.9)
{
  for(i in 1:number_of_folds)
{
  do_stable_classification(dataset, labels, number_of_features, feature_ranking_method,
                           classification_file,
                    feature_ranking_file,
                    decision_value_file,
                    test_labels_file,
                    size_of_subsample)
}
  rez <- juozometodas()
  unlink(c('classification_errors.txt', 'decision_value.txt', 'feature_rankings.txt', 'test_label_file.txt'))
  return(rez)
}

do_stable_classification <- function(dataset, labels, number_of_features, feature_ranking_method,
                              classification_file,
                              feature_ranking_file,
                              decision_values_file,
                              test_labels_file,
                              size_of_subsample=0.9)
{
  train_size <- round(length(dataset[1, ]) * size_of_subsample)
  train_indexes <- sample(1:length(dataset[1, ]), train_size)
  test_indexes <- setdiff(1:length(dataset[1, ]), train_indexes)
  train_labels <- labels[train_indexes]
  train_data <- dataset[, train_indexes]
  pos <- which(train_labels == -1, arr.ind=T)
  neg <- which(train_labels == 1, arr.ind=T)
  best_features <- get_stable_features(train_data, pos, neg, number_of_features, get_fisher_ranking)
#   print(best_features)
  write(best_features, file=feature_ranking_file, append=T, ncolumns=length(dataset[, 1]))
#   number_of_features <- seq(from=10, to=500, by=10)
  for (i in length(best_features))
  {
    train_data <- t(dataset[best_features, train_indexes])
    test_data <-  t(dataset[best_features, test_indexes])
    model <- best.svm(x=train_data, y=as.factor(labels[train_indexes]), kernel='linear', cost=0.01)
    guess <- predict(model, test_data, decision.values=T)
    write(c(i, attr(guess, 'decision.values')), file=decision_values_file, ncolumns=length(dataset[, 1]), append=T)
    write(c(i, labels[test_indexes]), file=test_labels_file, ncolumns=length(dataset[, 1]), append=T)
    trez <- table(labels[test_indexes], guess[1:length(labels[test_indexes])], useNA="always")
    to_file <- c(i, trez[1,2] + trez[2,1], trez[1,1], trez[1,2], trez[2,1], trez[2,2])
    write(to_file, ncolumns=length(dataset[, 1]),
          file=classification_file, append=T)
    rm(model, guess, train_data, test_data, trez)
    gc()
  }
  return(T)
}

juozometodas <- function()
{
  moo <- read.csv(file='classification_errors.txt', sep=" ", header=F)
  error_rate <- mean(moo[, 2])/6
  moo <- read.csv(file='feature_rankings.txt', sep=" ", header=F)
  overal_jaccard_stability <- get_overall_jaccard_index(moo)
  overal_hamming_stability <- get_overall_hamming_distance(moo)
  return(c(error_rate, overal_jaccard_stability, overal_hamming_stability))
}


measure_stability <- function(dataset, labels, feature_ranking_method, method_name)
{
  K <- seq(from=20, to=200, by=10)
  for (i in K)
  {
    err_stab <- stable_classification(nncolon, labels, i, number_of_folds=10, feature_ranking_method, method_name)
    print(c(i, err_stab))
    write(c(i, err_stab), file=paste('stability_of_', method_name, '.txt', sep=""), append=T, ncolumns=length(dataset[, 1]))
  }
}
