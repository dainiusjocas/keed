library(foreach)
library(doMC)
library(e1071)
source('fusion.r')
source('consensus.r')

# ENTRY POINT
# this method use svm and best feature selection to classify dataset.
# input: dataset - rows - features, columns - tuples
# input: labels: vector of labels of classes
# input: number_of_best_features - how many features we will use in 
#   classification
# input: train_size - amount of tuples that will be used to train model
# input: number_of_folds - how many times we will repeat the classification
# output: model - the best model for classification
classify_with_folds <- function(dataset, labels, number_of_best_features,
                                train_size, number_of_folds)
{
  classification_errors <- foreach (i = 1:number_of_folds) %dopar%
  {
    test_classify(dataset, labels, number_of_best_features, train_size)
  }
  for (i in 1:length(classification_errors))
  {
    write(classification_errors[[i]], file='cerrors.txt', append=T)  
  }
}

classify_with_mcf_rfe <- function(dataset, labels, train_size, number_of_folds)
{
  registerDoMC(2)
  print(getDoParWorkers())
  features <- seq(170, 250, by=10)
  for(j in features)
  {
    classification_errors <- foreach(i = 1:number_of_folds) %dopar%
    {
      do_classification(dataset, labels, train_size, j)
    }
    for(k in 1:length(classification_errors))
    {
      write(c(j, classification_errors[[k]]), file='rez/classification_fusion_2.txt', append=T)
    }
  }
  return(rez)
}

do_classification <- function(dataset, labels, train_size, number_of_features)
{
  train_indexes <- sample(1:length(dataset[1, ]), train_size)
  test_indexes <- setdiff(1:length(dataset[1, ]), train_indexes)
  train_data <- dataset[, train_indexes] # buvo transponuota
  train_labels <- labels[train_indexes]
  pos <- which(train_labels == 1, arr.ind=T)
  neg <- which(train_labels == -1, arr.ind=T)
  best_features <- get_mcf_rfe(train_data, pos, neg, number_of_features)
  write(best_features[number_of_features], file='rez/best_features_fusion_2.txt', append=T, ncolumns=100)
  write(" ", file='rez/best_features_fusion_2.txt', append=T)
  train_data <- train_data[best_features, ]
  model <- best.svm(x=t(train_data), y=as.factor(labels[train_indexes]), kernel='linear', cost=0.01)
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
  return(errors)
}

# output: number of errors
test_classify <- function(dataset, labels, number_of_best_features,
                          train_size)
{
  train_indexes <- sample(1:length(dataset[1, ]), train_size)
  test_indexes <- setdiff(1:length(dataset[1, ]), train_indexes)
  train_data <- dataset[, train_indexes] # buvo transponuota
  train_labels <- labels[train_indexes]
  pos <- which(train_labels == 1, arr.ind=T)
  neg <- which(train_labels == -1, arr.ind=T)
  best_features <- get_best_features(train_data, pos, neg)
  write(best_features, file='bfeatures.txt', append=T, ncolumns=100)
  write(" ", file='bfeatures.txt', append=T)
  train_data <- train_data[best_features[1:number_of_best_features], ]
  model <- svm(t(train_data), as.factor(labels[train_indexes]), kernel="linear")
  test_data <-  t(dataset[best_features[1:number_of_best_features], test_indexes])
  pred <- predict(model, (test_data))
  errors<- 0
  for(j in 1:length(test_indexes))
  {
    if (as.vector(pred)[j] != as.factor(labels[test_indexes])[j]) {
      errors <- errors + 1
    }
  }
  return(errors)
}

measure_performance <- function(dataset, bp, sz, cc, start, end)
{
  amount_of_features <- seq(start, end, by=500)
  for(i in amount_of_features)
  {
    for(j in 1:3)
    {
      begin <- Sys.time()
      get_best_features(dataset[1:i, c(bp, cc)], 1:length(bp),
                        (length(bp) + 1):(length(bp)+length(cc)))
      duration <-  c(i, Sys.time() - begin)
      write(duration, file='performance_fusion.txt', append=T)
    }
  }
  return(TRUE)
}

measure_methods_performance <- function(dataset, bp, sz, cc, start, end)
{
  amount_of_features <- seq(start, end, by=500)
  for(i in amount_of_features)
  {
    for(j in 1:3)
    {
      begin <- Sys.time()
      get_fisher_scores(dataset[1:i, c(bp, cc)], 1:length(bp),
                        (length(bp) + 1):(length(bp)+length(cc)))
      duration_fisher <- Sys.time() - begin
      begin <- Sys.time()
      get_relief_scores(dataset[1:i, c(bp, cc)], 1:length(bp),
                        (length(bp) + 1):(length(bp)+length(cc)))
      duration_relief <- Sys.time() - begin
      begin <- Sys.time()
      get_adc(dataset[1:i, c(bp, cc)], 1:length(bp),
                    (length(bp) + 1):(length(bp)+length(cc)))
      duration_adc <- Sys.time() - begin
      begin <- Sys.time()
      get_svm_feature_weights(dataset[1:i, c(bp, cc)], 1:length(bp),
              (length(bp) + 1):(length(bp)+length(cc)))
      duration_svm <- Sys.time() - begin
      begin <- Sys.time()
      get_best_features(dataset[1:i, c(bp, cc)], 1:length(bp),
                        (length(bp) + 1):(length(bp)+length(cc)))
      duration_fusion <-  Sys.time() - begin
      write(c(i, duration_fisher, duration_relief, duration_adc, duration_svm, duration_fusion), file='rez/all_performance.txt', append=T, ncolumns=10)
      gc()
    }
  }
}

measure_adc_performance <- function(dataset, bp, sz, cc, start, end)
{
  amount_of_features <- seq(start, end, by=500)
  for(i in amount_of_features)
  {
    for(j in 1:3)
    {
      begin <- Sys.time()
      get_adc(dataset[1:i, c(bp, cc)], 1:length(bp),
              (length(bp) + 1):(length(bp)+length(cc)))
      duration_adc <- Sys.time() - begin
      write(c(i, duration_adc), file='rez/performance_adc.txt', append=T)
    }
  }
}

measure_cgs_performance <- function(dataset, bp, sz, cc, start, end)
{
  amount_of_features <- seq(start, end, by=500)
  for(i in amount_of_features)
  {
    for(j in 1:3)
    {
      begin <- Sys.time()
      get_consensus_groups(t(dataset[1:i, c(bp, cc)]), 1:length(bp),
              (length(bp) + 1):(length(bp)+length(cc)), 8, get_fisher_scores)
      duration_adc <- Sys.time() - begin
      write(c(i, duration_adc), file='rez/cgs_performance.txt', append=T)
    }
  }
}
