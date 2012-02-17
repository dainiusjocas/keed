library(e1071)
source('fusion.r')

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
  errors <- c(seq(100000, 100000, length.out=number_of_folds))
  best_model <- NULL
  for (i in 1:number_of_folds)
  {
    train_indexes <- sample(1:length(dataset[1, ]), train_size)
    test_indexes <- setdiff(1:length(dataset[1, ]), train_indexes)
    train_data <- dataset[, train_indexes] # buvo transponuota
    
    train_labels <- labels[train_indexes]
    pos <- which(train_labels == 1, arr.ind=T)
    neg <- which(train_labels == -1, arr.ind=T)
    best_features <- get_best_features(train_data, pos, neg)
    train_data <- train_data[best_features[1:number_of_best_features], ]
    
    model <- svm(t(train_data), as.factor(labels[train_indexes]))
    test_data <-  t(dataset[best_features[1:number_of_best_features], test_indexes])
    pred <- predict(model, (test_data))
    errors[i] <- 0
    for(j in 1:length(test_indexes))
    {
      if (as.vector(pred)[j] != as.factor(labels[test_indexes])[j]) {
        errors[i] <- errors[i] + 1
      }
    }
    print(errors[i])
    if (errors[i] == min(errors))
    {
      best_model <- model
    }
  }
  return(best_model)
}