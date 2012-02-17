# input: train - range in dataset to train
# input: best_features - vector of indexes of best features
classify <- function(dataset, labels, best_features, train_indexes, test_indexes)
{
  train_data <- t(dataset[best_features, train_indexes])
  test_data <-  t(dataset[best_features, test_indexes])
  model <- svm(train_data, as.factor(labels[train_indexes]))
  pred <- predict(model, test_data)
  table(pred, labels[test_indexes])
}

# input: train - range in dataset to train
# input: best_features - vector of indexes of best features
classify_with_folds <- function(dataset, labels, train_size, number_of_folds)
{
  errors <- c(seq(0, 0, length.out=number_of_folds))
  for (i in 1:number_of_folds)
  {
    train_indexes <- sample(1:length(dataset[1, ]), train_size)
    test_indexes <- setdiff(1:length(dataset[1, ]), train_indexes)
    train_data <- dataset[, train_indexes] # buvo transponuota
    #test_data <-  t(dataset[, test_indexes])
    train_labels <- labels[train_indexes]
    pos <- which(train_labels == 1, arr.ind=T)
    neg <- which(train_labels == -1, arr.ind=T)
    best_features <- get_best_features(train_data, pos, neg)
    train_data <- train_data[best_features, ]
    model <- svm(t(train_data), as.factor(labels[train_indexes]))
    pred <- predict(model, t(test_data))
    for(j in 1:length(test_indexes))
    {
      if (as.vector(pred)[j] != as.factor(labels[test_indexes])[j]) {
        errors[i] <- errors[i] + 1
      }
    }
  }
  print(mean(errors))
}