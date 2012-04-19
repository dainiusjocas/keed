get_stable_features <- function(dataset, pos, neg, number_of_features,
                                feature_ranking_method=get_fisher_ranking,
                                number_of_subsampling=10,
                                size_of_subsample=0.9, 
                                threshold=1)
{
  labels <- c()
  labels[pos] <- 1
  labels[neg] <- -1
  train_size <- round(length(dataset[1, ]) * size_of_subsample)
  ranks <- c()
  for (i in 1:number_of_subsampling)
  {
    train_indexes <- sample(1:length(dataset[1, ]), train_size)
    train_labels <- labels[train_indexes]
    train_data <- dataset[, train_indexes]
    pos <- which(train_labels == -1, arr.ind=T)
    neg <- which(train_labels == 1, arr.ind=T)
    best_features <- feature_ranking_method(train_data, pos, neg)
    ranks <- rbind(ranks, best_features)
    gc()
  }
  stable_features <- get_all_stable_features(ranks, number_of_features, threshold)
  return(stable_features)
}

# input: ranks - matrix of ranking; in one line one ranking
# input: percentage - what fraction of line to take
get_unique_features <- function(ranks, number_of_features)
{
  unique_features <- c()
  how_many <- number_of_features
  for (i in 1:length(ranks[, 1]))
  {
    unique_features <- union(unique_features, ranks[i, 1:how_many])
  }
  return(unique_features)
}

# This method checks if feature id in subset of rankings occurs not less
#   frequently than there were rankings. And if feature_id occurs not less
#   frequently than feature is stable, otherwise - not stable.
# input: feature_id
# input: ranks - list of ranking - every row one ranking
# input: percentage - fraction of ranking length. Default = 0.5
# input: threshold - by passign parameter less than 1 we can make stability
#   criteria less rigid. Defauls is 1 - very rigid
# output: TRUE of FALSE
is_feature_stable <- function(feature_id, ranks, number_of_features, threshold=1)
{
  stability_criteria <- length(ranks[, 1]) * threshold
  ranks_to_check <- ranks[, 1:number_of_features]
  observations <- length(which(ranks_to_check == feature_id))
  if (observations >= stability_criteria)
  {
    return(T)
  }
  return(F)
}

# This method collects all stable features from feature rankings
# input: ranks
# input: percentage
# input: threshold
# output: stable_features - vector of features that are stable
get_all_stable_features <- function(ranks, number_of_features, threshold=1)
{
  unique_features <- get_unique_features(ranks, number_of_features)
  stable_features <- c()
  for(i in unique_features)
  {
    if (is_feature_stable(i, ranks, number_of_features, threshold))
    {
      stable_features <- c(stable_features, i)
    }
  }
  return(sort(stable_features))
}
