source('fscore.r')
source('quality.r')
source('adc.r')
source('svm_weights.r')
source('ranking_based_fusion.r')
source('score_based_fusion.r')

# ENTRY POINT
# input: dataset - rows - features, columns - tuples
# input: pos - positive tuples (sick patients)
# input: neg - negative tuples
# output: list of ranked features
get_best_features <- function(dataset, pos, neg)
{
  list_of_scores <- get_list_of_scores(dataset, pos, neg)
  ranks_of_best_features <- get_score_based_feature_ranking(list_of_scores)
  list_of_scores <- cbind(list_of_scores,
                          weights=ranks_of_best_features)
  ranks_of_best_features <- get_ranking_based_fusion(list_of_scores)
  best_features <- sort(ranks_of_best_features, decreasing=T, index.return=T)$ix
  return(best_features)
}

# input: dataset: rows - features, collumns - tuples
# input: pos - 
# input: neg -  
# output: best_features - indexes of features sorted from the best to the
#   worst. Ex. best_features[1] = 493 means that feature with number 493 is
#   the best feature.
get_best_features_by_scores <- function(dataset, pos, neg)
{
  list_of_scores <- get_list_of_scores(dataset, pos, neg)
  ranks_of_best_features <- get_ranking_based_fusion(list_of_scores)
  best_features <- sort(ranks_of_best_features, decreasing=T, index.return=T)$ix
  return(best_features)
}

# input: dataset: rows - features, collumns - tuples
# input: pos - 
# input: neg -  
# output: best_features - indexes of features sorted from the best to the
#   worst. Ex. best_features[1] = 493 means that feature with number 493 is
#   the best feature.
get_best_features_by_weight <- function(dataset, pos, neg)
{
  list_of_scores <- get_list_of_scores(dataset, pos , neg)
  ranks_of_best_features <- get_score_based_feature_ranking(list_of_scores)
  best_features <- sort(ranks_of_best_features, decreasing=T, index.return=T)$ix
  return(best_features)
}

# this method computes scores of features of dataset.
# input: dataset - rows - features, columns - tuples
# input: pos - indexes of normal patients
# input: neg - indexes of patients with tumor
# output: list_of_scores - list of vectors, where every vector is filled with
#   weights of features
get_list_of_scores <- function(dataset, pos, neg)
{
  list_of_scores <- NULL
  list_of_scores <- cbind(list_of_scores, 
                          fisher=get_fisher_scores(dataset, pos, neg))
  list_of_scores <- cbind(list_of_scores, 
                          relief=get_relief_scores(dataset, pos, neg))
  list_of_scores <- cbind(list_of_scores, 
                          adc=get_adc(dataset, pos, neg))
  list_of_scores <- cbind(list_of_scores, 
                        svm_weights=get_svm_feature_weights(dataset, pos, neg))
  return(list_of_scores)
}

###############################################################################
# MCF-RFE implementation library
###############################################################################

# This method gets n best features using fusion feature ranking algorithm
#   recursively 
# input: dataset - rows -> features; columns - tuples
# input: pos - first class, neg - second class
# input: n - how many best features we want to get (default=20)
get_mcf_rfe <- function(dataset, pos, neg, n=20)
{
  indexes = c(1:length(dataset[ , 1]))
  i <- length(dataset[ , 1])
  while (i > 2 * n)
  {
    ranks <- get_best_features(dataset[indexes, ], pos, neg) 
    i <- round(i * 0.5)
    indexes <- indexes[-tail(ranks, n=i)]
  }
  while (i >= n)
  {
    ranks <- get_best_features(dataset[indexes, ], pos, neg)
    indexes <- indexes[-tail(ranks, n=1)]
    i <- i - 1
  }
  return(indexes)
}

###############################################################################
# MCF-RFE implementation library
###############################################################################

# This method gets n best features using fusion feature ranking algorithm
#   recursively 
# input: dataset - rows -> features; columns - tuples
# input: pos - first class, neg - second class
# input: n - how many best features we want to get (default=20)
get_mcf_rfe_ranking <- function(dataset, pos, neg)
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

