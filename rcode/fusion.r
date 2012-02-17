source('fscore.r')
source('relief.r')
source('adc.r')
source('svm_weights.r')
source('ranking_based_fusion.r')
source('score_based_fusion.r')

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
                          relief=get_relevance(dataset, pos, neg))
  list_of_scores <- cbind(list_of_scores, 
                          adc=get_adc(dataset, pos, neg))
  list_of_scores <- cbind(list_of_scores, 
                        svm_weights=get_svm_feature_weights(dataset, pos, neg))
  return(list_of_scores)
}
