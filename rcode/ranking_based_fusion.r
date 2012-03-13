###############################################################################
# Library to compute Ranking-Based Multicriterion Fusion
###############################################################################

# ENTRY POINT
# This method gets ranked-based fusion of features. We get values values of 
# features measured with some algorithm ex. fisher sration, relief. 
# Firstly, we need to get the ranks of the features. Then we will count the
#   fusion of ranks.
# input: list_of_scores: data.frame where every frame is a vector of measures
#   for the weight of a feature
# output: fusion_of_ranks - sum of ranks of every feature. Best features with
#   the highest values.
get_ranking_based_fusion <- function(list_of_scores)
{
  ranked_features <- rank_features(list_of_scores)
  fusion_of_ranks <- get_fusion_of_ranks(ranked_features)
  return(fusion_of_ranks)
}

# This method computes the ranking-based fusion of features by formula:
#   values = sum(v[i], i=1..m), where m - number of features
# input: ranked_features - feature <-> score
# output: fusion_of_ranks - vector where every value is sum of ranks of 
#   feature.
get_fusion_of_ranks <- function(ranked_features)
{
  fusion_of_ranks <- c()
  for (i in 1:length(ranked_features[ , 1]))
  {
    fusion_of_ranks[i] <- sum(ranked_features[i, ])
  }
  return(fusion_of_ranks)
}

# This method assigns ranks to the features. It just sorts values of 
#   the features and returns indexes of those features if the ranked list.
#   Features are sorted in ascending order in order to assign higher values to
#   the best features measured by specific method of evaluation.
# input: list_of_scores - data.frame where every frame is a vector of measures
#   of the feature.
# output: ranked_features - data.frame where every frame is a vector of features
#   with their number in the sorted list.
rank_features <- function(list_of_scores)
{
  ranked_features <- NULL
  for (i in 1:length(list_of_scores[1, ]))
  {
    ranked_features <- cbind(ranked_features, sort(sort(list_of_scores[ , i], index.return=T)$ix,
                                 index.return=T)$ix)
  }
  return(ranked_features)
}

