###############################################################################
# Library to compute Score-Based Multicriterion Fusion
###############################################################################

# ENTRY POINT
# This method computes score-based multicriterion fusion of feature scores
# input: list_of_scores: rows - scores of feature, 
#   columns - ranking methods (actually data frame is expected)
# output: fusioned list of scores of features
get_score_based_feature_ranking <- function(list_of_scores)
{
  normalized_list_of_scores <- get_normalized_scores(list_of_scores)
  fusion_of_scores <- get_fusion_of_scores(normalized_list_of_scores)
  return(fusion_of_scores)
}

# This method normalizes the list of scores by formula:
# U[i]' = (U[i]-U[i[min]]) / (U[i[max]] - U[i[min]]), where
# U[i] is scores of one ranking method
# input: list_of_scores: rows - scores of feature,
#   columns - scores by ranking methods
# output: normalized_list_of_scores - all values falls into [0..1]
get_normalized_scores <- function(list_of_scores)
{
  normalized_list_of_scores <- list_of_scores
  for (i in 1:length(list_of_scores[1, ]))
  {
    new_score <- 
      (list_of_scores[, i]) - min(list_of_scores[ , i])
    normalized_list_of_scores[ , i] <- new_score /
      (max(list_of_scores[ , i]) - min(list_of_scores[ , i]))
  }
  return(normalized_list_of_scores)
}

# This method computes fusion of scores by formula:
# U = (1/m) * sum(U[i]', i=1..m), where m - number of methods used,
#   U[i]' - normalized values of scores computed by one method
# output: fusion_of_scores - vector of values of fusioned feature scores
get_fusion_of_scores <- function(normalized_list_of_scores)
{
  fusion_of_scores <- c()
  number_of_methods <- length(normalized_list_of_scores[1, ])
  for (i in 1:length(normalized_list_of_scores[, 1]))
  {
    fusion_of_scores[i] <- 
      sum(normalized_list_of_scores[i, ]) / 
      number_of_methods
  }
  return(fusion_of_scores)
}

