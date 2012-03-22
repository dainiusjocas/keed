###############################################################################
# Library for measuring robustness of feature selection
###############################################################################

# This method computes Koncheva's index (KI) - measure of a stability
# KI(fi, fj) = (r * N - s^2) / (s * (N-s)) = (r - (s^2/N)) / (r - (s^2/N)),
# where
#   KI - stability index between fi and fj
#   fi, fj - feature rankings
#   s - size of fi or size of fj
#   r - length of intersection of fi and fj
#   N - number of features of original dataset 
# input: feature_ranking_i
# input: feature_ranking_j
# input: number_of_features of the dataset
# output: KI - stability index
get_koncheva_index <- function(feature_ranking_i,
                           feature_ranking_j,
                           number_of_features)
{
  s <- length(feature_ranking_i)
  r <- length(intersect(feature_ranking_i, feature_ranking_j))
  N - number_of_features
  KI <- (r * N - s^2) / (s * (N - s))
  return(KI)
}

# This method computer overal stability of feature selection method.
# Stot = (2 * SUM(SUM(KI(fi, fj), j=i+1..k), i=1..k))/(k * (k - 1)), where
#   Stot - average over all pairwise similarity comparisons between all 
#     signatures on the k subsamplings - overal stability
#   k - number of subsamplings
#   KI - Koncheva index
#   fi, fj - i^th and j^th feature rankings
# input: rankings - feature rankings of some dataset - a matrix where every row
#   is a ranking, first number is an index of best ranked feature.
# input: percentage - fraction of number of best ranked features. Default is 5%
# output: stability
overal_stability <- function(rankings, percentage=0.05)
{
  stability <- 0
  k <- length(rankings[, 1])
  total_number_of_features <- length(rankings[1, ])
  number_of_best_features <- round(k * percentage)
  best_rankings <- ranking[, 1:number_of_best_features]
  for (i in 1:k)
  {
    for (j in (i + 1):k)
    {
      stability <- 
        stability + 
        get_koncheva_index(rankings[i, ],
                           rankings[j, ],
                           total_number_of_features)
    }
  }
  stability <- (2 * stability) / (k * (k - 1))
  return <- stability
}
