###############################################################################
# Library for measuring robustness of feature selection
###############################################################################

# This method computes Koncheva's index (KI) - measure of a stability
# KI(fi, fj) = (r * N - s^2) / (s * (N-s)) = (r - (s^2/N)) / (s - (s^2/N)),
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
  N <- number_of_features
  if (N == s)
  {
    return(1)
  }
  KI <- (r * N - s^2) / (s * (N - s))
  return(KI)
}

# This method computer overal stability of feature selection method.
# Stot = (2 * SUM(SUM(KI(fi, fj), j=i+1..k-1), i=1..k))/(k * (k - 1)), where
#   Stot - average over all pairwise similarity comparisons between all 
#     signatures on the k subsamplings - overal stability
#   k - number of subsamplings
#   KI - Koncheva index
#   fi, fj - i^th and j^th feature rankings
# input: rankings - feature rankings of some dataset - a matrix where every row
#   is a ranking, first number is an index of best ranked feature.
# input: percentage - fraction of number of best ranked features. Default is 5%
# output: stability. if percentage is less that 0 or more than 1 then NaN is
#   returned
get_overal_stability <- function(rankings, percentage=0.05)
{
  if ((percentage < 0) || (percentage > 1))
  {
    return(NaN)
  }
  rankings <- as.matrix(rankings)
  stability <- 0
  k <- length(rankings[, 1])
  total_number_of_features <- length(rankings[1, ])
  number_of_best_features <- ceiling(total_number_of_features * percentage)
  best_rankings <- matrix(rankings[, 1:number_of_best_features],
                          ncol=number_of_best_features)
  for (i in 1:(k-1))
  {
    for (j in (i + 1):k)
    {
      stability <- 
        stability + 
        get_koncheva_index(best_rankings[i, ],
                           best_rankings[j, ],
                           total_number_of_features)
    }
  }
  stability <- (2 * stability) / (k * (k - 1))
  return(stability)
}

# This method plots the values of robustness, when specific amount
#   of best feaures is considered
# input: rankings - list of lists with feature ranking.
# input: method_name - name of method which was used to rank the
#   features
plot_robustness <- function(rankings, method_name)
{
  percentage <- c(0.005, 0.01, 0.02, 0.05, 0.1, 0.25, 0.5)
  KI_values <- c()
  for (i in 1:7)
  {
    KI_values[i] <- get_overal_stability(rankings, percentage[i])
  }
  x_labels <- c('0.5', '1', '2', '5', '10', '25', '50')
  plot(x=1:7, xaxt="n", y=KI_values, type='b', 
       xlab="Percentage of selected features",
       ylab='Kuncheva index', ylim=c(-0.5, 0.99), pch=0)
  axis(side=1, at=1:7, labels=x_labels)
  legend(x=1, y=-0.1, legend=method_name, pch=0, lty=1)
}

get_jaccard_index <- function(A, B)
{
  A <- A[which(!is.na(A))]
  B <- B[which(!is.na(B))]
  AB_intersection_size <- length(intersect(A, B))
  AB_union_size <- length(union(A, B))
  jaccard_index <- AB_intersection_size / AB_union_size
  return(jaccard_index)
}

get_overall_jaccard_index <- function(rankings)
{
  rankings <- as.matrix(rankings)
  stability <- 0
  k <- length(rankings[, 1])
  for (i in 1:(k-1))
  {
    for (j in (i + 1):k)
    {
      stability <- 
        stability + 
        get_jaccard_index(rankings[i, ], rankings[j, ])
    }
  }
  stability <- (2 * stability) / (k * (k - 1))
  return(stability)
}

# input: A - vector of indexes 
# input: B - vector of indexes
get_hamming_distance <- function(A, B)
{
  A <- A[which(!is.na(A))]
  B <- B[which(!is.na(B))]
  maximal_value <- max(A, B)
  new_A <- seq(from=0, to=0, length.out=maximal_value)
  new_B <- seq(from=0, to=0, length.out=maximal_value)
  new_A[A] <- 1
  new_B[B] <- 1
  hamming_distance <- sum(abs(new_A - new_B))
  return(hamming_distance)
}

get_overall_hamming_distance <- function(rankings)
{
  rankings <- as.matrix(rankings)
  stability <- 0
  k <- length(rankings[, 1])
  for (i in 1:(k-1))
  {
    for (j in (i + 1):k)
    {
      stability <- 
        stability + 
        get_hamming_distance(rankings[i, ], rankings[j, ])
    }
  }
  stability <- (2 * stability) / (k * (k - 1))
  return(stability)
}

