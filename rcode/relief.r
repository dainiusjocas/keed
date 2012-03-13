################################################################################
# Library for computing the relevance criteria
################################################################################

library(multicore)
library(doMC)
#registerDoMC()

# ENTRY POINT
# This method computes the relevance of feature for relief feature ranking
#   method
# input: dataset: rows - features, collumns - tuples
# input: indexes_of_classes - list of two lists: first - indexes of normal
#  patients, second - patients with tumor (this paremeter can be prepared
#  with get_indexes_of_positives_and_negatives method)
get_relevance <- function(dataset, pos, neg)
{
  indexes_of_classes <- c(list(pos), list(neg))
  distance_matrix <- get_distance_matrix(dataset)
  dataset_as_list <- c()
  for(i in 1:length(dataset[,1])) 
  {
    dataset_as_list[i] <- list(dataset[i, ])
  }
  result_as_list <- mclapply(X=dataset_as_list,
                             FUN=get_rel_val, 
                             distance_matrix = distance_matrix,
                             indexes = indexes_of_classes)
  result_as_vector <- c()
  for(i in 1:length(dataset[,1]))
  {
    result_as_vector[i] <- result_as_list[[i]]
  }
  return(result_as_vector)
}

# This method computes distance matrix and return it as a matrix
# Measures distance between columns
# input: dataset - (lines - features), (column - tuple)
# output: distance matrix
get_distance_matrix <- function(dataset)
{
  distance_matrix <- dist(t(dataset), method = "euclidean", upper = TRUE, diag = TRUE)
  return(as.matrix(distance_matrix))
}

# This method computes relevance value of the feature. 
# input: x - values of all the features
# input: distance_matrix
# input: indexes
get_rel_val <- function(x, distance_matrix, indexes)
{
  relevance_of_feature <- 0
  minimal_value <- min(x)
  maximal_value <- max(x)
  index_of_nearest_hit <- 0
  index_of_nearest_miss <- 0
  number_of_tuples <- nrow(as.matrix(x))
  for (i in 1:number_of_tuples) 
  {
    value_of_feature_j <- x[i]
    if (any(i == indexes[[2]]))
    { # tumor between tumors
      index_of_nearest_hit <- 
        get_index_of_nearest_neigbour_from_specific_class(i, distance_matrix, indexes[[2]])
      index_of_nearest_miss <- 
        get_index_of_nearest_neigbour_from_specific_class(i, distance_matrix, indexes[[1]])      
    } 
    else 
    { # normal between normal
      index_of_nearest_hit <- 
        get_index_of_nearest_neigbour_from_specific_class(i, distance_matrix, indexes[[1]])
      index_of_nearest_miss <- 
        get_index_of_nearest_neigbour_from_specific_class(i, distance_matrix, indexes[[2]])
    }  
    relevance_of_feature <- 
      relevance_of_feature - 
      (relief_difference(value_of_feature_j, x[index_of_nearest_hit],
                        maximal_value, minimal_value) / number_of_tuples) + 
      (relief_difference(value_of_feature_j, x[index_of_nearest_miss],
                        maximal_value, minimal_value) / number_of_tuples)      
  }
  return(relevance_of_feature)
}

# This method finds the nearest neighbour and returns it's index in vector of
#   features
# input: index_of_tuple - tuple to which we'll search the nearest neighbour
# input: distance_matrix
# input: indexes_of_specific_class - indexes of columns in the distance matrix
#   in which we'll search the nearest neighbour
# output: index_of_nearest_neighbour
get_index_of_nearest_neigbour_from_specific_class <- function(index_of_tuple,
                                                              distance_matrix, 
                                                     indexes_of_specific_class)
{
  more_than_zero <- 
    which(distance_matrix[index_of_tuple, indexes_of_specific_class] > 0)
  index <- which(distance_matrix[index_of_tuple, indexes_of_specific_class] == 
    min(distance_matrix[index_of_tuple, indexes_of_specific_class[more_than_zero]]))
  index_of_nearest_neigbour <- indexes_of_specific_class[index]
  return(index_of_nearest_neigbour)
}

# This method computes the difference of feature between samples 'value'
# and 'nearest_hit_or_miss' and divides ir from the difference of maximal and
# minimal values of that feature.
# input: value - , 
#   nearest_hit_or_miss - ,
#   max_value - ,
#   min_value - ,
# output: difference - difference for relief function.
relief_difference <- function(value, 
                              nearest_hit_or_miss,
                              max_value, 
                              min_value)
{
  difference <- 
    abs(value - nearest_hit_or_miss) /
    (max_value - min_value)
  return(difference)
}

