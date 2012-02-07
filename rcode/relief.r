################################################################################
# Library for computing the relevance criteria
################################################################################

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
  result <- apply(dataset, 1, get_rel_val, distance_matrix = distance_matrix,
                  indexes = indexes_of_classes)
  return(result)
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
    { # normal between normals
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

# This method separates indexes of patiens with positive tissues and negative
# tisues
# input: patients - list of patiens. negative sign - tumor; positive - normal.
# output: vector of two vectors, where first vector contains indexes of
#   patiens with normal tissue, second vector contains indexes of tumor tissues.
get_indexes_of_positives_and_negatives <- function(patients)
{
  indexes_of_positives <- c()
  indexes_of_negatives <- c()
  result <- c()
  for (i in 1:length(patients[ , 1]))
  {
    if (patients[i, ] > 0) 
    {
      indexes_of_positives <- 
        append(indexes_of_positives, c(i), length(indexes_of_positives))
    }
    else 
    {
      indexes_of_negatives <- 
        append(indexes_of_negatives, c(i), length(indexes_of_negatives))
    }
  }
  result[[1]] <- indexes_of_positives
  result[[2]] <- indexes_of_negatives
  return(result)
}
