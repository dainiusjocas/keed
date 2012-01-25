# This method computes so called relevance parameter for the dimentions of dataset
# input: dataset - pacientu matrica, kur eilutes - savybes, stulpeliai - 
#   pacientai
# input: indexes - sarasas kurio pirmas elementas yra sarasas, kuriame yra
#   sveiku pacientu indeksai, o antrame elemente yra pacientu turinciu augli
# output: relevance_vector - vektorius, kuriame yra sudeti savybiu relief vertes
update_relevance <- function(dataset, indexes)
{
  #print(length(dataset[ , 1])) # number of features
  #print(length(dataset[1, ]))  # number of patients
  relevance_vector <- c(seq(0, 0, length.out = length(dataset[ , 1])))
  print(relevance_vector)
  for (j in 1:length(dataset[ , 1])) # iterate through feature
  {
    max_value_of_feature_j <- max(dataset[j, ])
    min_value_of_feature_j <- min(dataset[j, ])
    index_of_nearest_hit <- 0
    index_of_nearest_miss <- 0
    for (i in 1:length(dataset[1, ])) # iterate through patients
    {
      value_of_feature_j <- dataset[j, i]
      if (is_tumor(i, indexes[[2]]))
      { # patients with tumor      
        index_of_nearest_hit <- get_index_of_nearest_neigbour(i, indexes[[2]],
                                                            dataset)
        index_of_nearest_miss <- get_index_of_nearest_neigbour(i, indexes[[1]],
                                                             dataset)
      } 
      else 
      { # normal patients
        index_of_nearest_hit <- get_index_of_nearest_neigbour(i, indexes[[1]],
                                                            dataset)
        index_of_nearest_miss <- get_index_of_nearest_neigbour(i, indexes[[2]],
                                                             dataset)
      }
      feature_value_of_nearest_hit <- dataset[j, index_of_nearest_hit]
      feature_value_of_nearest_miss <- dataset[j, index_of_nearest_miss]
      relevance_vector[j] <- 
        relevance_vector[j] - 
        relief_difference(value_of_feature_j, feature_value_of_nearest_hit, 
                          max_value_of_feature_j, min_value_of_feature_j) + 
        relief_difference(value_of_feature_j, feature_value_of_nearest_miss, 
                          max_value_of_feature_j, min_value_of_feature_j)
    }
  }
  return(relevance_vector)
}

# This method finds the index of the nearest neighbour to our patient from
# the list of indexed neigbours
# input: index of patient
# input: indexes of patients that belong to one class - normal or tumor
# input: dataset of patients
# output: index of nearest neigbour
get_index_of_nearest_neigbour <- function(index_of_patient,
                                 indexes_of_neighbours, dataset)
{
  indexes_of_neigbours_without_my_patient <- setdiff(indexes_of_neighbours,
                                                  index_of_patient)
  distances <- c()
  k <- 1 # index of list distances
  for (i in indexes_of_neigbours_without_my_patient)
  {
    distances[[k]] <- c(dist(rbind(dataset[ , index_of_patient],
                               dataset[ , i]), method="euclidean"), i)
    k <- k + 1
  }
  min_value <- distances[[1]][1]
  min_index <- distances[[1]][2]
  for (j in length(distances))
  {
    if (min_value > distances[[j]][1])
    {
      min_value <- distances[[j]][1]
      min_index <- distances[[j]][2]
    } 
  }
  return(min_index)
}

# This method checks if the index of patient is in the indexes of patients
# with tumor.
# input: index_of_patient - index of patient in patients db
# input: indexes_of_patients_with_tumor - --||-- in patients db
# output: 
is_tumor <- function(index_of_patient, indexes_of_patients_with_tumor)
{
  result <- FALSE
  if (any(index_of_patient == indexes_of_patients_with_tumor))
  {
    result <- TRUE
  }
  return(result)
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

# This method computes the difference of feature between samples 'value'
# and 'nearest_hit_or_miss' and divides ir from the difference of maximal and
# minimal values of that feature.
# input: value - , 
#   nearest_hit_or_miss - ,
#   max_value - ,
#   min_value - ,
# output: difference - difference for relief function.
relief_difference <- function(value, nearest_hit_or_miss,
                              max_value, min_value)
{
  difference <- abs(value - nearest_hit_or_miss) /
    (max_value - min_value)
  return(difference)
}
