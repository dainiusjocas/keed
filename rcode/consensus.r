###############################################################################
# Implementation of CGS (Consensus Group Stable feature selection) algorithm
###############################################################################

source('dgf.r')

# input: dataset: rows - tuples, columns - features
# input: pos - positive tuples, neg - negative tuples
# input: number_of_subsamplings - number of boostraped groups
# input: relevance_measurement_method - function to compute a relevance.
#   function must take three parameters: dataset, pos, neg, and return vector
#   of length equal to number of features
# EXAMPLE: get_consensus_groups(mymatrix, c(1,2), c(3,4,5), 2, get_fisher_scores)
get_consensus_groups <- function(dataset, pos, neg,
                                 number_of_subsamplings,
                                 relevance_measurement_method)
{
  indexes_in_groups <- NULL
  for (i in 1:number_of_subsamplings)
  {
    sample_indexes <- sample(1:length(dataset[, 1]),
                             length(dataset[, 1]) * 0.632)
    dense_groups <- DGF(dataset[sample_indexes, ], 1)
    indexes_in_groups <- rbind(indexes_in_groups,
                               get_indexes_of_groups(dense_groups))
  }
  frequencies <- matrix(seq(from=0, to=0,
                            length.out=length(dataset[1, ]) ** 2),
                        ncol=length(dataset[1, ]))
  for (i in 1:length(dataset[1, ]))
  {
    for (j in i:length(dataset[1, ])) 
    {
      for(k in 1:length(indexes_in_groups))
      {
        if (i %in% indexes_in_groups[[k]] && j %in% indexes_in_groups[[k]]) 
        {
          frequencies[i,j] <- frequencies[i,j] + 1
          if (i != j) 
          {
            frequencies[j,i] <- frequencies[j,i] + 1
          }
        }
      }
    }
  }
  frequencies <- frequencies / number_of_subsamplings
  diag(frequencies) <- 1
  my_hc <- hclust(d=as.dist(1-frequencies), method="average")
  #plot(my_hc)
  #abline(h=0.5)
  clusters <- cutree(my_hc, h=0.5)
  indexes_of_representative_features <- c()
  for (i in 1:max(clusters))
  {
    mean_vector <- 
      get_mean_vector(as.matrix(dataset[ , which(clusters == i, arr.ind=T)]))
    indexes_of_representative_features[i] <- 
      get_index_of_representative_feature(as.matrix(dataset),
                                 which(clusters == i, arr.ind=T),
                                 mean_vector)
  }
  relevance_scores <- 
    relevance_measurement_method(t(dataset), pos, neg) # cia galima ir fusion metoda ikalt
  sorted_relevance_scores <- 
    sort(relevance_scores, decreasing=T, index.return=T)
#   print(indexes_of_representative_features)
#   print(sorted_scores)
  consensus_features <- 
    sorted_relevance_scores$ix[which(sorted_relevance_scores$ix %in%
                                     indexes_of_representative_features)]
  return(consensus_features)
}

get_index_of_representative_feature <- function(data, indexes, mean_vector)
{
  min_distance <- Inf
  index_of_representative_feature <- 0
  for (i in indexes)
  {
    if (min_distance > sqrt(sum((mean_vector - data[, i]) ^ 2)))
    {
      min_distance <- sqrt(sum((mean_vector - data[, i]) ^ 2))
      index_of_representative_feature <- i
    }
  }
  return(index_of_representative_feature)
}

# This method counts mean vector of a cluster.
# input: data, where:
#   rows - values of features
#   columns - tuples
# output - matrix of one column where rows are mean of data
get_mean_vector <- function(data)
{
  mean_vector <- data[ , 1]
  for (i in 1:length(data[, 1])){
    mean_vector[i] <- mean(data[i, ])
  }
  return(as.matrix(mean_vector))
}

get_indexes_of_groups <- function(dense_groups)
{
  indexes_in_group <- list()
  for (i in 1:length(dense_groups)) 
  {
    indexes_in_group <- rbind(indexes_in_group,
                              list(dense_groups[[i]]$features))
  }
  return(indexes_in_group)
}
