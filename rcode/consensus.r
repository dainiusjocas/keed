###############################################################################
# Implementation of CGS (Consensus Group Stable feature selection) algorithm
###############################################################################

library(e1071)
library(doMC)
library(multicore)
library(iterators)
library(foreach)
source('dgf.r')
source('fscore.r')

# ENTRY POINT
# input: dataset: rows - tuples, columns - features !!!
# input: pos - positive tuples, neg - negative tuples
# input: number_of_subsamplings - number of boostraped groups
# input: relevance_measurement_method - function to compute a relevance.
#   function must take three parameters: dataset, pos, neg, and return vector
#   of length equal to number of features
# output: vector of several best ranked features
# EXAMPLE: get_consensus_groups(mymatrix, c(1,2), c(3,4,5), 2, get_fisher_scores)
get_consensus_groups <- function(dataset, pos, neg,
                                 number_of_subsamplings,
                                 relevance_measurement_method)
{
  looprez <- foreach (i = 1:number_of_subsamplings) %dopar%
  {
    sample_indexes <- sample(1:length(dataset[, 1]),
                             length(dataset[, 1]) * 0.632)
    dense_groups <- DGF(dataset[sample_indexes, ], 0.7)
    get_indexes_of_clusters(dense_groups)
  }
  indexes_in_groups <- NULL
  for (i in 1:number_of_subsamplings)
  {
    indexes_in_groups <- rbind(indexes_in_groups, as.vector(looprez[[1]]))
  }
  frequencies <- matrix(seq(from=0, to=0,
                            length.out=length(dataset[1, ]) ** 2),
                        ncol=length(dataset[1, ]))
  for (i in 1:length(dataset[1, ]))
  {
    for (j in i:length(dataset[1, ])) 
    {
      diff_vector <- indexes_in_groups[,i] == indexes_in_groups[,j]
      diff_vector[which(is.na(diff_vector))] <- F 
      frequencies[i, j] <- sum(diff_vector)
      frequencies[j, i] <- sum(diff_vector)
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

get_indexes_of_clusters <- function(dense_groups)
{
  indexes_of_cluster <- c()
  for(i in 1:length(dense_groups))
  {
    indexes_of_cluster[dense_groups[[i]]$features] <- i
  }
  return(indexes_of_cluster)
}

consensus_performance <- function(dataset, bp, sz, cc, start, end, relevance_method)
{
  number_of_subsamplings <- 8
  rez <- data.frame(size=0, time=0)
  amount_of_features <- seq(start, end, by=100)
  for(i in amount_of_features)
  {
    for(j in 1:1)
    {
      begin <- Sys.time()
      groups <- get_consensus_groups(t(dataset[1:i, c(bp, cc)]),
                                     1:length(bp),
                                     ((length(bp)+1):(length(bp)+length(cc))),
                                     number_of_subsamplings,
                                     relevance_method)
      duration <-  c(i, Sys.time() - begin)
      write(duration, file='performance_consensus.txt', append=T)
    }
    print(duration)
    gc()
  }
  return(duration)
}

# This method return feture ranking according to CGS algorithm
# NOTE: NUMBER OF RANKS RETURNED IS VARYING
get_cgf_ranking <- function(dataset, pos, neg)
{
  dataset <- t(dataset)
  ranking <- get_consensus_groups(dataset, pos, neg, 10, get_fisher_scores)
  return(ranking)
}