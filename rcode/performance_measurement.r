library(foreach)
library(doMC)
library(e1071)
source('fusion.r')
source('consensus.r')

measure_performance <- function(dataset, bp, sz, cc, start, end)
{
  amount_of_features <- seq(start, end, by=500)
  for(i in amount_of_features)
  {
    for(j in 1:3)
    {
      begin <- Sys.time()
      get_best_features(dataset[1:i, c(bp, cc)], 1:length(bp),
                        (length(bp) + 1):(length(bp)+length(cc)))
      duration <-  c(i, Sys.time() - begin)
      write(duration, file='performance_fusion.txt', append=T)
    }
  }
  return(TRUE)
}

measure_methods_performance <- function(dataset, bp, sz, cc, start, end)
{
  amount_of_features <- seq(start, end, by=500)
  for(i in amount_of_features)
  {
    for(j in 1:3)
    {
      begin <- Sys.time()
      get_fisher_scores(dataset[1:i, c(bp, cc)], 1:length(bp),
                        (length(bp) + 1):(length(bp)+length(cc)))
      duration_fisher <- Sys.time() - begin
      begin <- Sys.time()
      get_relief_scores(dataset[1:i, c(bp, cc)], 1:length(bp),
                        (length(bp) + 1):(length(bp)+length(cc)))
      duration_relief <- Sys.time() - begin
      begin <- Sys.time()
      get_adc(dataset[1:i, c(bp, cc)], 1:length(bp),
              (length(bp) + 1):(length(bp)+length(cc)))
      duration_adc <- Sys.time() - begin
      begin <- Sys.time()
      get_svm_feature_weights(dataset[1:i, c(bp, cc)], 1:length(bp),
                              (length(bp) + 1):(length(bp)+length(cc)))
      duration_svm <- Sys.time() - begin
      begin <- Sys.time()
      get_best_features(dataset[1:i, c(bp, cc)], 1:length(bp),
                        (length(bp) + 1):(length(bp)+length(cc)))
      duration_fusion <-  Sys.time() - begin
      write(c(i, duration_fisher, duration_relief, duration_adc, duration_svm, duration_fusion), file='rez/all_performance.txt', append=T, ncolumns=10)
      gc()
    }
  }
}

measure_adc_performance <- function(dataset, bp, sz, cc, start, end)
{
  amount_of_features <- seq(start, end, by=500)
  for(i in amount_of_features)
  {
    for(j in 1:3)
    {
      begin <- Sys.time()
      get_adc(dataset[1:i, c(bp, cc)], 1:length(bp),
              (length(bp) + 1):(length(bp)+length(cc)))
      duration_adc <- Sys.time() - begin
      write(c(i, duration_adc), file='rez/performance_adc.txt', append=T)
    }
  }
}

measure_cgs_performance <- function(dataset, bp, sz, cc, start, end)
{
  amount_of_features <- seq(start, end, by=500)
  for(i in amount_of_features)
  {
    for(j in 1:3)
    {
      begin <- Sys.time()
      get_consensus_groups(t(dataset[1:i, c(bp, cc)]), 1:length(bp),
                           (length(bp) + 1):(length(bp)+length(cc)), 8, get_fisher_scores)
      duration_adc <- Sys.time() - begin
      write(c(i, duration_adc), file='rez/cgs_performance.txt', append=T)
    }
  }
}
