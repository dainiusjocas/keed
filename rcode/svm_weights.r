library(e1071)

# This method computes the ranking of features according to svm-rfe algorithm
# output: featureRankedList - vector which first item has the index of the
#   feature which is the most relevant to perform the classificatiom and the
#   last item is the least relevant 
svmrfeFeatureRanking = function(x,y){
  n = ncol(x)
  survivingFeaturesIndexes = seq(1:n)
  featureRankedList = vector(length=n)
  rankedFeatureIndex = n
  while(length(survivingFeaturesIndexes)>0){
    #train the support vector machine
    svmModel = svm(x[, survivingFeaturesIndexes], y, cost = 10, cachesize=500,
                   scale=F, type="C-classification", kernel="linear" )
    
    #compute the weight vector
    w = t(svmModel$coefs)%*%svmModel$SV
    
    #compute ranking criteria
    rankingCriteria = w * w
    
    #rank the features
    ranking = sort(rankingCriteria, index.return = TRUE)$ix
    
    #update feature ranked list
    featureRankedList[rankedFeatureIndex] = survivingFeaturesIndexes[ranking[1]]
    rankedFeatureIndex = rankedFeatureIndex - 1
    
    #eliminate the feature with smallest ranking criterion
    (survivingFeaturesIndexes = survivingFeaturesIndexes[-ranking[1]])
  }
  return (featureRankedList)
}

svmrfeFeatureRankingForMulticlass = function(x,y){
  n = ncol(x)
  survivingFeaturesIndexes = seq(1:n)
  featureRankedList = vector(length=n)
  rankedFeatureIndex = n
  while(length(survivingFeaturesIndexes)>0){
    #train the support vector machine
    svmModel = svm(x[, survivingFeaturesIndexes], y, cost = 10, cachesize=500,
                   scale=F, type="C-classification", kernel="linear" )
    
    #compute the weight vector
    multiclassWeights = svm.weights(svmModel)
    
    #compute ranking criteria
    multiclassWeights = multiclassWeights * multiclassWeights
    rankingCriteria = 0
    for(i in 1:ncol(multiclassWeights))rankingCriteria[i] =
      mean(multiclassWeights[,i])
    
    #rank the features
    (ranking = sort(rankingCriteria, index.return = TRUE)$ix)
    
    #update feature ranked list
    (featureRankedList[rankedFeatureIndex] = survivingFeaturesIndexes[ranking[1]])
    rankedFeatureIndex = rankedFeatureIndex - 1
    
    #eliminate the feature with smallest ranking criterion
    (survivingFeaturesIndexes = survivingFeaturesIndexes[-ranking[1]])
    cat(length(survivingFeaturesIndexes),"\n")
  }
}


svm.weights<-function(model){
  w=0
  if(model$nclasses==2){
    w=t(model$coefs)%*%model$SV
  }else{ #when we deal with OVO svm classification
    ## compute start-index
      start <- c(1, cumsum(model$nSV)+1)
      start <- start[-length(start)]
      calcw <- function (i,j) {
      ## ranges for class i and j:
      ri <- start[i] : (start[i] + model$nSV[i] - 1)
      rj <- start[j] : (start[j] + model$nSV[j] - 1)
      ## coefs for (i,j):
      coef1 <- model$coefs[ri, j-1]
      coef2 <- model$coefs[rj, i]
      ## return w values:
      w=t(coef1)%*%model$SV[ri,]+t(coef2)%*%model$SV[rj,]
      return(w)
    }
    W=NULL
    for (i in 1 : (model$nclasses - 1)){
      for (j in (i + 1) : model$nclasses){
        wi=calcw(i,j)
        W=rbind(W,wi)
      }
    }
    w=W
  }
  return(w)
}

# This method computes the weights of features of dataset. First, we build
#   a model of svm classifier, then we get weight values of every feature 
#   from that model. 
# NOTE: Only one iteration of building the svm model is done.
# input: dataset: rows - features, columns - tuples
# input: pos - indexes of normal patients in the dataset
# input: neg - indexes of patients with tumor in the dataset
# output: svm_weights - a vector of scores of features
get_svm_feature_weights <- function(dataset, pos, neg)
{
  y <- c()
  y[pos] <- -1
  y[neg] <- 1
  # dataset should be transposed in order to compute weights
  data <- t(dataset) 
  svmModel = svm(data, y, cost = 10, cachesize=500,
                 scale=F, type="C-classification", kernel="linear" )
  # For two-class situation
  svm_weights <- abs(t(svmModel$coefs)%*%svmModel$SV)
  # For multiclass situation
  # svm_weights <- abs(svm.weights(svmModel))
  return(as.vector(svm_weights))
}

# This method does input data transformation and then calls method 
#   svmrfeFeatureRanking to get the ranks of the feature.
get_svm_feature_ranks <- function(dataset, pos, neg)
{
  y <- c()
  y[pos] <- -1
  y[neg] <- 1
  # dataset should be transposed in order to compute weights
  data <- t(dataset)
  svm_feature_ranks <- svmrfeFeatureRanking(data, y)
  return(svm_feature_ranks)
}

