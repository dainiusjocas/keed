################################################
# Library for F-score feature ranking
################################################ 


# Computes column-wise variance
colVars <- function( x, na.rm = FALSE, dims=1, unbiased=TRUE, 
					SumSquares=FALSE, twopass=FALSE) 
{
	if (SumSquares) return ( colSums( x^2, na.rm, dims) )
	
	N <- colSums( !is.na( x ), FALSE, dims )
	Nm1 <- 	if (unbiased) N-1 else N
	( colSums(x^2, na.rm, dims) - colSums( x, na.rm, dims)^2/N)/Nm1
}

#Computes column-wise F-score
colFisher <- function(data, wt, mt) 
{
	means <- colMeans(data)
	nom <- (colMeans( data[wt,]) - means)^2 + (colMeans( data[mt,]) - means)^2
	den <- colVars( data[wt,] ) + colVars( data[mt, ] )
	nom/den
}


# Uses F-score to select the most prominent loci
selectTFeatures <- function(data, pos, neg, threshold) 
{
	fscore <- selectKFeatures( data, pos, neg, ncol(data))
	fscore$ix[ which( fscore$x >= threshold ) ]
}


#Returns sorted list of fisher scores and features
selectKFeatures <- function(data, wt, mt, k)
{
	ranks <- colFisher( data, wt, mt )
	loci <- sort( ranks, decreasing=TRUE, index.return=TRUE)
	return (list(x=loci$x[1:k], ix=loci$ix[1:k]))
}

# ENTRY POINT
# This methods gets the fisher scores of every feature of the dataset.
# input: dataset: rows - features, columns - tuples
# input: pos - indexes of normal patients in dataset
# input: neg - indexes of patiens with tumor in dataset
get_fisher_scores <- function(dataset, pos, neg)
{
  fisher_scores <- colFisher(t(dataset), pos, neg)
  return(fisher_scores)
}
