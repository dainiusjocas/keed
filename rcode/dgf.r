source("utils.r")


# source 
# http://www.cs.berkeley.edu/~jordan/courses/281B-spring04/lectures/lec5.ps
# and multivariate kernel in wikipedia
gaussian_kernel <- function(x, h) {
  k <- 1/sqrt( 2 * pi * h ) * exp( - (t(x) %*% x) / (2*h))
  return (as.numeric(k))
}


mean_shift <- function( data, y, h ) {	
  n <- ncol(data)	
  for ( iteration in 1:1000) {
    nom <- 0
    den <- 0	
    for (i in 1:n) {
      K <- gaussian_kernel( (y - data[,i]), h  )
      nom <- nom + data[,i] * K
      den <- den + K
    }
    yy <- nom / den
    if ( dist(rbind(yy, y)) < 0.00000001 )
      break
    else
      y <- yy
  }
  return (yy)
}


mergePeaks <- function( peaks, newpeak, h) {
  peakFound <- F
  if ( length(peaks ) > 0 ) {
    for ( i in 1:length(peaks) ) {
      peak <- peaks[[i]]$p
      count <- peaks[[i]]$c
      d <- dist( rbind( peak, newpeak) )
      if ( d <= h) {
        peak <- (count * peak + newpeak)/(count + 1)
        peaks[[i]]$p <- peak
        peaks[[i]]$c <- count + 1
        peakFound <- T
        break
      }
    }		
  }
  if ( !peakFound) {
    l <- list( p=newpeak, c=1)
    peaks[[ length(peaks) + 1 ]] <- l
  }
  return ( peaks )
}


constructGroups <- function( data, peaks, h) {
  groups <- list()
  n <- ncol(data)
  assert( n > 0)
  assert( length(peaks) > 0 )
  
  for ( i in 1:length(peaks)) {
    groups[[i]] <- list( peak=peaks[[i]]$p, features=numeric())
  }
  
  for ( f in 1:n) { 
    feature <- data[,f]
    for ( g in 1:length(groups) ) {
      peak <- groups[[g]]$peak
      if ( dist( rbind(peak, feature)) <= h ) {
        l <- length(groups[[g]]$features)
        groups[[g]]$features[l + 1] <- f
      }
    }
  }
  return(groups)
}


DGF <- function( data, h) {
  n <- ncol(data)
  peaks <- list()
  for ( i in 1:n) {
    newpeak <- mean_shift( data, data[, i], h)
    peaks <- mergePeaks( peaks, newpeak, h)
#     catln("Feature: ", i, "Peaks:", length(peaks))
    gc()
  }
  groups <- constructGroups( data, peaks, h)
  return (groups)
}

