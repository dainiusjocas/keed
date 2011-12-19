source("dgf.R")

library(testthat)

test_that( "Gaussian kernel is correct", {
	expect_equivalent( gaussian_kernel(-1, 1), gaussian_kernel(1, 1))
	expect_equivalent( gaussian_kernel(0, 1), 0.398942280)
})

test_that("Mean shift can handle 1D data", {
	data <- t(as.matrix(rnorm(1000, mean=1)))
	m <- mean_shift( data, data[,1] , 1); m
	expect_that( abs( m - 1) <= 0.1  , is_true() )
	rm(data)
})

test_that("Mean shift can handle 2D data", {
	library(MASS)
	data <- t( mvrnorm( 1000, c(1,1),  matrix(c(1,1,1,1),2,2) ) )
	m <- mean_shift( data, data[,1] , 1)
 	expect_that( dist( rbind(m, c(1, 1)) ) <= 0.1, is_true() )
})

test_that("mergePeaks is correct", {
	p1 <- c(1, 1)
	p2 <- c(1, 2)
	p3 <- c(2, 3)
	
	peaks <- list()
	expect_that( length(peaks), equals(0))
	peaks <- mergePeaks( peaks, p1, 1)
	expect_that( length(peaks), equals( 1))
	peaks <- mergePeaks( peaks, p1, 1)
	expect_that( length(peaks), equals(1))
	expect_that( peaks[[1]]$c, equals(2) )
	expect_that( peaks[[1]]$p, equals(p1))
	
	peaks <- mergePeaks( peaks, p2, 1)
	expect_that( length(peaks), equals(1))
	expect_that( peaks[[1]]$c, equals(3) )
	expect_that( peaks[[1]]$p, is_equivalent_to(c(1, 1.3333333333)))
	
	peaks <- mergePeaks( peaks, p3, 1)
	expect_that( length(peaks), equals(2))
	expect_that( peaks[[2]]$c, equals(1) )
	expect_that( peaks[[2]]$p, is_equivalent_to(c(2, 3)))	
	
	peaks <- list()
	peaks <- mergePeaks( peaks, p1, 1)
	peaks <- mergePeaks( peaks, p2, 1)
	expect_that( peaks[[1]]$c, equals(2))
	expect_that( peaks[[1]]$p, is_equivalent_to(c(1, 1.5)))
})


test_that("constructGroups is correct", {
	peaks <- list()
	groups <- list()
	
	p1 <- c(1, 1)
	p2 <- c(1, 2)
	p3 <- c(2, 3)
	p4 <- c(3, 3)

	peaks <- mergePeaks( peaks, p1, 1)
	peaks <- mergePeaks( peaks, p2, 1)
	peaks <- mergePeaks( peaks, p3, 1)

	data <- cbind( p1, p2, p3, p4)
	groups <- constructGroups( data, peaks, 1)
	expect_that( length(groups), equals(2))
	expect_that( groups[[1]]$peak, is_equivalent_to( c(1, 1.5)))
	expect_that( groups[[2]]$peak, is_equivalent_to( c(2, 3)))	
	expect_that( groups[[1]]$features, is_equivalent_to( c(1, 2)))
	expect_that( groups[[2]]$features, is_equivalent_to( c(3, 4)))
})

test_that("constructGroups assigns all features to peaks", {
	library(datasets)
	data(faithful)	
	faithful <- t(faithful)
	faithful <- faithful[, sample( 1:ncol(faithful), 20 ) ]
	groups <- DGF( faithful, .1)
	features <- lapply( 1:length(groups), function(i) groups[[i]]$features )
	set <- setdiff( 1:20, unlist(features))
	expect_that( length(set), equals(0))
})

