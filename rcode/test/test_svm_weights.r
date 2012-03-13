context("Testuosiu, kaip veikia svm-rfe svoriu skaiciavimo mechanizmas")

test_that("x dimensija turi buti reiksmingesne uz y dimensija", {
  values <- data.frame(x=c(1, 2, 3, 7, 8, 8), 
                       y=c(2, 1, 2, 1, 2, 1))
  tvalues <- t(values) # rows - savybes; columns - objektai
  pos <- c(1, 2, 3) # positive klase
  neg <- c(4, 5, 6) # negative klase
  svm_weights <- get_svm_feature_weights(tvalues, pos, neg)
  expect_that(svm_weights[1] > svm_weights[2], is_true())
})


test_that("x dimensija turi tureti aukstesni rank'a uz y dimensija", {
  values <- data.frame(x=c(1, 2, 3, 7, 8, 8), 
                       y=c(2, 1, 2, 1, 2, 1))
  tvalues <- t(values) # rows - savybes; columns - objektai
  pos <- c(1, 2, 3) # positive klase
  neg <- c(4, 5, 6) # negative klase
  svm_ranks <- get_svm_feature_ranks(tvalues, pos, neg)
  expect_that(svm_ranks[2] > svm_ranks[1], is_true())
})

test_that("x dimensija turi tureti zemesni rank'a uz y dimensija", {
  values <- matrix(c(1, 2, 2, 1, 3, 2, 7, 1, 8, 2, 8, 1), nrow=2)
  #tvalues <- t(values) # rows - savybes; columns - objektai
  pos <- c(1, 2, 3) # positive klase
  neg <- c(4, 5, 6) # negative klase
  nmatrix <- (values)
  nmatrix[1, ] <- values[2, ]
  nmatrix[2, ] <- values[1, ]
  svm_ranks <- get_svm_feature_ranks(nmatrix, pos, neg)
  expect_that(svm_ranks[1] > svm_ranks[2], is_true())
})
