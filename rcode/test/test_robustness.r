context('get_koncheva_index(feature_ranking_i, feature_ranking_j, number_of_features)')

test_that("Jei niekas nesutampa is puses saraso, tai Konchevos indexas turi 
          buti lygus -1", {
            list1 <- c(1,2,3)
            list2 <- c(6,5,4)
            number_of_features <- 6
            expected_rez <- -1
            actual_rez <- get_koncheva_index(list1, list2, number_of_features)
            expect_that(actual_rez, equals(expected_rez))
          })

test_that("Jei viskas sutampa is puses saraso, tai Konchevos indexas turi 
          buti lygus 1", {
            list1 <- c(1,2,3)
            list2 <- c(1,2,3)
            number_of_features <- 6
            expected_rez <- 1
            actual_rez <- get_koncheva_index(list1, list2, number_of_features)
            expect_that(actual_rez, equals(expected_rez))
          })

test_that("Jei viskas sutampa is puses saraso net ir su perstatomis, tai
          Konchevos indexas turi buti lygus 1", {
            list1 <- c(1,2,3)
            list2 <- c(2,3,1)
            number_of_features <- 6
            expected_rez <- 1
            actual_rez <- get_koncheva_index(list1, list2, number_of_features)
            expect_that(actual_rez, equals(expected_rez))
          })

test_that("Jei viena is triju reiksmiu sutampa is puses saraso, tai
          Konchevos indexas turi buti lygus -1/3", {
            list1 <- c(1,2,3)
            list2 <- c(2,6,4)
            number_of_features <- 6
            expected_rez <- -1/3
            actual_rez <- get_koncheva_index(list1, list2, number_of_features)
            expect_that(actual_rez, equals(expected_rez))
          })

test_that("Jei dvi is triju reiksmiu sutampa is puses saraso, kai saraso ilgis 6, 
          tai Konchevos indexas turi buti lygus 1/3", {
            list1 <- c(1,2,3)
            list2 <- c(2,6,1)
            number_of_features <- 6
            expected_rez <- 1/3
            actual_rez <- get_koncheva_index(list1, list2, number_of_features)
            expect_that(actual_rez, equals(expected_rez))
          })

test_that("Jei trys is keturiu reiksmiu sutampa, kai saraso ilgis 6, tai
          Konchevos indexas turi buti lygus 1/4", {
            list1 <- c(1,2,3,4)
            list2 <- c(2,6,1,4)
            number_of_features <- 6
            expected_rez <- 1/4
            actual_rez <- get_koncheva_index(list1, list2, number_of_features)
            expect_that(actual_rez, equals(expected_rez))
          })

test_that("Jei viena is dvieju reiksmiu sutampa is 6 elementu dydzio saraso, tai
          Konchevos indexas turi buti lygus 1/4", {
            list1 <- c(1,2)
            list2 <- c(2,6)
            number_of_features <- 6
            expected_rez <- 1/4
            actual_rez <- get_koncheva_index(list1, list2, number_of_features)
            expect_that(actual_rez, equals(expected_rez))
          })

test_that("Jei dvi is dvieju reiksmiu sutampa is 6 elementu dydzio saraso, tai
          Konchevos indexas turi buti lygus 1", {
            list1 <- c(1,2)
            list2 <- c(2,1)
            number_of_features <- 6
            expected_rez <- 1
            actual_rez <- get_koncheva_index(list1, list2, number_of_features)
            expect_that(actual_rez, equals(expected_rez))
          })

test_that("Jei ne viena is dvieju reiksmiu sutampa is 6 elementu dydzio saraso, tai
          Konchevos indexas turi buti lygus -1/2", {
            list1 <- c(1,2)
            list2 <- c(3,4)
            number_of_features <- 6
            expected_rez <- -1/2
            actual_rez <- get_koncheva_index(list1, list2, number_of_features)
            expect_that(actual_rez, equals(expected_rez))
          })

context("get_overal_stability(ranking, percentage)")

test_that("Overal stability is -2/6, when taking half of features", {
  ranks1 <- c(1,2,3,4,5,6)
  ranks2 <- c(1,2,4,3,5,6)
  ranks3 <- c(6,5,4,3,2,1)
  rankings <- rbind(ranks1, ranks2, ranks3)
  expected_stability <- -2/6
  actual_stability <- get_overal_stability(rankings, 0.5)
  expect_that(actual_stability, equals(expected_stability))
})

test_that("Overal stability is -2/6, when taking half of features", {
  ranks1 <- c(1,2,3,4,5,6)
  ranks2 <- c(1,2,4,3,5,6)
  ranks3 <- c(6,5,4,3,2,1)
  rankings <- rbind(ranks1, ranks2, ranks3)
  expected_stability <- -2/6
  actual_stability <- get_overal_stability(rankings, 0.5)
  expect_that(actual_stability, equals(expected_stability))
})