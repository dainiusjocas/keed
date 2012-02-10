context("Testuojam ranking-based fusion metoda")

test_that("vektoriui <3, 5, 4> reikia priskirti reiksmes <1, 3 ,2>", {
  values <- data.frame(values=c(3, 5, 4))
  expected_rez <- data.frame(values=c(1, 3, 2))
  ranked_features <- rank_features(values)
  expect_that(ranked_features[1], equals(expected_rez[1]))
})

test_that("vektoriui <2, 0, 4> reikia priskirti reiksmes <2, 1 , 3>", {
  values <- data.frame(values=c(2, 0, 4))
  expected_rez <- data.frame(values=c(2, 1, 3))
  ranked_features <- rank_features(values)
  expect_that(ranked_features[1], equals(expected_rez[1]))
})


test_that("vektoriui <4, 0, 2> reikia priskirti reiksmes <3, 1 ,2>", {
  values <- data.frame(values=c(4, 0, 2))
  expected_rez <- data.frame(values=c(3, 1, 2))
  ranked_features <- rank_features(values)
  expect_that(ranked_features[1], equals(expected_rez[1]))
})

test_that("data.frame <<4, 0, 2>, <4, 1, 0>> rankai turi buti 
         <<3, 1, 2>, <3, 2, 1>>", {
            initial_data <- data.frame(one=c(4, 0, 2), two=c(4, 1, 0))
            expected_rez <- data.frame(one=c(3, 1, 2), two=c(3, 2, 1))
            ranking_based_fusion <- rank_features(initial_data)
            expect_that(ranking_based_fusion[1], equals(expected_rez[1]))
            expect_that(ranking_based_fusion[2], equals(expected_rez[2]))
         })

test_that("data.frame <<3, 1, 2>, <3, 1, 2>> sumine tasku suma turi buti
          <6, 2, 4>", {
            initial_data <- data.frame(one=c(3, 1, 2), two=c(3, 1, 2))
            expected_rez <- c(6, 2, 4)
            ranking_based_fusion <- get_fusion_of_ranks(initial_data)
            expect_that(ranking_based_fusion, equals(expected_rez))
})

test_that("data.frame <<3, 1, 2>, <3, 2, 1>> sumine tasku suma turi buti
          <6, 3, 3>", {
            initial_data <- data.frame(one=c(3, 1, 2), two=c(3, 2, 1))
            expected_rez <- c(6, 3, 3)
            ranking_based_fusion <- get_fusion_of_ranks(initial_data)
            expect_that(ranking_based_fusion, equals(expected_rez))
          })

test_that("data.frame <<4, 0, 2>, <4, 1, 0>> sumine tasku suma turi buti
          <6, 3, 3>", {
            initial_data <- data.frame(one=c(4, 0, 2), two=c(4, 1, 0))
            expected_rez <- c(6, 3, 3)
            ranking_based_fusion <- get_ranking_based_fusion(initial_data)
            expect_that(ranking_based_fusion, equals(expected_rez))
})

