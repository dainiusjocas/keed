context('Ziuresim ar surenka visas prireitinguotas dimensijas')

test_that("ar surenka visas unikalias dimensijas is keliu sarasu",{
  rank1 <- c(1, 2, 3, 4, 5, 6)
  rank2 <- c(2, 3, 4, 5, 1, 6)
  ranks <- rbind(rank1, rank2)
  expected_rez <- c(1, 2, 3, 4)
  actual_rez <- get_unique_features(ranks, 0.5)
  expect_that(actual_rez, equals(expected_rez))
})


test_that("ar surenka visas unikalias dimensijas is keliu sarasu",{
  rank1 <- c(1, 2, 3, 4, 5, 6)
  rank2 <- c(2, 3, 4, 5, 1, 6)
  rank3 <- c(6, 1, 2, 3, 4, 5)
  ranks <- rbind(rank1, rank2, rank3)
  expected_rez <- c(1, 2, 3, 4, 6)
  actual_rez <- get_unique_features(ranks, 0.5)
  expect_that(actual_rez, equals(expected_rez))
})

test_that("ar surenka visas unikalias dimensijas is keliu sarasu",{
  rank1 <- c(1, 2, 3, 4, 5, 6)
  rank2 <- c(2, 3, 4, 5, 1, 6)
  rank3 <- c(1, 4, 2, 3, 6, 5)
  ranks <- rbind(rank1, rank2, rank3)
  expected_rez <- c(1, 2, 3, 4)
  actual_rez <- get_unique_features(ranks, 0.5)
  expect_that(actual_rez, equals(expected_rez))
})

context('Ieskosim sankirtos tokiu iskreiptu budu')

test_that('ar visuose rankinguose pasitaiko ta dimensija',{
  rank1 <- c(1, 2, 3, 4, 5, 6)
  rank2 <- c(2, 3, 4, 5, 1, 6)
  ranks <- rbind(rank1, rank2)
  expected_rez <- T
  actual_rez <- is_feature_stable(2, ranks, 0.5)
  expect_that(actual_rez, equals(expected_rez))
})

test_that('ar visuose rankinguose pasitaiko ta dimensija',{
  rank1 <- c(1, 2, 3, 4, 5, 6)
  rank2 <- c(2, 3, 4, 5, 1, 6)
  ranks <- rbind(rank1, rank2)
  expected_rez <- F
  actual_rez <- is_feature_stable(1, ranks, 0.5)
  expect_that(actual_rez, equals(expected_rez))
})

test_that("ar surenka visas unikalias dimensijas is keliu sarasu",{
  rank1 <- c(1, 2, 3, 4, 5, 6)
  rank2 <- c(2, 3, 4, 5, 1, 6)
  rank3 <- c(1, 4, 2, 3, 6, 5)
  ranks <- rbind(rank1, rank2, rank3)
  expected_rez <- T
  actual_rez <- is_feature_stable(2, ranks, 0.5)
  expect_that(actual_rez, equals(expected_rez))
})

test_that("ar surenka visas unikalias dimensijas is keliu sarasu",{
  rank1 <- c(1, 2, 3, 4, 5, 6)
  rank2 <- c(2, 3, 4, 5, 1, 6)
  rank3 <- c(1, 4, 2, 3, 6, 5)
  ranks <- rbind(rank1, rank2, rank3)
  expected_rez <- F
  actual_rez <- is_feature_stable(1, ranks, 0.5)
  expect_that(actual_rez, equals(expected_rez))
})

context("Susirinksim visas stabilias dimensijas")

test_that('Turi buti viena stabili dimensija',{
  rank1 <- c(1, 2, 3, 4, 5, 6)
  rank2 <- c(2, 3, 4, 5, 1, 6)
  rank3 <- c(1, 4, 2, 3, 6, 5)
  ranks <- rbind(rank1, rank2, rank3)
  expected_rez <- c(2)
  actual_rez <- get_all_stable_features(ranks, percentage=0.5, threshold=1)
  expect_that(actual_rez, equals(expected_rez))
})

test_that('Turi buti dvi stabilios dimensijos',{
  rank1 <- c(1, 2, 3, 4, 5, 6)
  rank2 <- c(2, 1, 4, 5, 3, 6)
  rank3 <- c(1, 4, 2, 3, 6, 5)
  ranks <- rbind(rank1, rank2, rank3)
  expected_rez <- c(1, 2)
  actual_rez <- get_all_stable_features(ranks, percentage=0.5, threshold=1)
  expect_that(actual_rez, equals(expected_rez))
})

context('Ar tikrai stabiliai atrinkineja')

