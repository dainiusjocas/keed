context("Testuojam RELIEF metoda")

test_that("vektoriu [0, 0] ir [1, 1] atstumu matrica turi buti [[0, sqrt2],
          [sqrt(2), 0]]", {
  t_matrix <- matrix(c(0, 0, 1, 1), nrow = 2, ncol = 2)
  distance_matrix <- get_distance_matrix(t_matrix)
  expect_that(distance_matrix[1, 1], equals(0))
	expect_that(distance_matrix[1, 2], equals(sqrt(2)))
  expect_that(distance_matrix[2, 1], equals(sqrt(2)))
  expect_that(distance_matrix[2, 2], equals(0))
})

test_that("Svorio komponente turi buti 0.1, kai einamojo elemento savybes
          reiksme yra 2, artimiausio kaimyno 3, savybes max=11, min=1", {
  expect_that(relief_difference(2, 3, 11, 1), equals(0.1))            
})
