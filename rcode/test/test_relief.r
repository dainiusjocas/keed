context("Testuojam RELIEF savybiu atrinkimo metoda")
context("get_min_max_diff(dataset)")

test_that("Skirtumai tarp minimalios ir maksimalios savybes reiksmiu turi buti <3, 6>, 
 kai savybiu vektoriai yra s1=<2,3,4,5> ir s2=<3,4,5,9>", {
   s1 <- c(2,3,4,5)
   s2 <- c(3,4,5,9)
   dataset <- rbind(s1, s2)
   expected_min_max_diff <- c(3,6)
   expect_that(get_min_max_diff(dataset), equals(expected_min_max_diff))
 })

test_that("Skirtumai tarp minimalios ir maksimalios savybes reiksmiu turi buti <7, 12>, 
 kai savybiu vektoriai yra s1=<-2,3,4,5> ir s2=<-3,4,5,9>", {
   s1 <- c(-2,3,4,5)
   s2 <- c(-3,4,5,9)
   dataset <- rbind(s1, s2)
   expected_min_max_diff <- c(7, 12)
   expect_that(get_min_max_diff(dataset), equals(expected_min_max_diff))
 })

test_that("Skirtumai tarp minimalios ir maksimalios savybes reiksmiu turi buti <7, 6>, 
 kai savybiu vektoriai yra s1=<-2,3,4,5> ir s2=<-3,4,5,9>", {
   s1 <- c(-2,-3,-4,-9)
   s2 <- c(-3,-4,-5,-9)
   dataset <- rbind(s1, s2)
   expected_min_max_diff <- c(7,6)
   expect_that(get_min_max_diff(dataset), equals(expected_min_max_diff))
 })

test_that("Skirtumai tarp minimalios ir maksimalios savybes reiksmiu turi buti <0, 0>, 
 kai savybiu vektoriai yra s1=<0,0,0,0> ir s2=<3,3,3,3>", {
   s1 <- c(0,0,0,0)
   s2 <- c(3,3,3,3)
   dataset <- rbind(s1, s2)
   expected_min_max_diff <- c(0,0)
   expect_that(get_min_max_diff(dataset), equals(expected_min_max_diff))
 })

context("make_distance_matrix(dataset)")

test_that("Atstumu matrica turi buti [(Inf, 5), (5, Inf)], kai duomenys yra 
          [(0, 3), (4, 0)]", {
   dataset <- rbind(c(0, 3), c(4, 0))
   distance_matrix <- make_distance_matrix(dataset)
   expected_distance_matrix <- rbind(c(Inf, 5), c(5, Inf))
   expect_that(distance_matrix, equals(expected_distance_matrix))
})

test_that("Atstumu matrica turi buti [(Inf, 2), (2, Inf)], kai duomenys yra 
          [(1,0), (1,2), (2,1), (1,2)]", {
  dataset <- rbind(c(1,0), c(1,2), c(2,1), c(1,2))
  distance_matrix <- make_distance_matrix(dataset)
  expected_distance_matrix <- rbind(c(Inf, 2), c(2, Inf))
  expect_that(distance_matrix, equals(expected_distance_matrix))
})

test_that("Atstumu matrica turi buti [(Inf, 2), (2, Inf)], kai duomenys yra 
          [(-1,0), (-1,-2), (-2,-1), (-1,-2)]", {
            dataset <- rbind(c(-1,0), c(-1,-2), c(-2,-1), c(-1,-2))
            distance_matrix <- make_distance_matrix(dataset)
            expected_distance_matrix <- rbind(c(Inf, 2), c(2, Inf))
            expect_that(distance_matrix, equals(expected_distance_matrix))
          })

test_that("Atstumu matrica turi buti 
[(Inf, 2, sqrt(7)), (2, Inf, 3), (sqrt(7), 3, Inf)], 
kai duomenys yra [(-1,0,0), (-1,-2,0), (-2,-1,0), (-1,-2,0)]", {
            dataset <- rbind(c(-1,0,0), c(-1,-2,0), c(-2,-1,0), c(-1,-2,0))
            distance_matrix <- make_distance_matrix(dataset)
            expected_distance_matrix <- rbind(c(Inf, 2, sqrt(7)),
                                              c(2, Inf, 3),
                                              c(sqrt(7), 3, Inf))
            expect_that(distance_matrix, equals(expected_distance_matrix))
          })

context("get_index_of_nearest_hit(distance_matrix, instance_index, pos, neg)")

test_that("Artimiausio kaimyno is tos pacios klases pirmajam objektui indeksas turi buti 2, 
          kai duomenys yra [(-1,0,0), (-1,-2,0), (-2,-1,0), (-1,-2,0)],
          kai pirmas ir antras objektai yra is tos pacios klases", {
            dataset <- rbind(c(-1,0,0), c(-1,-2,0), c(-2,-1,0), c(-1,-2,0))
            pos <- c(1,2)
            neg <- c(3)
            distance_matrix <- make_distance_matrix(dataset)
            index_of_nearest_hit <- get_index_of_nearest_hit(distance_matrix, 1, pos, neg)
            expected_index_of_nearest_hit <- 2
            expect_that(index_of_nearest_hit, equals(expected_index_of_nearest_hit))
          })

test_that("Artimiausio kaimyno is tos pacios klases antrajam objektui indeksas turi buti 1, 
          kai duomenys yra [(-1,0,0), (-1,-2,0), (-2,-1,0), (-1,-2,0)],
          kai pirmas ir antras objektai yra is tos pacios klases", {
            dataset <- rbind(c(-1,0,0), c(-1,-2,0), c(-2,-1,0), c(-1,-2,0))
            pos <- c(1,2)
            neg <- c(3)
            distance_matrix <- make_distance_matrix(dataset)
            index_of_nearest_hit <- get_index_of_nearest_hit(distance_matrix, 2, pos, neg)
            expected_index_of_nearest_hit <- 1
            expect_that(index_of_nearest_hit, equals(expected_index_of_nearest_hit))
          })

test_that("Artimiausio kaimyno is tos pacios klases treciajam objektui indeksas turi buti 4, 
          kai duomenys yra [(-1,0,0,1), (-1,-2,0,1), (-2,-1,0,1), (-1,-2,0,1)],
          kai pirmas ir antras objektai yra is tos pacios klases", {
            dataset <- rbind(c(1,0,0,1), c(1,2,0,1), c(2,1,0,1), c(1,2,0,1))
            pos <- c(1,2)
            neg <- c(3,4)
            distance_matrix <- make_distance_matrix(dataset)
            index_of_nearest_hit <- get_index_of_nearest_hit(distance_matrix, 3, pos, neg)
            expected_index_of_nearest_hit <- 4
            expect_that(index_of_nearest_hit, equals(expected_index_of_nearest_hit))
          })

test_that("Artimiausio kaimyno is tos pacios klases ketvirtajam objektui indeksas turi buti 3, 
          kai duomenys yra [(-1,0,0,1), (-1,-2,0,1), (-2,-1,0,1), (-1,-2,0,1)],
          kai pirmas ir antras objektai yra is tos pacios klases", {
            dataset <- rbind(c(1,0,0,1), c(1,2,0,1), c(2,1,0,1), c(1,2,0,1))
            pos <- c(1,2)
            neg <- c(3,4)
            distance_matrix <- make_distance_matrix(dataset)
            index_of_nearest_hit <- get_index_of_nearest_hit(distance_matrix, 4, pos, neg)
            expected_index_of_nearest_hit <- 3
            expect_that(index_of_nearest_hit, equals(expected_index_of_nearest_hit))
          })

context('get_index_of_nearest_miss(distance_matrix, instance_index, pos, neg)')

test_that("Artimiausio kaimyno is kitos klases pirmajam objektui indeksas turi buti 3, 
          kai duomenys yra [(-1,0,0), (-1,-2,0), (-2,-1,0), (-1,-2,0)],
          kai pirmas ir antras objektai yra is tos pacios klases", {
            dataset <- rbind(c(-1,0,0), c(-1,-2,0), c(-2,-1,0), c(-1,-2,0))
            pos <- c(1,2)
            neg <- c(3)
            distance_matrix <- make_distance_matrix(dataset)
            index_of_nearest_miss <- get_index_of_nearest_miss(distance_matrix, 1, pos, neg)
            expected_index_of_nearest_miss <- 3
            expect_that(index_of_nearest_miss, equals(expected_index_of_nearest_miss))
          })

test_that("Artimiausio kaimyno is kitos klases antrajam objektui indeksas turi buti 3, 
          kai duomenys yra [(-1,0,0), (-1,-2,0), (-2,-1,0), (-1,-2,0)],
          kai pirmas ir antras objektai yra is tos pacios klases", {
            dataset <- rbind(c(-1,0,0), c(-1,-2,0), c(-2,-1,0), c(-1,-2,0))
            pos <- c(1,2)
            neg <- c(3)
            distance_matrix <- make_distance_matrix(dataset)
            index_of_nearest_miss <- get_index_of_nearest_miss(distance_matrix, 2, pos, neg)
            expected_index_of_nearest_miss <- 3
            expect_that(index_of_nearest_miss, equals(expected_index_of_nearest_miss))
          })

test_that("Artimiausio kaimyno is kitos klases treciajam objektui indeksas turi buti 1, 
          kai duomenys yra [(-1,0,0), (-1,-2,0), (-2,-1,0), (-1,-2,0)],
          kai pirmas ir antras objektai yra is tos pacios klases", {
            dataset <- rbind(c(-1,0,0), c(-1,-2,0), c(-2,-1,0), c(-1,-2,0))
            pos <- c(1,2)
            neg <- c(3)
            distance_matrix <- make_distance_matrix(dataset)
            index_of_nearest_miss <- get_index_of_nearest_miss(distance_matrix, 3, pos, neg)
            expected_index_of_nearest_miss <- 1
            expect_that(index_of_nearest_miss, equals(expected_index_of_nearest_miss))
          })

context("get_relief_scores(dataset, pos, neg)")

test_that("Kokybinio skirtumo dydis turi buti 1, 
          kai duomenys yra [(-1,0,0,1), (-1,-2,0,1), (-2,-1,0,1), (-1,-2,0,1)],
          kai pirmas ir trecias objektai yra is tos pacios klases", {
            dataset <- rbind(c(1,0,0,1), c(1,2,0,1), c(2,1,0,1), c(1,2,0,1))
            pos <- c(1,3)
            neg <- c(2,4)
            distance_matrix <- make_distance_matrix(dataset)
            instance_index <- 1
            feature_index <- 1
            nearest_hit <- 3
            nearest_miss <- 4
            d <- get_diff(dataset, instance_index, feature_index, nearest_hit, nearest_miss)
            expect_that(d, equals(1))
          })