context("Testuojam funkcija kuri kreivai skaiciuoja vidurki")

test_that("Skaiciu {2,4} vidurkis turi buti lygus 3", {
	expect_that(vidurkis(c(2,4)), equals(3))
	expect_that(vidurkis(c(4,2)), equals(3))
})
	
test_that("Skaiciu {1,3} vidurkis turi buti lygus 2", {
	expect_that(vidurkis(c(1,3)), equals(2))
	expect_that(vidurkis(c(3,1)), equals(2))
})