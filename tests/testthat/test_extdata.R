test_that("if CSV fies are there", {
  testthat::expect_equal(list.files(system.file("extdata", package = "Week4BuildingRPackages")),
                         c("accident_2013.csv.bz2",
                           "accident_2014.csv.bz2",
                           "accident_2015.csv.bz2"))
})

