library(testthat)
library(framirez)


test_that("Function 'make_filename' returns a name for the file with the given year", {
  make_filename(2009)
})

