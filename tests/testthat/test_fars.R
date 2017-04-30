context("create filename")

test_that("make_filename creates the right file names", {
  expect_equal(make_filename(2015), "data/accident_2015.csv.bz2")
  expect_equal(make_filename(2013), "data/accident_2013.csv.bz2")
})
