context("make_filename")

test_that("fun_list is merged with new args", {
  expect_that(make_filename(2015), equals("accident_2015.csv.bz2"))
})
