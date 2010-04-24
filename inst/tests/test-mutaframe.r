library(testthat)

context("Basic construction and mutability")

test_that("can construct empty pframe", {
  empty <- pframe()
  
  expect_that(nrow(empty), equals(0))
  expect_that(ncol(empty), equals(0))
})
