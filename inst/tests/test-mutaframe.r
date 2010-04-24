library(testthat)

context("Basic construction and mutability")

test_that("can construct empty pframe", {
  empty <- pframe()
  
  expect_that(nrow(empty), equals(0))
  expect_that(ncol(empty), equals(0))
})

test_that("can construct single column pframes", {
  a <- pframe(1:10)
  expect_that(ncol(a), equals(1))
  
  b <- pframe(a = 1:10)
  expect_that(ncol(a), equals(1))
  
})