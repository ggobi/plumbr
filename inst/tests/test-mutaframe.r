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

test_that("shorter columns recycled", {
  a <- pframe(a = 1:10, b = 1:5)
  expect_that(nrow(a), equals(10))

  b <- pframe(a = 1:10, b = 1:5, c = 1:2)
  expect_that(nrow(b), equals(10))
})

test_that("failure to recycle raises error", {
  expect_that(pframe(a = 1:10, b = 1:9), throws_error("different row counts"))
})

test_that("default names behave like data.frame", {
  a <- pframe(1, 2)
  expect_that(names(a), equals(c("X1", "X2")))
  
  b <- pframe(1, X1 = 2)
  expect_that(names(b), equals(c("X1", "X1.1")))

  c <- pframe(`a b` = 1)
  expect_that(names(c), equals("a.b"))
  
})
