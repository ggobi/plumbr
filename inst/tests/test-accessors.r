library(testthat)

context("Accessors")

test_that("getting and setting a single value works", {
  a <- mutaframe(a = 1:10)
  expect_that(a$a[1], equals(1))
  expect_that(a[[1]][1], equals(1))
  expect_that(a[["a"]][1], equals(1))
  expect_that(a[1][1 ,1], equals(1))
  expect_that(a["a"][1 ,1], equals(1))
  expect_that(a[1,1], equals(1))
  
  a[1, 1] <- 2
  expect_that(a[1,1], equals(2))
})
