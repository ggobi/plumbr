library(testthat)

context("Events")

test_that("can add listeners", {
  a <- mutaframe(a = 1:10)
  expect_that(length(listeners(a)), equals(0))
  
  add_listener(a, function() 1)
  expect_that(length(listeners(a)), equals(1))
})