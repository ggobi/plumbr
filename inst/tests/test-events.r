library(testthat)

context("Events")

test_that("can add listeners", {
  a <- mutaframe(a = 1:10)
  expect_that(length(listeners(a)), equals(0))
  
  add_listener(a, function() 1)
  expect_that(length(listeners(a)), equals(1))
})

test_that("callback recieves correct info", {
  a <- mutaframe(a = 1:10)
  
  event <- NULL
  add_listener(a, function(i, j) {
    event <<- list(i = i, j = j)
  })

  a[1, 1] <- 2
  expect_that(event$i, equals(1))
})