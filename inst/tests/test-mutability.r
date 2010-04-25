context("Mutability")

test_that("changing one affects copies", {
  a <- pframe(a = 1:10)
  b <- a
  
  a[1,1] <- 2
  expect_that(b[1, 1], equals(2))

  b[1,1] <- 3
  expect_that(a[1, 1], equals(3))
})
