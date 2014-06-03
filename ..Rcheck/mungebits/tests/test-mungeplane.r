context("mungeplane")

test_that("it initializes correctly", {
  expect_equal(mungeplane(iris)$data, iris)
})

test_that("it converts environments to mungeplanes", {
  tmp <- new.env()
  tmp2 <- mungeplane(tmp)
  class(tmp2) <- 'environment'
  expect_equal(tmp2, tmp)
})
