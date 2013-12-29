context("mungeplane")

test_that("it initializes correctly", {
  expect_equal(mungeplane(iris)$data, iris)
})

