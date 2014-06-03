context("mungepiece reference class")
require(mungebitsTransformations)

test_that("it correctly initializes without prediction arguments", {
  mb <- mungebit(function(x) x)
  expect_equal(as.character(class(mungepiece(mb, list()))), "mungepiece")
})

test_that("it correctly initializes with prediction arguments", {
  mb <- mungebit(function(x) x)
  expect_equal(as.character(class(mungepiece(mb, list(), list()))), "mungepiece")
})

test_that("run methods correctly executes on a trivial case", {
  mb <- mungebit(column_transformation(function(x) 2 * x))
  mp <- mungepiece(mb, 1)
  plane <- mungeplane(iris)
  lapply(seq_len(2), function(x) mp$run(plane))
  expect_equal(plane$data[[1]], iris[[1]] * 4)
})

test_that("it does nothing if a mungepiece is already given", {
  mp <- mungepiece(mungebit(function(x) x))
  expect_equal(parse_mungepiece(mp), mp)
})

