context("mungebit reference class")

test_that("it correctly sets trained flag after one run", {
  mb <- mungebit(column_transformation(function(x) x))
  expect_true(!mb$trained)
  mb$run(mungeplane(iris))
  expect_true(mb$trained)
})

test_that("it correctly executes training and prediction function", {
  mb <- mungebit(
    column_transformation(function(x) assign('..train', TRUE, globalenv())),
    column_transformation(function(x) assign('..predict', TRUE, globalenv())))
  lapply(seq_len(2), function(x) mb$run(mungeplane(iris)))
  expect_true(..train); expect_true(..predict)
  rm(..train, envir = globalenv()); rm(..predict, envir = globalenv())
})

test_that("it sets inputs correctly", {
  mb <- mungebit(column_transformation(function(x) { inputs <<- list(1); x }))
  mb$run(mungeplane(iris))
  expect_equal(mb$inputs, list(1))
})

