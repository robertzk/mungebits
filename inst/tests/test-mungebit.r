context("mungebit reference class")
require(mungebitsTransformations)

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

test_that("it accepts a NULL predict function", {
  assign("*count*", 0, globalenv())
  mb <- mungebit(column_transformation(function(x) {
                                       `*count*` <<- `*count*` + 1; x }), NULL)
  mb$run(mungeplane(iris), 1)
  expect_equal(`*count*`, 1)
  mb$run(mungeplane(iris), 1)
  expect_equal(`*count*`, 1)
  rm("*count*", envir = globalenv())
})

test_that("it accepts a NULL train function", {
  assign("*count*", 0, globalenv())
  mb <- mungebit(NULL, column_transformation(function(x) {
                                       `*count*` <<- `*count*` + 1; x }))
  mb$run(mungeplane(iris), 1)
  expect_equal(`*count*`, 0)
  mb$run(mungeplane(iris), 1)
  expect_equal(`*count*`, 1)
  rm("*count*", envir = globalenv())
})

test_that("it can read inputs", {
  mb <- mungebit(function(df) {
    if (!'foo' %in% names(inputs)) { inputs$foo <<- 1; print("one") }
    else print("two") })
  mp <- mungeplane(iris)
  expect_output(mb$run(mp), "one")
  expect_output(mb$run(mp), "two")
})

