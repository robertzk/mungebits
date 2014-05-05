context('inject_inputs')

test_that('it correctly injects a sample environment', {
  inputs <- list()
  fn <- function() {}
  inject_inputs(fn)
  expect_true('inputs' %in% ls(environment(fn)))
})

test_that('it maintains the debug flag on a function', {
  inputs <- list()
  fn <- function() {}
  debug(fn)
  inject_inputs(fn)
  expect_true(isdebugged(fn))
})

