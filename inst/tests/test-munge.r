context("munge function")

test_that("it correctly does nothing if no mungepieces are passed", {
  expect_equal(munge(iris), iris[,])
})

test_that("it correctly adds to the mungepieces list", {
  args <- lapply(seq_len(2),
    function(.) list(column_transformation(function(x) x), 1))
  iris2 <- munge(iris, args)
  expect_equal(length(attr(iris2, 'mungepieces')), 2)
})

test_that("it correctly munges a simple mungepiece sequence", {
  args <- lapply(seq_len(2) + 1,
    function(x) list(column_transformation(function(x, v) v * x), 1, x))
  iris2 <- munge(iris, args)
  expect_equal(iris2[[1]], iris[[1]] * 6)
})

test_that("it correctly predicts a simple mungepiece sequence", {
  args <- lapply(seq_len(2),
    function(.) list(list(column_transformation(function(x) 2 * x), 1),
                     list(column_transformation(function(x) 3 * x), 1)))
  iris2 <- munge(iris, args)
  iris2 <- munge(iris, attr(iris2, 'mungepieces'))
  expect_equal(iris2[[1]], iris[[1]] * 9)
})

test_that("it correctly predicts a simple multicolumn transformation", {
  iris2 <- munge(iris,
    list(multi_column_transformation(`*`),
         c('Sepal.Length', 'Sepal.Width'), 'Sepal.Area'))
  expect_equal(iris2[['Sepal.Area']], with(iris, Sepal.Length * Sepal.Width))
})

test_that("it correctly predicts using a munged dataframe", {
  args <- lapply(seq_len(2),
    function(.) list(list(column_transformation(function(x) 2 * x), 1),
                     list(column_transformation(function(x) 3 * x), 1)))
  iris2 <- munge(iris, args)
  iris2 <- munge(iris, iris2)
  expect_equal(iris2[[1]], iris[[1]] * 9)
})

test_that("it correctly trains using multiple ... arguments", {
  args <- lapply(seq_len(2),
    function(.) list(column_transformation(function(x) x), 1))
  iris2 <- do.call(munge, append(list(iris), args))
  expect_equal(length(attr(iris2, 'mungepieces')), 2)
})

test_that("it correctly handles a single argument that is not a nested list", {
  iris2 <- munge(iris, 
    list(c(column_transformation(function(x) 2 * x),
           column_transformation(function(x) 3 * x)), 'Sepal.Length'))
  expect_equal(iris2[[1]], iris[[1]] * 2)
  iris2 <- munge(iris, iris2)
  expect_equal(iris2[[1]], iris[[1]] * 3)
})

test_that("it procures the correct stagerunner", {
  args <- lapply(seq_len(2),
    function(.) list(list(column_transformation(function(x) 2*x), NULL), 1))
  sr <- munge(iris, args, stagerunner = TRUE)
  expect_is(sr, 'stageRunner')
  expect_equal(length(sr$stages), 3)
  sr$run()
  expect_equal(sr$context$data[[1]], 4 * iris[[1]])
  sr$run()
  expect_equal(sr$context$data[[1]], 4 * iris[[1]])
})

test_that("it procures a stagerunner with training only if train_only = TRUE", {
  args <- lapply(seq_len(2),
    function(.) list(list(column_transformation(function(x) 2*x), NULL), 1))
  sr <- munge(iris, args, stagerunner = TRUE, train_only = TRUE)
  expect_identical(sr$context$data, iris)
  sr$run(1)
  sr$run(1)
  expect_identical(sr$context$data[[1]], 4 * iris[[1]],
    info = "The stagerunner should only run the training step")
})

