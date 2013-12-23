context('multi column transformations')

test_that('correctly transforms one column by multiplying by two', {
  iris2 <- iris
  doubler <- multi_column_transformation(function(x) 2 * x)
  doubler(iris2, 'Sepal.Length')
  expect_equal(iris2[[1]], 2 * iris[[1]],
               "multi_column_transformation must double first column of iris2")
})

test_that('correctly creates derivative column that is multiple by two', {
  iris2 <- iris
  doubler <- multi_column_transformation(function(x, v) v * x)
  doubler(iris2, 'Sepal.Length', 'Sepal.Length2', 2)
  expect_equal(iris2[['Sepal.Length2']], 2 * iris[[1]],
    paste0("multi_column_transformation must create new column 'Sepal.Length2'",
           " whose values are double those in the first column of iris2"))
})

test_that('correctly combines two columns into one new derivative column', {
  iris2 <- iris
  adder <- multi_column_transformation(function(x, y) x + y)
  adder(iris2, c('Sepal.Length', 'Sepal.Width'), 'sum')
  expect_equal(iris2[['sum']], with(iris2, Sepal.Length + Sepal.Width),
   info = paste("multi_column_transformation must create new column 'sum' ",
                "w/ values the sum of those in Sepal.Length and Sepal.Width"))
})

test_that('correctly swaps two columns', {
  iris2 <- iris
  swapper <- multi_column_transformation(function(x, y) list(y, x))
  swapper(iris2, c('Sepal.Length', 'Sepal.Width'))
  expect_equal(unname(iris2[c('Sepal.Length', 'Sepal.Width')]),
               unname(iris[c('Sepal.Width', 'Sepal.Length')]),
   info = paste("multi_column_transformation must swap first 2 columns"))
})

test_that('correctly removes columns', {
  iris2 <- iris
  remover <- multi_column_transformation(function(...) NULL)
  remover(iris2, 'Sepal.Length')
  expect_true(!'Sepal.Length' %in% colnames(iris2),
    info = paste("multi_column_transformation must remove first column"))
  remover(iris2, c(2, 3))
  expect_equal(sum(c('Petal.Length', 'Petal.Width') %in% colnames(iris2)), 0,
    info = paste("multi_column_transformation must remove first two columns"))
  expect_equal(length(iris2), 2,
    info = paste("multi_column_transformation must retain 2nd and 5th columns"))
})

test_that('correctly replaces NA', {
  iris2 <- iris
  replace_na <- function(...) {
    args <- list(...)
    val <- args[[length(args)]]
    args <- args[seq_len(length(args) - 1)]
    if (length(args) == 1) { args[[1]][is.na(args[[1]])] <- val; args[[1]] }
    else lapply(args, function(x) { x[is.na(x)] <- val; x })
  }
  iris2[1, ] <- NA
  na_replacer <- multi_column_transformation(replace_na)
  cols <- colnames(iris2)[1:1]
  replace_val <- 10
  na_replacer(iris2, cols, cols, replace_val)
  
  expect_equal(unname(unlist(iris2[1, cols])), rep(replace_val, length(cols)),
    info = paste(
      "multi_column_transformation",
      testthat:::colourise("na_replacer", "blue"),
      "must replace NAs in first row w/", replace_val))
})

test_that('correctly transforms using numeric column indices', {
  iris2 <- iris
  doubler <- multi_column_transformation(function(x) 2 * x)
  doubler(iris2, 2)
  
  expect_equal(iris2[[2]], 2 * iris[[2]],
     info = paste("multi_column_transformation must be able to reference",
                  "columns using numeric indices",
                  "(e.g., doubler(iris2, 2)"))
})

test_that('correctly transforms using logical column indices', {
  iris2 <- iris
  doubler <- multi_column_transformation(function(x) 2 * x)
  doubler(iris2, 'Sepal.Width' == colnames(iris2))
  
  expect_equal(iris2[[2]], 2 * iris[[2]],
     info = paste("column_transformation must be able to reference",
                  "columns using numeric indices",
                  "(e.g., doubler(iris2, c(F,T,F,F,F))"))
})

test_that('accepts transformation calls with missing arguments', {
  iris2 <- iris[, 1:4]
  scaler <- multi_column_transformation(function(...) {
    args <- list(...); const <- args[[length(args)]]; args <- head(args, -1)
    if (length(args) == 1) args[[1]] * const
    else lapply(args, function(col) col * const)
  })
  scaler(iris2, , , 2)
  expect_equal(iris2, 2 * iris[, 1:4],
               info = "column_transformation must double first column of iris2")
})

test_that('transforms a partial data frame', {
  iris2 <- iris
  swapper <- multi_column_transformation(function(x, y) list(y, x))
  swapper(iris2[c(1, 2)])
  expect_equal(unname(iris2[, 1:2]), unname(iris[, 2:1]),
    info = "column_transformation must swap values of first two columns")
})

# This is technically a benchmark but I have no place to put it yet
test_that('it doubles a column no more than 3.5x as slow as a raw operation', {
  require(microbenchmark)
  iris2 <- iris
  raw_double <- function(dataframe, cols) {
    class(dataframe) <- 'list'
    for(col in cols) dataframe[[col]] <- 2 * dataframe[[col]]
    class(dataframe) <- 'data.frame'
    dataframe
  }
  numeric_cols <- names(which(vapply(iris2, is.numeric, logical(1))))
  doubler <- multi_column_transformation(
    function(...) lapply(list(...), function(x) 2 * x))
  speeds <- summary(microbenchmark(doubler(iris2, numeric_cols),
                                   raw_double(iris2, numeric_cols),
                                   times = 5L))
  multi_column_transformation_runtime <- speeds$median[[1]]
  apply_raw_function_runtime <- speeds$median[[2]]
  # The 3.5 is sort of a magic value here but it is almost always OK.
  expect_true(multi_column_transformation_runtime <
                3.5 * apply_raw_function_runtime,
    paste0("Execution of ",
     testthat:::colourise('multi_column_transformation', "blue"),
     " took too long: \nFormer took ",
     testthat:::colourise(paste0(multi_column_transformation_runtime, "ms"), "red"),
     " but latter took ",
     testthat:::colourise(paste0(apply_raw_function_runtime, "ms"), "red"), ".\n",
     "You need to make sure the code for multi_column_transformation\n",
     "stays efficient relative to ",
     testthat:::colourise('raw_double', 'blue'),
     " (see code for this unit test)"))
})

