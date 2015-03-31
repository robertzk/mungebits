context('column transformations')

# TODO: Test negative indexing

test_that('correctly transforms one column by multiplying by two', {
  iris2 <- iris
  doubler <- column_transformation(function(x) 2 * x)
  doubler(iris2, 'Sepal.Length')
  expect_identical(iris2[[1]], 2 * iris[[1]],
               "column_transformation must double first column of iris2")
})

test_that('correctly transforms multiple columns by multiplying by two', {
  iris2 <- iris
  doubler <- column_transformation(function(x) 2 * x)
  doubler(iris2, c('Sepal.Length', 'Sepal.Width'))
  expect_equal(iris2[, 1:2], 2 * iris[, 1:2],
               info = paste("column_transformation must double first",
                            "2 columns of iris2"))
})

test_that('correctly transforms one column by converting to character', {
  iris2 <- iris
  stringer <- column_transformation(as.character)
  stringer(iris2, 'Sepal.Length')
  expect_equal(iris2[[1]], as.character(iris[[1]]),
               info = paste("column_transformation must convert to character",
                            "first column of iris2"))
})

test_that('correctly transforms multiple columns by converting to character', {
  iris2 <- iris
  stringer <- column_transformation(as.character)
  stringer(iris2, colnames(iris2))
  char_dataframe <- data.frame(vapply(iris, as.character, character(nrow(iris))),
                        stringsAsFactors = FALSE)
  expect_equal(iris2, char_dataframe,
               info = paste("column_transformation must convert to character",
                            "all columns of iris2"))
})

test_that('correctly transforms a column into rounded factors', {
  iris2 <- iris
  round_and_factor <- column_transformation(function(x) factor(round(x)))
  round_and_factor(iris2, 'Sepal.Length')
  
  expect_equal(iris2[[1]], factor(round(iris[[1]])),
               info = paste("column_transformation must convert to factor",
                            "after rounding first column of iris2"))
})

test_that('correctly transforms using numeric column indices', {
  iris2 <- iris
  doubler <- column_transformation(function(x) 2 * x)
  doubler(iris2, 2)
  
  expect_equal(iris2[[2]], 2 * iris[[2]],
               info = paste("column_transformation must be able to reference",
                            "columns using numeric indices",
                            "(e.g., doubler(iris2,2)"))
})

test_that('correctly transforms using logical column indices', {
  iris2 <- iris
  doubler <- column_transformation(function(x) 2 * x)
  doubler(iris2, 'Sepal.Width' == colnames(iris2))
  
  expect_equal(iris2[[2]], 2 * iris[[2]],
               info = paste("column_transformation must be able to reference",
                            "columns using numeric indices",
                            "(e.g., doubler(iris2, c(F,T,F,F,F))"))
})

test_that('correctly imputes means', {
  iris2 <- iris
  mean_imputer <- column_transformation(function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE); x
  })
  iris2[1, ] <- NA
  mean_imputer(iris2, 1)
  non_nas <- tail(iris[[1]], -1)
  
  expect_equal(unlist(unname(iris2[, 1])),
               c(mean(non_nas), non_nas),
               info = paste("column_transformation must impute NAs with mean"))
})

test_that('correctly passes dots arguments', {
  iris2 <- iris[, 1:4]
  scaler <- column_transformation(function(x, v) v * x)
  scaler(iris2, , 2)
  expect_equal(iris2, 2 * iris[, 1:4],
               info = "column_transformation must double first column of iris2")
})

test_that('accepts transformation calls with missing arguments', {
  iris2 <- iris[, 1:4]
  doubler <- column_transformation(function(x) 2 * x)
  doubler(iris2)
  expect_equal(iris2, 2 * iris[, 1:4],
               info = "column_transformation must double first column of iris2")
})

test_that('correctly uses mutating transformations', {
  iris2 <- iris
  mutater <- column_transformation(function(x) { inputs <<- 'test'; x })
  inputs <- NULL
  (function() { mutater(iris2) })()
  expect_equal(inputs, 'test')
})

test_that('correctly runs column transformations using a function as column name specifier', {
  iris2 <- iris
  doubler <- column_transformation(function(x) 2*x)
  doubler(iris2, is.numeric)
  expect_equal(iris2[, 1:4], iris[, 1:4]*2)
})

test_that('correctly uses named transformations', {
  iris2 <- iris
  name_duper <- column_transformation(function(x) {
    x[[1]] <- rep(names(x)[[1]], length(x[[1]])); x[[1]] }, named = TRUE)
  name_duper(iris2, 1)
  expect_equal(iris2[[1]], rep(names(iris)[[1]], nrow(iris)))
})


# This is technically a benchmark but I have no place to put it yet
test_that('it doubles a column no more than 4.5x as slow as a raw operation', {
  require(microbenchmark)
  iris2 <- iris
  raw_double <- function(dataframe, cols) {
    class(dataframe) <- 'list'
    for(col in cols) dataframe[[col]] <- 2 * dataframe[[col]]
    class(dataframe) <- 'data.frame'
    dataframe
  }
  numeric_cols <- colnames(iris2)[which(vapply(iris2, is.numeric, logical(1)))]
  doubler <- column_transformation(function(x) 2 * x)
  speeds <- summary(microbenchmark(doubler(iris2, numeric_cols),
                                   raw_double(iris2, numeric_cols),
                                   times = 5L))
  column_transformation_runtime <- speeds$median[[1]]
  apply_raw_function_runtime <- speeds$median[[2]]
  # The 4.5 is sort of a magic value here but it is almost always OK.
  # TODO: (RK) Re-enable this - Doesn't work on Travis CI
#  expect_true(column_transformation_runtime < 4.5 * apply_raw_function_runtime,
#    paste0("Execution of ", testthat:::colourise('column_transformation', "blue"),
#     " took too long: \nFormer took ",
#     testthat:::colourise(paste0(column_transformation_runtime, "ms"), "red"),
#     " but latter took ",
#     testthat:::colourise(paste0(apply_raw_function_runtime, "ms"), "red"), ".\n",
#     "You need to make sure the code for column_transformation\n",
#     "stays efficient relative to ",
#     testthat:::colourise('raw_double', 'blue'),
#     " (see code for this unit test)"))
})

