context('column transformations')

test_that('correctly transforms one column by multipling by two', {
  iris2 <- iris
  doubler <- column_transformation(function(x) 2 * x)
  doubler(iris2, 'Sepal.Length')
  expect_equal(iris2[[1]], 2 * iris[[1]],
               "column_transformation must double first column of iris2")
})

test_that('correctly transforms multiple columns by multipling by two', {
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
  char_df <- data.frame(vapply(iris, as.character, character(nrow(iris))),
                        stringsAsFactors = FALSE)
  expect_equal(iris2, char_df,
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

# This is technically a benchmark but I have no place to put it yet
test_that('it doubles a column no more than twice as slow as a raw operation', {
  require(microbenchmark)
  iris2 <- iris
  raw_double <- function(df, cols) {
    class(df) <- 'list'
    for(col in cols) df[[col]] <- 2 * df[[col]]
    class(df) <- 'data.frame'
    df
  }
  numeric_cols <- colnames(iris2)[which(vapply(iris2, is.numeric, logical(1)))]
  doubler <- column_transformation(function(x) 2 * x)
  speeds <- summary(microbenchmark(doubler(iris2, numeric_cols),
                                   raw_double(iris2, numeric_cols),
                                   times = 5L))
  column_transformation_runtime <- speeds$median[[1]]
  apply_raw_function_runtime <- speeds$median[[2]]
  expect_true(column_transformation_runtime < 3.0 * apply_raw_function_runtime,
    paste0("Execution of ", testthat:::colourise('column_transformation', "blue"),
     " took too long: \nFormer took ",
     testthat:::colourise(paste0(column_transformation_runtime, "ms"), "red"),
     " but latter took ",
     testthat:::colourise(paste0(apply_raw_function_runtime, "ms"), "red"), ".\n",
     "You need to make sure the code for column_transformation\n",
     "stays efficient relative to ",
     testthat:::colourise('raw_double', 'blue'),
     " (see code for this unit test)"))
})
