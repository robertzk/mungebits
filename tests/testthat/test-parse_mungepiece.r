context("parse_mungepiece function")

test_that("it correctly parser just a training_function with 1 argument", {
  doubler <- column_transformation(function(x) x * 2)
  mp <- parse_mungepiece(list(doubler, 'Sepal.Length'))
  iris2 <- mungeplane(iris)
  mp$run(iris2)
  expect_equal(iris2$data[[1]], 2 * iris[[1]],
    info = "First column of iris2$data should be twice first column of iris")
})

test_that(paste0("it correctly parses a training_function with ",
                 "different prediction_function with same arguments"), {
  doubler <- column_transformation(function(x) x * 2)
  tripler <- column_transformation(function(x) x * 3)
  mp <- parse_mungepiece(list(c(doubler, tripler), 'Sepal.Length'))
  iris2 <- mungeplane(iris)
  mp$run(iris2)
  expect_equal(iris2$data[[1]], 2 * iris[[1]]) 
  mp$run(iris2)
  expect_equal(iris2$data[[1]], 6 * iris[[1]], 
    info = "First column of iris2$data should be 2*3 first column of iris")
})

test_that(paste0("it correctly parses training and predictions functions ",
                 "with different arguments"), {
  imputer <- column_transformation(function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE); x
  })
  restorer <- column_transformation(function(x, imputed_value) {
    x[is.na(x)] <- imputed_value; x
  })
  mp <- parse_mungepiece(list(c(imputer, 'Sepal.Length'),
                              c(restorer, 1, mean(iris[[1]]))))
  mp$run(mungeplane(iris))
  plane <- mungeplane(structure(data.frame(cbind(NA_real_)), names = names(iris)[1]))
  
  mp$run(plane)
  expect_equal(mean(iris[[1]]), plane$data[1, 1],
    info = paste("plane$data should have had its one missing value imputed",
                 "with the mean of the first column of iris"))
})

test_that("it correctly uses the inputs keyword to cache results", {
  imputer <- column_transformation(function(x) {
    if (!'mean' %in% names(inputs)) inputs$mean <<- mean(x, na.rm = TRUE)
    x[is.na(x)] <- inputs$mean; x
  })
  mp <- parse_mungepiece(list(imputer, 'Sepal.Length'))
  mp$run(mungeplane(iris))
  plane <- mungeplane(structure(
    list(c(NA_real_)), names = 'Sepal.Length', class = 'data.frame',
    row.names = 1L))
  mp$run(plane)
  expect_equal(mean(iris[[1]]), plane$data[1, 1],
    info = paste("plane$data should have had its one missing value imputed",
                 "with the mean of the first column of iris by caching the",
                 "mean in the inputs field of the mungebit"))
})

test_that("it correctly uses the train_only argument", {
  doubler <- column_transformation(function(x) x * 2)
  mp <- parse_mungepiece(list(list(doubler, NULL), 'Sepal.Length'), train_only = TRUE)
  iris2 <- mungeplane(iris)
  mp$run(iris2)
  mp$run(iris2)
  expect_equal(iris2$data[[1]], 4 * iris[[1]],
    info = "The mungepiece should not run prediction")
})

test_that("it correctly parses a mungebit", {
  mb <- mungebit(doubler <- column_transformation(function(x) x * 2))
  mp <- parse_mungepiece(list(doubler))
  iris2 <- mungeplane(iris)
  mp$run(iris2, 1)
  expect_equal(iris2$data[[1]], 2 * iris[[1]])
  expect_equal(iris2$data[[2]], iris[[2]])

  mp <- parse_mungepiece(list(doubler, 1))
  iris2 <- mungeplane(iris)
  mp$run(iris2)
  expect_equal(iris2$data[[1]], 2 * iris[[1]])
  expect_equal(iris2$data[[2]], iris[[2]])
})

test_that("it correctly parses a mungepiece", {
  mp <- mungepiece(mungebit(identity))
  expect_identical(parse_mungepiece(list(mp)), mp)
})
