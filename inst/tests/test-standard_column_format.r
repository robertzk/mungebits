context('standard column format')

test_that('it correctly parses numeric input', {
  expect_equal(standard_column_format(c(1,5), iris),
               c('Sepal.Length', 'Species'))
})

test_that('it correctly parses logical input', {
  expect_equal(standard_column_format(c(TRUE, FALSE, FALSE, FALSE, TRUE), iris),
               c('Sepal.Length', 'Species'))
})

test_that('it correctly parses character input', {
  expect_equal(standard_column_format('Sepal.Length', iris), 'Sepal.Length')
})

