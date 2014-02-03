#if (!isClass('functionOrNULL'))
#  setClassUnion('functionOrNULL', c('function', 'transformation', 'NULL'))

#' Constructor for mungebit class.
#'
#' Mungebits are atomic data transformations of a data.frame that,
#' loosely speaking, aim to modify "one thing" about a variable or
#' collection of variables. This is pretty loosely defined, but examples
#' include dropping variables, mapping values, discretization, etc.
#'
#' @docType class
#' @name mungebit
#' @param train_fn a function. This specifies the behavior to perform
#'    on the dataset when preparing for model training. A value of NULL
#'    specifies that there should be no training step.
#' @param predict_fn a function. This specifies the behavior to perform
#'    on the dataset when preparing for model prediction. A value of NULL
#'    specifies that there should be no prediction step.
#' @param inputs a list. Used for maintaining meta-data between
#'    training and prediction runs.
#' @param trained a logical. Used for determining whether or not the
#'    mungebit has been run on a dataset already.
#' @seealso \code{\link{mungepiece}}
#' @examples
#' \dontrun{
#' mp <- mungeplane(iris)
#' mb <- mungebit(column_transformation(function(col, scale = NULL) {
#'   if ('scale' %in% names(inputs))
#'     cat("Column scaled by ", inputs$scale, "\n")
#'   else inputs$scale <<- scale
#'   col * inputs$scale
#' }))
#' mb$run(mp, 'Sepal.Length', 2)
#' # mp$data now contains a copy of iris w/ the Sepal.Length column doubled
#' head(mp$data[[1]] / iris[[1]])
#' # > [1] 2 2 2 2 2 2
#' mb$run(mp, 'Sepal.Length')
#' # > Column scaled by 2
#' head(mp$data[[1]] / iris[[1]])
#' # > [1] 4 4 4 4 4 4 
#' }
#' 
mungebit <- setRefClass('mungebit',
  fields = list(train_function = 'ANY',
                predict_function = 'ANY',
                arguments_cache = 'list',
                inputs = 'list',
                trained = 'logical'),
  methods = list(
    initialize = function(train_fn = function(x) x, predict_fn = train_fn) {
      train_function <<- train_fn
      predict_function <<- predict_fn
      inputs <<- list()
      trained <<- FALSE
    },
    
    run = function(mungeplane, ...) {
      if (!trained) train(mungeplane, ...)
      else predict(mungeplane, ...)
      invisible()
    },
    
    predict = function(mungeplane, ...) {
      if (!is.null(predict_function)) predict_function(mungeplane$data, ...) 
    },

    train = function(mungeplane, ...) {
      on.exit(trained <<- TRUE)
      if (!is.null(train_function)) train_function(mungeplane$data, ...) 
    }
  )
)

is.mungebit <- function(x) inherits(x, 'mungebit')


# S3 class...uglier way to do it
# mungebit <- function(train_function,
#                      predict_function = train_function, 
#                      modifies.column = TRUE,
#                      modifies.row = FALSE,
#                      modifies.column.dimension = FALSE,
#                      modifies.row.dimension = FALSE) {
#   function(...) {
#     arguments <- as.list(...)
#     # function(df) {
#     #   do.call(mungebit_function, arguments)
#   }
# }

