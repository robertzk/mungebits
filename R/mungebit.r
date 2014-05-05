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
                trained = 'logical',
                enforce_train = 'logical'),
  methods = list(
    initialize = function(train_fn = function(x) x, predict_fn = train_fn, enforce_train = TRUE) {
      train_function <<- inject_inputs(train_fn)
      predict_function <<- inject_inputs(predict_fn)

      inputs <<- list()
      trained <<- FALSE
      enforce_train <<- enforce_train
    },
    
    run = function(mungeplane, ...) {
      # We cannot use, e.g., .self$train(mungeplane, ...),
      # because we must force the ... to get evaluated due to
      # non-standard evaluation in the train and predict methods.
      do.call(if (!trained) .self$train else .self$predict,
              list(mungeplane, ...))
      invisible()
    },
    
    predict = function(mungeplane, ...) {
      if (!is.null(predict_function)) {
        run_env <- new.env(parent = environment(predict_function))
        run_env$inputs <- inputs
        debug_flag <- isdebugged(predict_function)
        environment(predict_function) <<- run_env
        on.exit(environment(predict_function) <<- parent.env(run_env))
        if (debug_flag) debug(predict_function)

        predict_function(mungeplane$data, ...) 
      }
      invisible(TRUE)
    },

    train = function(mungeplane, ...) {
      if (!is.null(train_function)) {
        run_env <- new.env(parent = environment(train_function))
        run_env$inputs <- list()
        debug_flag <- isdebugged(train_function)
        environment(train_function) <<- run_env
        on.exit(environment(train_function) <<- parent.env(run_env))
        if (debug_flag) debug(train_function)

        train_function(mungeplane$data, ...) 

        # TODO: Oh no. :( Sometimes inputs is being set and sometimes run_env$inputs
        # is being set--I think this has to do with changing the environment of
        # the function that's running. How do we get around this? This seems
        # incredibly messy.
        inputs <<- if (length(run_env$inputs) > 0) run_env$inputs else inputs
      }
      if (enforce_train) trained <<- TRUE
      invisible(TRUE)
    }
  )
)

is.mungebit <- function(x) inherits(x, 'mungebit')

#' Inject a parent environment that has only an inputs key so that
#' things like \code{inputs <<- 'foo'} work.
#'
#' @param fn function. The function on which to inject.
inject_inputs <- function(fn) {
  if (is.null(fn)) return(NULL)
  middle_parent <- new.env()
  middle_parent$inputs <- list()
  parent.env(middle_parent) <- parent.env(environment(fn))
  parent.env(environment(fn)) <- middle_parent
  fn
}

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

