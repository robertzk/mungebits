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
mungebit__initialize <- function(train_fn = function(x) x,
                                  predict_fn = train_fn, enforce_train = TRUE) {
  train_function <<- train_fn
  predict_function <<- predict_fn

  inputs <<- list()
  trained <<- FALSE
  enforce_train <<- enforce_train
}

#' Run a mungebit.
#' 
#' Imagine flipping a switch on a set of train tracks. A mungebit
#' behaves like this: once the \code{trained} switch is flipped,
#' it can only run the \code{predict_fn}, otherwise it will
#' run the \code{train_fn}.
#'
#' @alias mungebit__initialize
#' @param mungeplane mungeplane. Essentially an environment containing
#'   a \code{data} variable.
#' @param ... additional arguments to the mungebit's \code{train_fn} or
#'   \code{predict_fn}.
#' @seealso \code{\link{mungebit__initialize}}
mungebit__run <- function(mungeplane, ...) {
  # We cannot use, e.g., .self$train(mungeplane, ...),
  # because we must force the ... to get evaluated due to
  # non-standard evaluation in the train and predict methods.
  do.call(if (!trained) .self$train else .self$predict,
          list(mungeplane, ...))
  invisible()
}

#' Run the predict function on a mungebit.
#'
#' @alias mungebit__initialize
#' @param mungeplane mungeplane. Essentially an environment containing
#'   a \code{data} variable.
#' @param ... additional arguments to the mungebit's \code{predict_fn}.
#' @seealso \code{\link{mungebit__run}}, \code{\link{mungebit__initialize}}
mungebit__predict <- function(mungeplane, ...) {
  if (!is.null(predict_function)) {
    inject_inputs(predict_function)
    on.exit(environment(predict_function) <<-
      parent.env(environment(predict_function)))

    predict_function(mungeplane$data, ...) 
  }
  invisible(TRUE)
}

#' Run the train function on a mungebit.
#'
#' @alias mungebit__initialize
#' @param mungeplane mungeplane. Essentially an environment containing
#'   a \code{data} variable.
#' @param ... additional arguments to the mungebit's \code{train_fn}.
#' @seealso \code{\link{mungebit__run}}, \code{\link{mungebit__initialize}}
mungebit__train <- function(mungeplane, ...) {
  if (!is.null(train_function)) {
    inject_inputs(train_function)
    on.exit(environment(train_function) <<-
      parent.env(environment(train_function)))

    train_function(mungeplane$data, ...) 

    # TODO: Oh no. :( Sometimes inputs is being set and sometimes
    # environment(train_function)$inputs is being set--I think this
    # has to do with changing the environment of the function that's
    # running. How do we get around this? This seems incredibly messy.
    inputs <<-
      if (length(tmp <- environment(train_function)$inputs) > 0) tmp
      else inputs
  }
  if (enforce_train) trained <<- TRUE
  invisible(TRUE)
}

mungebit <- setRefClass('mungebit',
  fields = list(train_function = 'ANY',
                predict_function = 'ANY',
                inputs = 'list',
                trained = 'logical',
                enforce_train = 'logical'),
  methods = list(
    initialize = mungebits:::mungebit__initialize,
    run        = mungebits:::mungebit__run,
    predict    = mungebits:::mungebit__predict,
    train      = mungebits:::mungebit__train
  )
)

is.mungebit <- function(x) inherits(x, 'mungebit')

#' Inject a parent environment that has only an inputs key so that
#' things like \code{inputs <<- 'foo'} work.
#'
#' @param fn function. The function on which to inject.
inject_inputs <- function(fn) {
  eval.parent(substitute({
    run_env <- new.env(parent = environment(fn))
    run_env$inputs <- inputs
    debug_flag <- isdebugged(fn)
    environment(fn) <<- run_env
    # Restore debugging if it was enabled.
    if (debug_flag) debug(fn)
  }))
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

