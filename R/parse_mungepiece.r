#' Translate a list of arguments passed to a mungebit into a mungepiece
#'
#' For example, one can pass a training function, a prediction function,
#' and additional arguments, and the constructed mungepiece will hold
#' these arguments, ready to call the attached mungebit on a given dataframe.
#'
#' @param args a list. This can be of the formats
#'   \code{list(train_fn, ...)}, \code{list(list(train_fn, predict_fn), ...)},
#'   and \code{list(list(train_fn, ...), list(predict_fn))}. In the first
#'   example, the train and predict function are assumed to be identical. In
#'   the first two examples, the arguments to these functions are assumed to
#'   be identical (for example, if the same kind of filter needs to be applied
#'   to a data set that is about to be trained as to one about to be
#'   predicted). Finally, the last example is the most flexible and allows
#'   different parameters for the training and prediction function,
#'   respectively. The given training and prediction functions are used to
#'   construct a \code{mungebit}, and the resulting \code{mungebit} and 
#'   the remaining arguments are stored in a \code{mungepiece}.
#' @return the parsed mungepiece
#' @export
#' @examples
#' \dontrun{
#' doubler <- column_transformation(function(x) x * 2)
#' mp <- parse_mungepiece(list(doubler, 'Sepal.Length')) 
#' iris2 <- mungeplane(iris)
#' mp$run(iris2)
#' stopifnot(all.equal(iris2$data[[1]], 2 * iris[[1]]))
#' 
#' # TODO: Way more examples, unit tests
#' 
#' }
parse_mungepiece <- function(args) {
  if (is.mungepiece(args)) return(args)
  if (is.function(args)) args <- list(args)
  stopifnot(is.list(args))  

  if (is.list(args[[1]]) && length(args) > 1 && is.list(args[[2]]) &&
      length(args[[1]]) > 0 && is.function(args[[1]][[1]]) &&
      length(args[[2]]) > 0 && is.function(args[[2]][[1]])) {
    # train and predict functions have separate arguments
    # list(list(train_fn, ...), list(predict_fn, ...))
    train_fn <- parse_function(args[[1]][1])
    args[[1]][[1]] <- NULL
    train_args <- args[[1]]
    
    predict_fn <- parse_function(args[[2]][1])
    args[[2]][[1]] <- NULL
    predict_args <- args[[2]]
  } else {
    stopifnot(length(args) > 0)
    if (is.list(args[[1]])) {
      # list(list(train_fn, predict_fn), ...)
      stopifnot(length(args[[1]]) == 2)
      train_fn <- parse_function(args[[1]][1])
      predict_fn <- parse_function(args[[1]][2])
    } else {
      # list(train_fn, ...)
      train_fn <- parse_function(args[1])
      predict_fn <- train_fn
    }
                      
    args[[1]] <- NULL
    train_args <- args
    predict_args <- train_args
  }
  stopifnot((is.function(train_fn) || is.null(train_fn)) && 
            (is.function(predict_fn) || is.null(predict_fn)))
  #if (!is.null(train_fn)) class(train_fn) <- 'function'    # Clear triggers
  #if (!is.null(predict_fn)) class(predict_fn) <- 'function'

  mungepiece(mungebit(train_fn, predict_fn),
             train_args,
             predict_args)
}

#' Parse a function for training or prediction.
#'
#' This is a helper method to allow one to specify things like:
#'
#' \code{list(c = function(column) 2 * column)}           # column transformation
#' \code{list(r = function(row) ifelse(row < 0, 0, row))} # row transformation
#' \code{list(g = function(dataframe) 2 * dataframe}      # global transform
#' \code{list(cn = function(x) paste0(names(x), x[[1]]))} # named column transformation
#' \code{list(cm = function(x) x * (inputs$rand <<- inputs$rand %||% runif(1, 0, 1))}
#' # mutating column transformation
#' \code{list(cnm = function(x) paste0(inputs$name <<- inputs$name %||% names(x), x[[1]]))}
#' # named mutating column transformation
#'
#' @param fn a 1-element list containing a function. The name will decide
#'    what happens with the function. (See examples)
parse_function <- function(fn) {
  stopifnot(is.list(fn) && length(fn) == 1)
  name <- names(fn)
  fn <- fn[[1]]

  if (inherits(fn, 'trigger')) {
    return(fn)
  }

  # By default, assume the user wishes to use a column transformation
  if (is.null(name) || name == '') {
    if (inherits(fn, 'transformation')) fn
    else column_transformation(fn)
  } else {
    first_char <- tolower(substr(name, 1, 1))
    if (first_char == 'c') {
      chars <- strsplit(tolower(name), '')[[1]]
      named <- 'n' %in% chars
      mutating <- 'm' %in% chars
      if (inherits(fn, 'transformation')) fn
      else column_transformation(fn, named = named, mutating = mutating)
    }
    else if (first_char %in% c('r', 'g')) {
      # TODO: How to make default args work?? Using dput? This sounds like
      # a rabbithole...
      inner <- paste0("(function(", paste0(names(formals(fn)), collapse = ','), ") ", 
        paste(deparse(body(fn)), collapse = "\n"), ")")
      if (first_char == 'r') inner <- paste0("apply(dataframe, 1, ", inner, ")")
      else inner <- paste0(inner, "(dataframe)")

      # This is a little tricky. For optimization, we wrap the function with
      # eval(substitute(...), envir = parent.frame()) changing the parent
      # scope directly.
      eval(parse(text = paste0("function(dataframe) eval(substitute(
        dataframe <- ", inner, "), envir = parent.frame())")))
    }
    else fn
  }
}

