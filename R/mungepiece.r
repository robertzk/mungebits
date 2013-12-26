#' Mungepieces are mungebits that have had their arguments cached
#' (with the exception of the first, the data.frame).
#'
#' A mungepiece allows one to fix the arguments to a mungebit
#' (such as input and output columns), without yet necessarily
#' specifying the data.frame on which the mungebit operates. In
#' this way, the mungepiece can "pick up" what operations to perform
#' on a to-be-trained dataset, and later what operations to perform
#' on a to-be-predicted dataset, without yet knowing what the data
#' itself is.
#'
#' @docType class
#' @name mungepiece
#' @param bit a mungebit. The mungepiece produced will be a wrapper around
#'    this mungebit that caches the arguments to subsequent calls.
#' @param train_args a list. These will be passed when a mungebit is run
#'    the first time using \code{mungebit$run} or \code{mungebit$train}.
#' @param predict_args a list. These will be passed when a mungebit is run
#'    subsequent times using \code{mungebit$run} or \code{mungebit$predict}.
#' @examples
#' mb <- mungebit(column_transformation(function(x) x * 2))
#' mp <- mungepiece(mb, list('Sepal.Length'))
#' mpl <- mungeplane(iris)
#' do.call(attr(mp, 'mungebit'), append(list(mpl), attr(mpl, 'train_args')))
#' 

setClassUnion('listOrNull', c('list', 'NULL'))
mungepiece <- setRefClass('mungepiece',
  fields = list(bit = 'mungebit',
                train_args = 'list',
                predict_args = 'listOrNull'),
  methods = list(
    initialize = function(.bit, .train_args, .predict_args = NULL) {
      stopifnot(is.mungebit(.bit), is.list(.train_args),
                is.null(.predict_args) || is.list(.predict_args))
      bit <<- .bit
      train_args <<- .train_args
      predict_args <<- .predict_args
    },

    run = function(.mungeplane) {
      method <- if (bit$trained) bit$predict else bit$train
      rest_args <-
        if (bit$trained) train_args
        else predict_args %||% train_args
      do.call(method, append(list(.mungeplane), rest_args))
    }
  )
)
    
# S3 definition... uglier I think
#mungepiece <- function(bit, train_args, predict_args = train_args) {
#  mp <- list()
#  class(mp) <- 'mungepiece'
#
#  stopifnot(is.mungebit(bit), is.list(train_args), is.list(predict_args))
#  attr(mp, 'mungebit') <- bit
#  attr(mp, 'train_args') <- train_args
#  attr(mp, 'predict_args') <- predict_args
#  mp
#}
