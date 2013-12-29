#' Mungeplane are reference classes wrappers around a dataframe.
#'
#' By exploiting pass-by-reference semantics of reference classes,
#' mungeplanes allow mungebits to manipulate individual columns and rows of
#' dataframes without forcing copy-on-write triggers that would copy the
#' entire dataframe.
#'
#' @param dataframe a \code{data.frame}. This can also be a list to help with
#'    optimization procedures in mungebit operations that temporarily remove
#'    the \code{data.frame} class to use built-in C replacement functions.
#' @return mungeplane an environment with a "data" object of class "mungeplane"
#' @examples
#' mp <- mungeplane(iris)
#' (function(plane) plane$data[[1]] <- 2 * plane$data[[1]])(mp)
#' stopifnot(all.equal(mp$data[[1]], 2 * iris[[1]]))
mungeplane <- function(dataframe) {
  plane <- new.env(parent = parent.frame()) 
  plane$data <- dataframe
  class(plane) <- 'mungeplane'
  plane
}

is.mungeplane <- function(x) inherits(x, 'mungeplane')

#setClassUnion('listOrDataFrame', c('list', 'data.frame'))
#mungeplane <- setRefClass('mungeplane',
#  fields = list(data = 'listOrDataFrame'),
#  methods = list(
#    initialize = function(dataframe) {
#      data <<- dataframe
#    }
#  )
#)


