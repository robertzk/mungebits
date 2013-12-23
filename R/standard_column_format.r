#' Converts a logical / numeric / character vector into a character vector
#' of column names for a dataframe.
#'
#' @param cols a vector.
#'    column.
#' @param dataframe a reference dataframe. Necessary for computing the
#'    column names if a numeric or logical vector is specified for \code{cols}.
#' @export
#' @examples
#' standard_column_format(c(1,5), iris)  # c('Sepal.Length', 'Species')
#' standard_column_format(c(T,F,F,F,T), iris)  # c('Sepal.Length', 'Species')
#' standard_column_format('Sepal.Length', iris)  # 'Sepal.Length'

standard_column_format <- function(cols, dataframe) {
  if (missing(dataframe)) stop('No dataframe provided')
  eval(substitute(
   if (is.character(cols)) force(cols) 
   else colnames(dataframe)[cols]
  ), envir = parent.frame())
}
