#' A convenience method to make printing of transformations more obvious.
#'
#' @param x a \code{transformation}. Either a \code{column_transformation}
#'    or \code{multi_column_transformation}.
#' @param ... additional parameters to print.
#' @export
#' @seealso \code{\link{column_transformation}},
#'      \code{\link{multi_column_transformation}}
print.transformation <- function(x, ...) {
  cat("This is a mungebits transformation as defined in the",
      "syberiaMungebits package.\n")
  labels <- if (identical(attr(x, 'named'), TRUE)) 'named' else c()
  labels <- append(labels, if (identical(attr(x, 'mutating'), TRUE)) 'mutating' else c())
  if (length(labels) > 0) {
    labels <- paste0(labels, collapse = ' and ')
    cat("It is a", labels, "transformation.",
        "For help, see ?column_transformation.\n")
  }
  fn <- environment(x)$transformation
  base::print(fn, ...)
}
