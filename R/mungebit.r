#' Constructor for mungebit class.
#'
#' Mungebits are atomic data transformations of a data.frame that,
#' loosely speaking, aim to modify "one thing" about a variable or
#' collection of variables. This is pretty loosely defined, but examples
#' include dropping variables, mapping values, discretization, etc.
#'
#' @param mungebit_function a function. The first argument should always be 
#'    the data.frame. The other arguments are optional parameters that can
#'    be curried (see examples).
#' @param modifies.column a logical. Whether or not the mungebit tends to 
#'    modify entire columns.
#' @param modifies.row a logical. Whether or not the mungebit tends to 
#'    modify entire rows.
#' @param modifies.column.dimension a logical. Whether or not the mungebit tends to 
#'    modify column dimension.
#' @param modifies.row.dimension a logical. Whether or not the mungebit tends to 
#'    modify row dimension.
#' @return mungebit
#' @export
#' @examples
#'  scalebit <- mungebit(function(df, cols, factor = 1) {
#'    df[cols] <- vapply(cols, function(x) factor * x, numeric(nrow(df)))
#'    df
#'  })
#'  scalebit(iris, c('Sepal.Length', 'Sepal.Width'), 2)
mungebit <- function(mungebit_function,
                     modifies.column = TRUE,
                     modifies.row = FALSE,
                     modifies.column.dimension = FALSE,
                     modifies.row.dimension = FALSE) {
  function(...) {
    arguments <- as.list(...)
    do.call(mungebit_function, arguments)
  }
}
