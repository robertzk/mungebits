#' General-purpose data munging
#'
#' One can use \code{munge} to take a data.frame, apply a given set
#' of transformations, and persistently store the operations on
#' the data.frame, ready to run on a future dataframe.
#'
#' @param dataframe a data set to operate on.
#' @param ... usually a list specifying the necessary operations (see
#'    examples).
#' @return data.frame
#' @export
#' @examples
#' iris2 <- munge(iris,
#'   list(column_transformation(function(x) 2 * x), 'Sepal.Length'))
#' stopifnot(iris2[['Sepal.Length']] == iris[['Sepal.Length']] * 2)
#'
#' iris2 <- munge(iris,
#'    # train function & predict function
#'    list(c(column_transformation(function(x) 2 * x),
#'         column_transformation(function(x) 3 * x)),
#'    # arguments to pass to transformation, i.e. column names in this case
#'    'Sepal.Length'))
#' stopifnot(iris2[['Sepal.Length']] == iris[['Sepal.Length']] * 2)
#' iris3 <- munge(iris, attr(iris2, 'mungepieces'))
#' # used transformations ("mungepieces") stored on iris2 and apply to iris3.
#' # They will remember that they've been trained already and run the
#' # prediction routine instead of the training routine. Note the above is
#' # also equivalent to the shortcut: munge(iris, iris2)
#' stopifnot(iris3[['Sepal.Length']] == iris[['Sepal.Length']] * 3)
munge <- function(dataframe, ...) {
  mungepieces <- list(...)
  if (length(mungepieces) == 0) return(dataframe)

  old_mungepieces <-
    attr(if (is.mungeplane(dataframe)) dataframe$data else dataframe,
         'mungepieces', exact = TRUE)
  plane <- if (is.mungeplane(dataframe)) dataframe else mungeplane(dataframe)

  if (is.data.frame(mungepieces[[1]]))
    mungepieces[[1]] <- attr(mungepieces[[1]], 'mungepieces')

  # If mungepieces[[1]] is of the form
  # list(list|mungepiece, list|mungepiece, ...)
  # just put it into mungepieces. This is so munge can be called as either
  # munge(dataframe, list(...)) or munge(dataframe, ...)
  if (length(mungepieces) == 1 && is.list(mungepieces[[1]]) &&
      all(unlist(lapply(mungepieces[[1]],
                        function(x) is.mungepiece(x) || is.list(x))))) {
      mungepieces <- mungepieces[[1]]
  }

  mungepieces <- lapply(mungepieces, parse_mungepiece)

  # order matters, do not parallelize!
  lapply(mungepieces, function(piece) piece$run(plane))

  # For now, store the mungepieces on the dataframe
  if (length(mungepieces) > 0)
    attr(plane$data, 'mungepieces') <- append(old_mungepieces, mungepieces)
  plane$data
}

