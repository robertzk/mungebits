#' Pure column transformations.
#'
#' A mungebit which affects multiple columns identically and independently
#' can be abstracted into a column transformation. This function allows one
#' to specify what happens to an individual column, and the mungebit will be
#' the resulting column transformation applied to an arbitrary combination of
#' columns.
#'
#' @param transformation a function. The only argument should be the original
#'    column.
#' @param mutating a logical. Announces whether the transformation passed in
#'    attempts to maintain state between prediction and training runs using
#'    the "inputs" global. (see also \code{\link{mungebit}})
#' @return a function which takes a data.frame and a vector of columns and
#'    applies the transformation.
#' @seealso \code{\link{multi_column_transformation}}
#' @export
#' @examples
#' doubler <- column_transformation(function(x) 2*x)
#' # doubles the Sepal.Length column in the iris dataset
#' doubler(iris, c('Sepal.Length')) 
column_transformation <- function(transformation, mutating = FALSE) {
  force(transformation); force(mutating)
  invisible(function(dataframe, cols = colnames(dataframe), ...) {
    # The fastest way to do this. The alternatives are provided in the comment below
    assign("*tmp.fn.left.by.mungebits.library*",
           transformation, envir = parent.frame())
    mutating <- mutating # Copy from parent scope to this environment
    cols <- force(cols)
    if (is.logical(cols)) cols <- which(cols)
    colns <- if (is.character(cols)) cols else colnames(dataframe)[cols]

    invisible(eval(substitute({
      # Trick to make assignment incredibly fast. Could screw up the
      # data.frame if the function is interrupted, however.
      class(dataframe) <- 'list'
      on.exit(class(dataframe) <- 'data.frame')
      if (!mutating) {
        environment(`*tmp.fn.left.by.mungebits.library*`) <- environment()
        dataframe[cols] <- lapply(dataframe[cols],
          `*tmp.fn.left.by.mungebits.library*`, ...)
      } else {
        # We must now be surgically precise. The transformation function
        # is attempting to store values in "inputs" using inputs <<- ...
        # However, we would like to allow different values for different
        # columns (for example, a mean imputer needs to remember the mean
        # for each column separately). Therefore, we set an "inputs" variable
        # locally and re-assign the scope of the transformation so that the <<-
        # operator modifies this "inputs" rather than the one in a parent
        # scope (like in a mungebit). Afterwards, we exploit the fact that the
        # <<- operator never modifies local scope using
        #   inputs[[column_name]] <<- inputs
        dataframe[cols] <- lapply(colns, function(column_name) {
          inputs <- NULL
          environment(`*tmp.fn.left.by.mungebits.library*`) <- environment()
          column <- `*tmp.fn.left.by.mungebits.library*`(dataframe[[column_name]], ...)
          if (!is.null(inputs)) {
            # The <<- operator never modifies local scope so that left "inputs"
            # refers to the parent.frame() whereas the right "inputs" refers
            # to the one in local scope. The end result is that if the column
            # names were, e.g., c('age', 'height'), the "inputs" in the parent
            # scope would end up as a list with an $age and $height value.
            inputs[[column_name]] <<- inputs
          }
          column
        })
      }
      # Slightly slower is:
      # for(i in cols) dataframe[[i]] <-
      #   `*tmp.fn.left.by.mungebits.library*`(dataframe[[i]], ...)
      dataframe[cols[
        vapply(cols,
               function(x) is.null(dataframe[[x]]),
               logical(1))
      ]] <- NULL

      class(dataframe) <- 'data.frame'
      NULL
    }), envir = parent.frame()))
  })
}

# Possible column transformations:
# 1: function(dataframe, col) { dataframe[col] <- 2*dataframe[col]; dataframe }
# 2: function(dataframe, col) { eval(substitute(dataframe[col] <- 2*dataframe[col]), envir = parent.frame()) }
# 3: function(dataframe, col) { class(dataframe) <- 'list'; for(colname in col) dataframe[[colname]] <- 2*dataframe[[colname]]; class(dataframe) <- 'data.frame'; dataframe }
# 4: function(dataframe, col) { eval(substitute({ class(dataframe) <- 'list'; for(colname in col) dataframe[[col]] <- 2*dataframe[[col]]; class(dataframe) <- 'data.frame'; dataframe }), envir = parent.frame()) }
# 5: The method above for dynamic lambdas
# An extra rm function after the assign increases runtime by 75% with frequent application.
# The fifth option is the fastest.

