#' Old-to-new column mapping transformation
#'
#' A mungebit which affects multiple columns identically and independently
#' can be abstracted into a column transformation. This function allows one
#' to specify what happens to an individual column, and the mungebit will be
#' the resulting column transformation applied to an arbitrary combination of
#' columns.
#'
#' @param transformation a function. The only argument should be the original
#'    column.
#' @return a function which takes a data.frame and a vector of columns and
#'    applies the transformation.
#' @export
#' @examples
#' doubler <- column_transformation(function(x) 2*x)
#' # doubles the Sepal.Length column in the iris dataset
#' doubler(iris, c('Sepal.Length')) 
create_column_transformation <- function(transformation) {
  function(df, cols, ...) {
    # The fastest way to do this. The alternatives are provided in the comment below
    assign("*tmp.fn.left.by.mungebits.library*",
           transformation, envir = parent.frame())
    cols <- 
      if (is.character(cols)) force(cols)
      else colnames(df)[cols]

    invisible(eval(substitute({
      # Trick to make assignment incredibly fast. Could screw up the
      # data.frame if the function is interrupted, however.
      class(df) <- 'list'
      on.exit(class(df) <- 'data.frame')
      for(colname in cols) {
        df[[colname]] <- `*tmp.fn.left.by.mungebits.library*`(df[[colname]], ...)
      }
      class(df) <- 'data.frame'
      df
    }), envir = parent.frame()))
  }
}

# Possible column transformations:
# 1: function(df, col) { df[col] <- 2*df[col]; df }
# 2: function(df, col) { eval(substitute(df[col] <- 2*df[col]), envir = parent.frame()) }
# 3: function(df, col) { class(df) <- 'list'; for(colname in col) df[[colname]] <- 2*df[[colname]]; class(df) <- 'data.frame'; df }
# 4: function(df, col) { eval(substitute({ class(df) <- 'list'; for(colname in col) df[[col]] <- 2*df[[col]]; class(df) <- 'data.frame'; df }), envir = parent.frame()) }
# 5: The method above for dynamic lambdas
# An extra rm function after the assign increases runtime by 75% with frequent application.
# The fifth option is the fastest.


