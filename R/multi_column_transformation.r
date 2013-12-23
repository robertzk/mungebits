#' Old-to-new column mapping transformation
#'
#' A mungebit which does not affect any existing columns, but can transform
#' other columns into new columns, or affect multiple columns simultaneously
#' as functions of each other.
#'
#' @param transformation a function. The only argument should be the original
#'    column.
#' @return a function which takes a data.frame and a vector of columns and
#'    applies the transformation.
#' @export
#' @examples
#' doubler <- multi_column_transformation(function(x) 2*x)
#' # doubles the Sepal.Length column in the iris dataset
#' doubler(iris, c('Sepal.Length')) 
multi_column_transformation <- function(transformation) {
  function(dataframe, input_cols, output_cols = input_cols, ...) {
    # The fastest way to do this. The alternative is to use pass by value
    # or replace list subset assignment below with mapply.
    assign("*tmp.fn.left.by.mungebits.library*",
           transformation, envir = parent.frame())
    #input_cols <- standard_column_format(input_cols, dataframe)
    #input_cols <-
    #  if (is.character(input_cols)) force(input_cols) 
    #  else colnames(dataframe)[input_cols]
    #input_cols <- force(input_cols)
    input_cols <- force(input_cols)
    if (is.logical(input_cols)) input_cols <- which(input_cols)
    output_cols <- force(output_cols)
    if (is.logical(output_cols)) output_cols <- which(output_cols)
    #stopifnot(is.character(output_cols) || is.numeric(output_cols))

    dataframe <- substitute(dataframe)
    invisible(eval(substitute({
      # Trick to make assignment incredibly fast. Could screw up the
      # data.frame if the function is interrupted, however.
      class(dataframe) <- 'list'
      on.exit(class(dataframe) <- 'data.frame')

      if (length(output_cols) == 1)
        dataframe[[output_cols]] <-
          do.call(`*tmp.fn.left.by.mungebits.library*`,
                  append(unname(dataframe[input_cols]), list(...)))
      else
        dataframe[output_cols] <-
          do.call(`*tmp.fn.left.by.mungebits.library*`,
                  append(unname(dataframe[input_cols]), list(...)))

      # Remove any columns that were set to NULL explicitly
      #dataframe[seq_along(dataframe)[
      #  vapply(seq_along(dataframe),
      #         function(x) is.null(dataframe[[x]]),
      #         logical(1))
      #]] <- NULL
      #for(i in seq_along(dataframe)) {
      #  if (is.null(dataframe[[i]])) dataframe[[i]] <- NULL
      #}
      #dataframe[output_cols[
      #  vapply(intersect(output_cols, names(dataframe)),
      #         function(x) is.null(dataframe[[x]]),
      #         logical(1))
      #]] <- NULL


      class(dataframe) <- 'data.frame'
      dataframe
    }), envir = parent.frame()))
  }
}


