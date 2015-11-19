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
#'    the "inputs" global. 
#' @param named a logical. Whether or not the transformation should be passed
#'    an atomic vector, or a list of length 1 whose single element is given
#'    the same name as the column currently being processed.
#' @return a function which takes a data.frame and a vector of columns and
#'    applies the transformation.
#' @seealso \code{\link{multi_column_transformation}}
#' @export
#' @examples
#' doubler <- column_transformation(function(x) 2*x)
#' # doubles the Sepal.Length column in the iris dataset
#' doubler(iris, c('Sepal.Length')) 
column_transformation <- function(transformation, mutating = FALSE, named = FALSE) {
  force(transformation); force(mutating); force(named)
  invisible(structure(function(dataframe, input_cols = colnames(dataframe), ...) {
    # The fastest way to do this. The alternatives are provided in the comment below
    assign("*tmp.fn.left.by.mungebits.library*",
           transformation, envir = parent.frame())
    if (isdebugged(transformation)) eval.parent(quote(
      debug(`*tmp.fn.left.by.mungebits.library*`)))
    mutating <- mutating # Copy from parent scope to this environment
    named <- named       # Copy from parent scope to this environment
    # if (is.logical(cols)) cols <- which(cols)

    # During prediction, always use same column names as during training
    # TODO: Clean this up
    force(input_cols)
    if (eval.parent(quote(!(exists('inputs') && '*colnames*' %in% names(inputs))))) {
      # Only calculate standard_cols when necessary.
      standard_cols <- intersect(colnames(dataframe),
                                 standard_column_format(input_cols, dataframe))
    }

    invisible(eval(substitute({
      cols <-
        if (exists('inputs') && !'*colnames*' %in% names(inputs))
          inputs$`*colnames*` <<- standard_cols
        else if (exists('inputs') && '*colnames*' %in% names(inputs))
          inputs$`*colnames*`
        else standard_cols

      is_training <- !isTRUE(trained)
      # Trick to make assignment incredibly fast. Could screw up the
      # data.frame if the function is interrupted, however.
      class(dataframe) <- 'list'
      on.exit(class(dataframe) <- 'data.frame')
      if (!mutating) {
        debug_flag <- isdebugged(`*tmp.fn.left.by.mungebits.library*`)
        prev_environment <- environment(`*tmp.fn.left.by.mungebits.library*`)
        environment(`*tmp.fn.left.by.mungebits.library*`) <- environment()
        if (debug_flag) debug(`*tmp.fn.left.by.mungebits.library*`)
        if (named)
          dataframe[cols] <- lapply(cols, function(colname) {
            `*tmp.fn.left.by.mungebits.library*`(dataframe[colname], ...)
          })
        else 
          dataframe[cols] <- lapply(dataframe[cols], 
            `*tmp.fn.left.by.mungebits.library*`, ...)

        environment(`*tmp.fn.left.by.mungebits.library*`) <- prev_environment
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
        input_vec <- NULL
        input_vec[cols] <- list(NULL)
        has_in <- cols %in% names(inputs)
	input_vec[has_in] <- inputs[cols[has_in]]
        
        dataframe[cols] <- lapply(1:length(cols), function(i, mycols, myinput_vec) {
          # If this is a prediction run and inputs already exists for this
          # column, use that, otherwise use NULL
          column_name <- mycols[i]				  
          inputs <- myinput_vec[[i]]
          trained <- exists('trained') # TODO: (RK) Be more careful with this
          debug_flag <- isdebugged(`*tmp.fn.left.by.mungebits.library*`)
          # Ensure transformation has access to "inputs"
          prev_environment <- environment(`*tmp.fn.left.by.mungebits.library*`)
          environment(`*tmp.fn.left.by.mungebits.library*`) <- environment()

          if (debug_flag) debug(`*tmp.fn.left.by.mungebits.library*`)
          column <- `*tmp.fn.left.by.mungebits.library*`(
            if (named) dataframe[column_name] else dataframe[[column_name]], ...)
          if (isTRUE(is_training) & !is.null(inputs)) {
            # The <<- operator never modifies local scope so that left "inputs"
            # refers to the parent.frame() whereas the right "inputs" refers
            # to the one in local scope. The end result is that if the column
            # names were, e.g., c('age', 'height'), the "inputs" in the parent
            # scope would end up as a list with an $age and $height value.
            inputs[[column_name]] <<- inputs
          }

          environment(`*tmp.fn.left.by.mungebits.library*`) <- prev_environment
          column
        }, cols, input_vec)
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
  }, class = c('transformation', 'function'), named = named, mutating = mutating))
}

# Possible column transformations:
# 1: function(dataframe, col) { dataframe[col] <- 2*dataframe[col]; dataframe }
# 2: function(dataframe, col) { eval(substitute(dataframe[col] <- 2*dataframe[col]), envir = parent.frame()) }
# 3: function(dataframe, col) { class(dataframe) <- 'list'; for(colname in col) dataframe[[colname]] <- 2*dataframe[[colname]]; class(dataframe) <- 'data.frame'; dataframe }
# 4: function(dataframe, col) { eval(substitute({ class(dataframe) <- 'list'; for(colname in col) dataframe[[col]] <- 2*dataframe[[col]]; class(dataframe) <- 'data.frame'; dataframe }), envir = parent.frame()) }
# 5: The method above for dynamic lambdas
# An extra rm function after the assign increases runtime by 75% with frequent application.
# The fifth option is the fastest.


##' @export
#CT <- column_transformation

