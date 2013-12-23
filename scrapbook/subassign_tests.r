f6d <- function(fn) {
  function(df, col) {
    #if (!(exists("*tmp.fn*", envir = parent.frame()) &&
    #    as.character(body(get("*tmp.fn*", envir = parent.frame()))) == as.character(body(fn))))
    # TODO: Maybe keep track of environments and remove later?
    #;assign("*tmp.fn*", fn, envir = parent.frame())
    eval(substitute({
      class(df) <- 'list'
      for(colname in col) df[[colname]] <- 2*df[[colname]] #get("*tmp.fn*")(df[[col]])
      class(df) <- 'data.frame'
      df
    }),
    envir = parent.frame())
    # rm("*tmp.fn*", envir = parent.frame())
  }
}

#eval(substitute(body(function(x)2*x), list(x=5)))
#substitute({df[[col]] <- fn_val}, list(fn_val = eval(substitute(body(function(x)2*x), list(x=quote(df[[col]]))))))
# fastest: function(df, col) { class(df) <- 'list'; for(colname in col) df[[colname]] <- 2*df[[colname]]; class(df) <- 'data.frame'; df }

