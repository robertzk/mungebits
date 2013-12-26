munge <- function(dataframe, ...) {
  plane <- mungeplane(dataframe) 

  args <- list(...)
  if (lengths(args) == 1 && is.list(args[[1]])) {
    stopifnot(length(args[[1]]) > 0)
    stopifnot(is.list(args[[1]][[1]]))
    mungepieces <- args[[1]]
  } else mungepieces <- args

  mungepieces <- lapply(mungepieces, parse_mungepiece)  

  # order matters, do not parallelize!
  lapply(mungepieces, function(piece) piece$run(plane))

  # TODO: Store mungepieces somewhere

  plane
}

parse_mungepiece <- function(args) {
  if (is.function(args)) args <- list(args)
  stopifnot(is.list(args))  

  if (is.list(args[[1]]) && length(args) > 1 && is.list(args[[2]]) &&
      length(args[[1]]) > 0 && is.function(args[[1]][[1]]) &&
      length(args[[2]]) > 0 && is.function(args[[2]][[1]])) {
    # train and predict functions have separate arguments
    # list(list(predict_fn, ...), list(train_fn, ...))
    train_fn <- args[[1]][[1]]
    args[[1]][[1]] <- NULL
    train_args <- args[[1]]
    
    predict_fn <- args[[2]][[1]]
    args[[2]][[1]] <- NULL
    predict_args <- args[[2]]
  } else {
    stopifnot(length(args) > 0)
    if (is.list(args[[1]])) {
      # list(list(train_fn, predict_fn), ...)
      stopifnot(length(args[[1]]) == 2)
      train_fn <- args[[1]][[1]]
      predict_fn <- args[[1]][[2]]
    } else {
      # list(train_fn, ...)
      train_fn <- args[[1]]
      predict_fn <- train_fn
    }
                      
    args[[1]] <- NULL
    train_args <- args
    predict_args <- train_args
  }

  mungepiece(mungebit(train_fn, predict_fn),
             train_args,
             predict_args)
}

