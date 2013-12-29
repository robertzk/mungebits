\docType{class}
\name{mungepiece}
\alias{mungepiece}
\title{Mungepieces are mungebits that have had their arguments cached
(with the exception of the first, the data.frame).}
\arguments{
  \item{bit}{a mungebit. The mungepiece produced will be a
  wrapper around this mungebit that caches the arguments to
  subsequent calls.}

  \item{train_args}{a list. These will be passed when a
  mungebit is run the first time using \code{mungebit$run}
  or \code{mungebit$train}.}

  \item{predict_args}{a list. These will be passed when a
  mungebit is run subsequent times using
  \code{mungebit$run} or \code{mungebit$predict}.}
}
\description{
A mungepiece allows one to fix the arguments to a mungebit
(such as input and output columns), without yet necessarily
specifying the data.frame on which the mungebit operates.
In this way, the mungepiece can "pick up" what operations
to perform on a to-be-trained dataset, and later what
operations to perform on a to-be-predicted dataset, without
yet knowing what the data itself is.
}
\examples{
doubler <- mungebit(column_transformation(function(x) x * 2))
cols <- c('Sepal.Length', 'Petal.Length')
mp <- mungepiece(doubler, list(cols))
iris2 <- mungeplane(iris)
mp$run(iris2)
stopifnot(iris2$data[cols] == 2 * iris[cols])
}

