\name{parse_mungepiece}
\alias{parse_mungepiece}
\title{Translate a list of arguments passed to a mungebit into a mungepiece}
\usage{
parse_mungepiece(args)
}
\arguments{
  \item{args}{a list. This can be of the formats
  \code{list(train_fn, ...)}, \code{list(list(train_fn,
  predict_fn), ...)}, and \code{list(list(train_fn, ...),
  list(predict_fn, ...))}. In the first example, the train
  and predict function are assumed to be identical. In the
  first two examples, the arguments to these functions are
  assumed to be identical (for example, if the same kind of
  filter needs to be applied to a data set that is about to
  be trained as to one about to be predicted). Finally, the
  last example is the most flexible and allows different
  parameters for the training and prediction function,
  respectively. The given training and prediction functions
  are used to construct a \code{mungebit}, and the
  resulting \code{mungebit} and the remaining arguments are
  stored in a \code{mungepiece}.}
}
\value{
the parsed mungepiece
}
\description{
For example, one can pass a training function, a prediction
function, and additional arguments, and the constructed
mungepiece will hold these arguments, ready to call the
attached mungebit on a given dataframe.
}
\examples{
\dontrun{
doubler <- column_transformation(function(x) x * 2)
mp <- parse_mungepiece(list(doubler, 'Sepal.Length'))
iris2 <- mungeplane(iris)
mp$run(iris2)
stopifnot(all.equal(iris2$data[[1]], 2 * iris[[1]]))

# TODO: Way more examples, unit tests

}
}

