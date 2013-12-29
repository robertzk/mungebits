\name{munge}
\alias{munge}
\title{General-purpose data munging}
\usage{
munge(dataframe, ...)
}
\arguments{
  \item{dataframe}{a data set to operate on.}

  \item{...}{usually a list specifying the necessary
  operations (see examples).}
}
\value{
data.frame
}
\description{
One can use \code{munge} to take a data.frame, apply a
given set of transformations, and persistently store the
operations on the data.frame, ready to run on a future
dataframe.
}
\examples{
iris2 <- munge(iris,
  list(column_transformation(function(x) 2 * x), 'Sepal.Length'))
stopifnot(iris2[['Sepal.Length']] == iris[['Sepal.Length']] * 2)

iris2 <- munge(iris,
   # train function & predict function
   list(c(column_transformation(function(x) 2 * x),
        column_transformation(function(x) 3 * x)),
   # arguments to pass to transformation, i.e. column names in this case
   'Sepal.Length'))
stopifnot(iris2[['Sepal.Length']] == iris[['Sepal.Length']] * 2)
iris3 <- munge(iris, attr(iris2, 'mungepieces'))
# used transformations ("mungepieces") stored on iris2 and apply to iris3.
# They will remember that they've been trained already and run the
# prediction routine instead of the training routine. Note the above is
# also equivalent to the shortcut: munge(iris, iris2)
stopifnot(iris3[['Sepal.Length']] == iris[['Sepal.Length']] * 3)
}

