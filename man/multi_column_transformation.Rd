\name{multi_column_transformation}
\alias{multi_column_transformation}
\title{Old-to-new column mapping transformation}
\usage{
multi_column_transformation(transformation)
}
\arguments{
  \item{transformation}{a function. The only argument
  should be the original column.}
}
\value{
a function which takes a data.frame and a vector of columns
and applies the transformation.
}
\description{
A mungebit which does not affect any existing columns, but
can transform other columns into new columns, or affect
multiple columns simultaneously as functions of each other.
}
\examples{
doubler <- multi_column_transformation(function(x) 2*x)
# doubles the Sepal.Length column in the iris dataset
doubler(iris, c('Sepal.Length'))
}

