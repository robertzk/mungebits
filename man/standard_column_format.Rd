\name{standard_column_format}
\alias{standard_column_format}
\title{Converts a logical / numeric / character vector into a character vector
of column names for a dataframe.}
\usage{
standard_column_format(cols, dataframe)
}
\arguments{
  \item{cols}{a vector.  column.}

  \item{dataframe}{a reference dataframe. Necessary for
  computing the column names if a numeric or logical vector
  is specified for \code{cols}.}
}
\description{
Converts a logical / numeric / character vector into a
character vector of column names for a dataframe.
}
\examples{
standard_column_format(c(1,5), iris)  # c('Sepal.Length', 'Species')
standard_column_format(c(T,F,F,F,T), iris)  # c('Sepal.Length', 'Species')
standard_column_format('Sepal.Length', iris)  # 'Sepal.Length'
}

