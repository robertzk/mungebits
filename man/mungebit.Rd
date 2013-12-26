\name{mungebit}
\alias{mungebit}
\title{Constructor for mungebit class.}
\usage{
mungebit(train_function, predict_function = train_function,
  modifies.column = TRUE, modifies.row = FALSE,
  modifies.column.dimension = FALSE, modifies.row.dimension = FALSE)
}
\arguments{
  \item{mungebit_function}{a function. The first argument
  should always be the data.frame. The other arguments are
  optional parameters that can be curried (see examples).}

  \item{modifies.column}{a logical. Whether or not the
  mungebit tends to modify entire columns.}

  \item{modifies.row}{a logical. Whether or not the
  mungebit tends to modify entire rows.}

  \item{modifies.column.dimension}{a logical. Whether or
  not the mungebit tends to modify column dimension.}

  \item{modifies.row.dimension}{a logical. Whether or not
  the mungebit tends to modify row dimension.}
}
\value{
mungebit
}
\description{
Mungebits are atomic data transformations of a data.frame
that, loosely speaking, aim to modify "one thing" about a
variable or collection of variables. This is pretty loosely
defined, but examples include dropping variables, mapping
values, discretization, etc.
}
\examples{
scalebit <- mungebit(function(df, cols, factor = 1) {
   df[cols] <- vapply(cols, function(x) factor * x, numeric(nrow(df)))
   df
 })
 scalebit(iris, c('Sepal.Length', 'Sepal.Width'), 2)
}

