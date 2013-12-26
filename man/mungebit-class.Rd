\docType{class}
\name{mungebit-class}
\alias{mungebit-class}
\title{Constructor for mungebit class.}
\arguments{
  \item{train_fn}{a function. This specifies the behavior
  to perform on the dataset when preparing for model
  training.}

  \item{predict_fn}{a function. This specifies the behavior
  to perform on the dataset when preparing for model
  prediction.}
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
mp <- mungeplane(iris)
mb <- mungebit(column_transformation(function(col, scale) {
  inputs$scale <<- scale
  col * inputs$scale
}))
mb$run(mp, 'Sepal.Length', 2)
# mp$data now contains a copy of iris w/ the Sepal.Length column doubled
head(mp$data[[1]] / iris[[1]])
[1] 2 2 2 2 2 2
}

