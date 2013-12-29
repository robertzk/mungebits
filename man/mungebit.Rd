\docType{class}
\name{mungebit}
\alias{mungebit}
\title{Constructor for mungebit class.}
\arguments{
  \item{train_fn}{a function. This specifies the behavior
  to perform on the dataset when preparing for model
  training.}

  \item{predict_fn}{a function. This specifies the behavior
  to perform on the dataset when preparing for model
  prediction.}

  \item{inputs}{a list. Used for maintaining meta-data
  between training and prediction runs.}

  \item{trained}{a logical. Used for determining whether or
  not the mungebit has been run on a dataset already.}
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
mb <- moongebit(column_transformation(function(col, scale = NULL) {
  if ('scale' \%in\% names(inputs))
    cat("Column scaled by ", inputs$scale, "\\n")
  else inputs$scale <<- scale
  col * inputs$scale
}))
mb$run(mp, 'Sepal.Length', 2)
# mp$data now contains a copy of iris w/ the Sepal.Length column doubled
head(mp$data[[1]] / iris[[1]])
# > [1] 2 2 2 2 2 2
mb$run(mp, 'Sepal.Length')
# > Column scaled by 2
head(mp$data[[1]] / iris[[1]])
# > [1] 4 4 4 4 4 4
}
\seealso{
\code{\link{mungepiece}}
}

