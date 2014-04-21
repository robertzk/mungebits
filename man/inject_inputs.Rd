\name{inject_inputs}
\alias{inject_inputs}
\title{Inject a parent environment that has only an inputs key so that
things like \code{inputs <<- 'foo'} work.}
\usage{
inject_inputs(fn)
}
\arguments{
  \item{fn}{function. The function on which to inject.}
}
\description{
Inject a parent environment that has only an inputs key so
that things like \code{inputs <<- 'foo'} work.
}

