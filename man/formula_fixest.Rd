% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formulaMakers.R
\name{formula_fixest}
\alias{formula_fixest}
\title{Stitches together formula for use in fixest::feols}
\usage{
formula_fixest(y, X, W = NULL, D = NULL, Z = NULL)
}
\arguments{
\item{y}{The dependent variable}

\item{X}{vector of controls}

\item{W}{treatment variable}

\item{D}{vector of factor variables to be partialed out}

\item{Z}{vector of instruments}
}
\description{
Stitches together formula for use in fixest::feols
}
\examples{
formula_fixest(y = 'mpg', X = c('hp', 'drat'), D = c('wt', 'vs'))
formula_fixest(y = 'mpg', X = c('hp', 'drat'), W = 'gear', Z = c('cyl', 'carb'), D = c('wt', 'vs'))

}
\keyword{linear}
\keyword{model}
\keyword{name}
\keyword{regression}
\keyword{variable}
