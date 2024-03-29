% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotUtils.R
\name{regplot_dens}
\alias{regplot_dens}
\title{Scatterplot with regression line and densites by grouping variable. Use this to evaluate overlap.}
\usage{
regplot_dens(df, xvar, yvar, zvar, title = "")
}
\arguments{
\item{df}{dataframe}

\item{xvar}{x variable}

\item{yvar}{y variable}

\item{zvar}{z variable (coerced to factor)}

\item{title}{plot title}
}
\description{
Scatterplot with regression line and densites by grouping variable. Use this to evaluate overlap.
}
\examples{
regplot_dens(mtcars, wt, mpg, am)
}
\keyword{Scatterplot}
\keyword{seaborn}
